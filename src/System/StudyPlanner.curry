---------------------------------------------------------------------------
--- This module supports the retrieval of data from the
--- Study Planner tool
--- (running at http://www-ps.informatik.uni-kiel.de/studienplaner/)
---------------------------------------------------------------------------

module System.StudyPlanner
  ( getModuleInstancesStudents, getTakenModuleInstances
  ) where

import ReadNumeric    ( readInt )

import HTML.Base
import System.URL     ( getContentsOfUrl )

import ConfigMDB      ( studyPlannerURL )
import MDB
import MDBExts        ( modInstSemester )
import SpecialQueries ( queryStudentNumberOfModSemester )
import System.Helpers ( showSemesterCode )
import System.Spicey  ( spEHref )

--- Retrieve the number of students of a module in a given semester
--- (represented by the semester code string).
getModuleStudents :: ModData -> String -> IO Int
getModuleStudents mdata sem = do
  nums <- getContentsOfUrl (studyPlannerURL++"student_count?offers="++sem++":"++modDataCode mdata)
  return (maybe (-2) fst (readInt nums))

--- Retrieve the number of students of a module for a given list
--- of module instances.
getModuleInstancesStudents :: ModData -> [ModInst] -> IO [Int]
getModuleInstancesStudents mdata minsts = do
  mapIO getModInstStudents minsts
 where
  getModInstStudents mi = do
    --spnum  <- getModuleStudents mdata (showSemesterCode (modInstSemester mi))
    mdbnum <- runQ $ queryStudentNumberOfModSemester mdata (modInstSemester mi)
    return $ mdbnum -- + spnum

--- Retrieve module instances that are already planned by some students
--- either in the study planner or in the module database.
getTakenModuleInstances :: [ModInst] -> IO [ModInst]
getTakenModuleInstances modinsts = do
  mapIO getTakenModuleInstance modinsts >>= return . concat
 where
  getTakenModuleInstance mi = do
    mdata <- runJustT (getModData (modInstModDataModuleInstancesKey mi))
    --spnum <- getModuleStudents mdata (showSemesterCode (modInstSemester mi))
    mdbnum <- runQ $ queryStudentNumberOfModSemester mdata (modInstSemester mi)
    --return (if spnum > 0 || mdbnum > 0 then [mi] else [])
    return (if mdbnum > 0 then [mi] else [])
