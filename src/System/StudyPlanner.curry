---------------------------------------------------------------------------
--- This module supports the retrieval of data from the
--- Study Planner tool
--- (running at https://www-ps.informatik.uni-kiel.de/studienplaner/)
---------------------------------------------------------------------------

module System.StudyPlanner
  ( getModuleInstancesStudents, getTakenModuleInstances
  ) where

import Numeric    ( readInt )

import HTML.Base
import System.URL     ( getContentsOfUrl )

import Model.ConfigMDB      ( studyPlannerURL )
import Model.MDB
import Model.MDB.Queries    ( modInstSemester, queryStudentNumberOfModSemester )
import System.Helpers ( showSemesterCode )

--- Retrieve the number of students of a module in a given semester
--- (represented by the semester code string).
getModuleStudents :: ModData -> String -> IO Int
getModuleStudents mdata sem = do
  nums <- getContentsOfUrl (studyPlannerURL++"student_count?offers="++sem++":"++modDataCode mdata)
  return $ case readInt nums of
    [(n,"")] -> n
    _        -> -2

--- Retrieve the number of students of a module for a given list
--- of module instances.
getModuleInstancesStudents :: ModData -> [ModInst] -> IO [Int]
getModuleInstancesStudents mdata minsts = do
  mapM getModInstStudents minsts
 where
  getModInstStudents mi = do
    --spnum  <- getModuleStudents mdata (showSemesterCode (modInstSemester mi))
    mdbnum <- runQ $ queryStudentNumberOfModSemester mdata (modInstSemester mi)
    return $ mdbnum -- + spnum

--- Retrieve module instances that are already planned by some students
--- either in the study planner or in the module database.
getTakenModuleInstances :: [ModInst] -> IO [ModInst]
getTakenModuleInstances modinsts = do
  mapM getTakenModuleInstance modinsts >>= return . concat
 where
  getTakenModuleInstance mi = do
    mdata <- runJustT (getModData (modInstModDataModuleInstancesKey mi))
    --spnum <- getModuleStudents mdata (showSemesterCode (modInstSemester mi))
    mdbnum <- runQ $ queryStudentNumberOfModSemester mdata (modInstSemester mi)
    --return (if spnum > 0 || mdbnum > 0 then [mi] else [])
    return (if mdbnum > 0 then [mi] else [])
