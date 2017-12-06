---------------------------------------------------------------------------
--- This module supports the retrieval of data from the
--- Study Planner tool
--- (running at http://www-ps.informatik.uni-kiel.de/studienplaner/)
---------------------------------------------------------------------------

module StudyPlanner where

import MDB
import URL(getContentsOfUrl)
import ReadNumeric(readInt)
import Helpers(showSemesterCode)
import KeyDatabase(runJustT)
import Spicey(spEHref)
import HTML.Base
import ConfigMDB(studyPlannerURL)

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
  getModInstStudents mi =
   getModuleStudents mdata (showSemesterCode (modInstSemester mi))


--- Retrieve module instances that are already planned by some students:
getTakenModuleInstances :: [ModInst] -> IO [ModInst]
getTakenModuleInstances modinsts = do
  mapIO getTakenModuleInstance modinsts >>= return . concat
 where
  getTakenModuleInstance mi = do
    mdata <- runJustT (getModData (modInstModDataModuleInstancesKey mi))
    num <- getModuleStudents mdata (showSemesterCode (modInstSemester mi))
    return (if num>0 then [mi] else [])
