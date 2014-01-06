---------------------------------------------------------------------------
--- This module supports the retrieval of data from the
--- Study Planner tool
--- (running at http://giscours.informatik.uni-kiel.de/studienplaner/)
---------------------------------------------------------------------------

module StudyPlanner where

import MDB
import URL(getContentsOfUrl)
import ReadNumeric(readInt)
import Helpers(showSemesterCode)

--- Retrieve the number of students of a module in a given semester
--- (represented by the semester code string).
getModuleStudents :: ModData -> String -> IO Int
getModuleStudents mdata sem = do
  nums <- getContentsOfUrl ("http://giscours.informatik.uni-kiel.de/studienplaner/student_count?offers="++sem++":"++modDataCode mdata)
  return (maybe (-2) fst (readInt nums))

--- Retrieve the number of students of a module for a given list
--- of module instances.
getModuleInstancesStudents :: ModData -> [ModInst] -> IO [Int]
getModuleInstancesStudents mdata minsts = do
  mapIO getModInstStudents minsts
 where
  getModInstStudents mi =
   getModuleStudents mdata (showSemesterCode (modInstTerm mi,modInstYear mi))
