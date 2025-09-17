---------------------------------------------------------------------------
--- This module supports the retrieval of data from the planning of
--- the students within the module database.
---------------------------------------------------------------------------

module System.StudyPlanner
  ( getModuleInstancesStudents, getTakenModuleInstances
  ) where

import Numeric    ( readInt )

import HTML.Base
import System.URL     ( getContentsOfUrl )

import Model.MDB
import Model.MDB.Queries    ( modInstSemester, queryStudentNumberOfModSemester )
import System.Helpers ( showSemesterCode )

--- Retrieve the number of students of a module for a given list
--- of module instances.
getModuleInstancesStudents :: ModData -> [ModInst] -> IO [Int]
getModuleInstancesStudents mdata minsts = do
  mapM getModInstStudents minsts
 where
  getModInstStudents mi =
    runQ $ queryStudentNumberOfModSemester mdata (modInstSemester mi)

--- Retrieve module instances that are already planned by some students
--- in the module database.
getTakenModuleInstances :: [ModInst] -> IO [ModInst]
getTakenModuleInstances modinsts = do
  mapM getTakenModuleInstance modinsts >>= return . concat
 where
  getTakenModuleInstance mi = do
    mdata <- runJustT (getModData (modInstModDataModuleInstancesKey mi))
    mdbnum <- runQ $ queryStudentNumberOfModSemester mdata (modInstSemester mi)
    return (if mdbnum > 0 then [mi] else [])
