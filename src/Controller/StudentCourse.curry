module Controller.StudentCourse ( mainStudentCourseController ) where

import System.Spicey
import HTML.Base
import Time
import MDB
import View.StudentCourse
import Maybe
import System.SessionInfo
import System.Authorization
import System.AuthorizedActions
import System.Helpers
import Config.UserProcesses
import View.MDBEntitiesToHtml
import Database.CDBI.Connection

import SpecialQueries ( getModuleConflictList )

--- Choose the controller for a StudentCourse entity according to the URL parameter.
mainStudentCourseController :: Controller
mainStudentCourseController =
  do args <- getControllerParams
     case args of
       [] -> listStudentCourseController
       ["conflicts"] -> showConflictsController
       ["list"] -> listStudentCourseController
       ["new"] -> newStudentCourseController
       ["show",s] ->
         applyControllerOn (readStudentCourseKey s)
          getStudentCourse showStudentCourseController
       ["edit",s] ->
         applyControllerOn (readStudentCourseKey s)
          getStudentCourse editStudentCourseController
       ["delete",s] ->
         applyControllerOn (readStudentCourseKey s)
          getStudentCourse deleteStudentCourseController
       _ -> displayError "Illegal URL"

--- Shows a form to create a new StudentCourse entity.
newStudentCourseController :: Controller
newStudentCourseController =
  checkAuthorization (studentCourseOperationAllowed NewEntity)
   $ (\sinfo ->
     do allModInsts <- runQ queryAllModInsts
        allStudents <- runQ queryAllStudents
        ctime <- getClockTime
        return
         (blankStudentCourseView sinfo ctime allModInsts allStudents
           (\entity ->
             transactionController (createStudentCourseT entity)
              (nextInProcessOr listStudentCourseController Nothing))
           listStudentCourseController))

--- Transaction to persist a new StudentCourse entity to the database.
createStudentCourseT :: (ClockTime,ModInst,Student) -> DBAction ()
createStudentCourseT (selectDate,modInst,student) =
  newStudentCourseWithStudentStudentCoursesKeyWithModInstStudentCourseInstancesKey
   selectDate
   (studentKey student)
   (modInstKey modInst)
   >+= (\_ -> return ())

--- Shows a form to edit the given StudentCourse entity.
editStudentCourseController :: StudentCourse -> Controller
editStudentCourseController studentCourseToEdit =
  checkAuthorization
   (studentCourseOperationAllowed (UpdateEntity studentCourseToEdit))
   $ (\sinfo ->
     do allModInsts <- runQ queryAllModInsts
        allStudents <- runQ queryAllStudents
        studentCourseInstancesModInst <- runJustT
                                          (getStudentCourseInstancesModInst
                                            studentCourseToEdit)
        studentCoursesStudent <- runJustT
                                  (getStudentCoursesStudent
                                    studentCourseToEdit)
        return
         (editStudentCourseView sinfo studentCourseToEdit
           studentCourseInstancesModInst
           studentCoursesStudent
           allModInsts
           allStudents
           (\entity ->
             transactionController (updateStudentCourseT entity)
              (nextInProcessOr listStudentCourseController Nothing))
           listStudentCourseController))

--- Transaction to persist modifications of a given StudentCourse entity
--- to the database.
updateStudentCourseT :: StudentCourse -> DBAction ()
updateStudentCourseT studentCourse = updateStudentCourse studentCourse

--- Deletes a given StudentCourse entity (after asking for confirmation)
--- and proceeds with the list controller.
deleteStudentCourseController :: StudentCourse -> Controller
deleteStudentCourseController studentCourse =
  checkAuthorization
   (studentCourseOperationAllowed (DeleteEntity studentCourse))
   $ (\_ ->
     confirmController
      [h3
        [htxt
          (concat
            ["Really delete entity \""
            ,studentCourseToShortView studentCourse
            ,"\"?"])]]
      (transactionController (deleteStudentCourseT studentCourse)
        listStudentCourseController)
      (showStudentCourseController studentCourse))

--- Transaction to delete a given StudentCourse entity.
deleteStudentCourseT :: StudentCourse -> DBAction ()
deleteStudentCourseT studentCourse = deleteStudentCourse studentCourse

--- Lists all StudentCourse entities with buttons to show, delete,
--- or edit an entity.
listStudentCourseController :: Controller
listStudentCourseController =
  checkAuthorization (studentCourseOperationAllowed ListEntities)
   $ (\sinfo ->
     do studentCourses <- runQ queryAllStudentCourses
        return (listStudentCourseView sinfo studentCourses))

--- Shows a StudentCourse entity.
showStudentCourseController :: StudentCourse -> Controller
showStudentCourseController studentCourse =
  checkAuthorization
   (studentCourseOperationAllowed (ShowEntity studentCourse))
   $ (\sinfo ->
     do studentCourseInstancesModInst <- runJustT
                                          (getStudentCourseInstancesModInst
                                            studentCourse)
        studentCoursesStudent <- runJustT
                                  (getStudentCoursesStudent studentCourse)
        return
         (showStudentCourseView sinfo studentCourse
           studentCourseInstancesModInst
           studentCoursesStudent))

--- Gets the associated ModInst entity for a given StudentCourse entity.
getStudentCourseInstancesModInst :: StudentCourse -> DBAction ModInst
getStudentCourseInstancesModInst sModInst =
  getModInst (studentCourseModInstStudentCourseInstancesKey sModInst)

--- Gets the associated Student entity for a given StudentCourse entity.
getStudentCoursesStudent :: StudentCourse -> DBAction Student
getStudentCoursesStudent sStudent =
  getStudent (studentCourseStudentStudentCoursesKey sStudent)

-----------------------------------------------------------------------

--- A controller to show the conflicts (student selections for two modules)
--- for a semester.
showConflictsController :: Controller
showConflictsController = do
  sinfo <- getUserSessionInfo
  if not (isAdminSession sinfo)
    then return [h3 [htxt $ "Operation not allowed!"]]
    else do
      csem <- getCurrentSemester >>= return . nextSemester
      return $ selectSemesterView sinfo semesterConflictController csem
                 "Select semester:" "Zeige Modulbelegungskonflikte"

--- A controller to show the conflicts (student selections for two modules)
--- for a given semester.
semesterConflictController :: (String,Int) -> Controller
semesterConflictController sem = do
  sinfo <- getUserSessionInfo
  conflicts <- getModuleConflictList sem
  return $ semesterConflictView sinfo sem conflicts

-----------------------------------------------------------------------
