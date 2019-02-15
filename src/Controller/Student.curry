
module Controller.Student ( mainStudentController ) where

import Char ( toLower )
import List ( (\\), isSuffixOf )
import System.Helpers
import System.Spicey
import HTML.Base
import Time
import MDB
import View.Student
import Maybe
import Controller.Default
import System.SessionInfo
import System.Authentication
import System.Authorization
import System.AuthorizedActions
import System.Crypto            ( randomString )
import System.MultiLang
import Config.UserProcesses
import View.MDBEntitiesToHtml
import Database.CDBI.Connection

import SpecialQueries ( queryModInstsOfSemester, queryStudentByEmail
                      , queryModInstsOfStudentInSem
                      , queryStudentCoursesOfStudent
                      , queryCoursesOfStudent )

--- Choose the controller for a Student entity according to the URL parameter.
mainStudentController :: Controller
mainStudentController =
  do args <- getControllerParams
     case args of
       [] -> listStudentController
       ["login"]     -> loginController
       ["logout"]    -> logoutController
       ["sendlogin"] -> sendLoginCodeController
       ["select"]    -> selectSemesterController
       ["showcourses"] -> showSelectionController
       ["list"] -> listStudentController
       ["new"] -> newStudentController
       ["show",s] ->
         applyControllerOn (readStudentKey s) getStudent
          showStudentController
       ["edit",s] ->
         applyControllerOn (readStudentKey s) getStudent
          editStudentController
       ["delete",s] ->
         applyControllerOn (readStudentKey s) getStudent
          deleteStudentController
       ["rlogin",s] ->
         applyControllerOn (readStudentKey s) getStudent
          loginAsStudentController
       _ -> displayError "Illegal URL"

--- Shows a form to create a new Student entity.
newStudentController :: Controller
newStudentController =
  checkAuthorization (studentOperationAllowed NewEntity) $ \sinfo -> do
    let t = translate sinfo
    ctime <- getClockTime
    return
     (blankStudentView sinfo ctime
       (\entity -> do
         let (email,_,_,_,_) = entity
         if emailAllowed email
           then do
             studemails <- runQ $ queryStudentByEmail email
             if null studemails
               then do tr <- runT (createStudentT entity)
                       either (\error -> displayError (show error))
                              (sendNewTAN sinfo)
                              tr
               else displayError (t "Email address already registered!")
           else displayError $ t "Email address not allowed!" ++
                               " (" ++ t "only" ++ " stu...@mail.uni-kiel.de)!")
       defaultController)
 where
  emailAllowed s = map toLower s == "mh@informatik.uni-kiel.de" -- for testing
                || (take 3 s == "stu" && "@mail.uni-kiel.de" `isSuffixOf` s)

--- Transaction to persist a new Student entity to the database.
createStudentT :: (String,String,String,String,ClockTime) -> DBAction Student
createStudentT (email,name,first,tAN,lastLogin) =
  newStudent email name first tAN lastLogin >+= returnT

--- Shows a form to edit the given Student entity.
editStudentController :: Student -> Controller
editStudentController studentToEdit =
  checkAuthorization (studentOperationAllowed (UpdateEntity studentToEdit))
   $ (\sinfo ->
     do return
         (editStudentView sinfo studentToEdit
           (\entity ->
             transactionController (updateStudentT entity)
              (nextInProcessOr listStudentController Nothing))
           listStudentController))

--- Transaction to persist modifications of a given Student entity
--- to the database.
updateStudentT :: Student -> DBAction ()
updateStudentT student = updateStudent student

--- Deletes a given Student entity (after asking for confirmation)
--- and proceeds with the list controller.
deleteStudentController :: Student -> Controller
deleteStudentController student =
  checkAuthorization (studentOperationAllowed (DeleteEntity student))
   $ (\_ ->
     confirmController
      [h3
        [htxt
          (concat
            ["Student \"",studentToShortView student,"\" wirklich löschen?"])]]
      (transactionController (deleteStudentT student)
        listStudentController)
      (showStudentController student))

--- Transaction to delete a given Student entity.
deleteStudentT :: Student -> DBAction ()
deleteStudentT student = deleteStudent student

--- Login as a given Student (only for administrator).
loginAsStudentController :: Student -> Controller
loginAsStudentController student = do
  sinfo <- getUserSessionInfo
  if isAdminSession sinfo
    then do let email = studentEmail student
            loginToStudentSession email
            setPageMessage (translate sinfo "Logged in as: " ++ email)
            showSelectionController
    else return [h3 [htxt $ "Operation not allowed!"]]

--- Lists all Student entities with buttons to show, delete,
--- or edit an entity.
listStudentController :: Controller
listStudentController =
  checkAuthorization (studentOperationAllowed ListEntities)
   $ (\sinfo ->
     do students <- runQ queryAllStudents
        return (listStudentView sinfo students))

--- Shows a Student entity.
showStudentController :: Student -> Controller
showStudentController student =
  checkAuthorization (studentOperationAllowed (ShowEntity student))
   $ (\sinfo -> do return (showStudentView sinfo student))

-----------------------------------------------------------------------

--- Send new login data to a student.
sendLoginCodeController :: Controller
sendLoginCodeController = do
  sinfo <- getUserSessionInfo
  return $ sendLoginCodeView defaultController sinfo

--- Login to the system.
loginController :: Controller
loginController = do
  sinfo <- getUserSessionInfo
  case studentLoginOfSession sinfo of
    Just _  -> return [h3 [htxt $ "Operation not allowed!"]]
    Nothing -> return $ studentLoginView defaultController
                                         showSelectionController sinfo

--- Logout the current user.
logoutController :: Controller
logoutController = do
  sinfo <- getUserSessionInfo
  let t = translate sinfo
  case studentLoginOfSession sinfo of
    Nothing -> return [h3 [htxt $ "Operation not allowed!"]]
    Just _  -> do logoutFromSession
                  setPageMessage (t "Logged out")
                  defaultController

-----------------------------------------------------------------------

--- Controller to show selected courses.
showSelectionController :: Controller
showSelectionController = do
  sinfo <- getUserSessionInfo
  case studentLoginOfSession sinfo of
    Nothing    -> return [h3 [htxt $ "Operation not allowed!"]]
    Just email -> do
      cinfos  <- runQ $ queryCoursesOfStudent email
      return $ showSelectionView sinfo cinfos

--- Select modules for a semester.
selectSemesterController :: Controller
selectSemesterController = do
  sinfo <- getUserSessionInfo
  case studentLoginOfSession sinfo of
    Nothing -> return [h3 [htxt $ "Operation not allowed!"]]
    Just _  -> do csem <- getCurrentSemester >>= return . nextSemester
                  return $ selectSemesterView selectCourseController sinfo csem

--- Select modules for a semester.
selectCourseController :: (String,Int) -> Controller
selectCourseController sem = do
  sinfo <- getUserSessionInfo
  case studentLoginOfSession sinfo of
    Nothing    -> return [h3 [htxt $ "Operation not allowed!"]]
    Just email -> do
      mimods <- runQ $ queryModInstsOfSemester sem
      stmis  <- runQ $ queryModInstsOfStudentInSem email sem
      return $ selectCoursesView defaultController
                                 storeCourseSelectionController
                                 sinfo sem stmis mimods

--- Store selected modules for a student.
storeCourseSelectionController :: [ModInstID] -> [ModInstID] -> Controller
storeCourseSelectionController oldmis newmis = do
  sinfo <- getUserSessionInfo
  case studentLoginOfSession sinfo of
    Nothing    -> return [h3 [htxt $ "Operation not allowed!"]]
    Just email -> do
      students <- runQ $ queryStudentByEmail email
      if null students
        then return [h3 [htxt $ "Operation not allowed!"]]
        else do
          let stud = head students
          scids  <- runQ $ queryStudentCoursesOfStudent email
          scs    <- mapM (\scid -> runQ (getStudentCourse scid)) scids
          ctime <- getClockTime
          let addmids = newmis \\ oldmis
              delmids = oldmis \\ newmis 
          deleteCourseSelect scs (studentKey stud) delmids
          runJustT (mapM_ (addCourseSelect ctime (studentKey stud)) addmids)
          showSelectionController
 where
  addCourseSelect ctime si mi =
    newStudentCourseWithStudentStudentCoursesKeyWithModInstStudentCourseInstancesKey
      ctime si mi

  deleteCourseSelect stcourses si mis = 
    mapM (\sc -> if studentCourseStudentStudentCoursesKey sc == si &&
                    studentCourseModInstStudentCourseInstancesKey sc `elem` mis
                   then runJustT (deleteStudentCourse sc)
                   else return ())
         stcourses
