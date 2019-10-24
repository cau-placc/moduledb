
module Controller.Student
  ( mainStudentController, newStudentForm, editStudentWuiForm
  , loginForm, sendLoginCodeForm
  , selectSemesterForm, selectCourseSelectionForm
  ) where

import Char ( isDigit, toLower )
import Global
import List ( (\\), isSuffixOf )
import System.Helpers
import System.Spicey
import HTML.Base
import HTML.Session
import HTML.WUI
import Time

import Config.Storage
import Config.EntityRoutes
import MDB
import View.Student
import Maybe
import System.SessionInfo
import System.Authentication
import System.Authorization
import System.AuthorizedActions
import Crypto.Hash            ( randomString )
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
mainStudentController = do
  args <- getControllerParams
  case args of
    [] -> listStudentController
    ["login"]     -> loginController
    ["logout"]    -> logoutController
    ["sendlogin"] -> sendLoginCodeController
    ["select"]    -> selectSemesterController
    ["showcourses"] -> showSelectionController
    ["list"] -> listStudentController
    ["new"] -> newStudentController
    ["show",s]    -> controllerOnKey s showStudentController
    ["edit",s]    -> controllerOnKey s editStudentController
    ["delete",s]  -> controllerOnKey s deleteStudentController
    ["destroy",s] -> controllerOnKey s destroyStudentController
    ["rlogin",s]  -> controllerOnKey s loginAsStudentController
    _ -> displayUrlError

-----------------------------------------------------------------------
--- Shows a form to create a new Student entity.
newStudentController :: Controller
newStudentController =
  checkAuthorization (studentOperationAllowed NewEntity) $ \sinfo -> do
    ctime <- getClockTime
    setParWuiStore wuiNewStudentStore sinfo
      ("stu...@mail.uni-kiel.de", "", "", "123456", ctime)
    return [formExp newStudentForm]

type NewStudent = (String,String,String,String,ClockTime)

--- The form definition to create a new Student entity
--- containing the controller to insert a new Student entity.
newStudentForm :: HtmlFormDef (UserSessionInfo, WuiStore NewStudent)
newStudentForm =
  pwui2FormDef "Controller.Student.newStudentForm"
    wuiNewStudentStore
    (\sinfo -> wStudent sinfo False)
    (\_ p -> createController p)
    (\sinfo -> renderWUI sinfo "Register as new student" "Register"
                         "?" ())
 where
  createController entity =
    checkAuthorization (studentOperationAllowed NewEntity) $ \sinfo -> do
      let t = translate sinfo  
          (email,_,_,_,_) = entity
      if emailAllowed email
        then do
          studemails <- runQ $ queryStudentByEmail email
          if null studemails
            then runT (createStudentT entity) >>=
                 either (\error -> displayError (show error))
                        (sendNewTAN sinfo)
            else displayError (t "Email address already registered!")
        else displayError $ t "Email address not allowed!" ++
                               " (" ++ t "only" ++ " stu...@mail.uni-kiel.de)!"

---- The data stored for executing the WUI form.
wuiNewStudentStore ::
  Global (SessionStore (UserSessionInfo, WuiStore NewStudent))
wuiNewStudentStore =
  global emptySessionStore (Persistent (inDataDir "wuiNewStudentStore"))

--- Is the email address allowed to register?
emailAllowed :: String -> Bool
emailAllowed s =
     map toLower s == "mh@informatik.uni-kiel.de" -- for testing
  || (take 3 s == "stu" && cauDomain `isSuffixOf` s &&
      all isDigit (drop 3 (take (length s - length cauDomain) s)))
 where
  cauDomain = "@mail.uni-kiel.de"

--- Transaction to persist a new Student entity to the database.
createStudentT :: (String,String,String,String,ClockTime) -> DBAction Student
createStudentT (email,name,first,tAN,lastLogin) =
  newStudent email name first tAN lastLogin >+= returnT

-----------------------------------------------------------------------
--- Shows a form to edit the given Student entity.
editStudentController :: Student -> Controller
editStudentController student =
  checkAuthorization
    (studentOperationAllowed (UpdateEntity student)) $ \sinfo -> do
     setParWuiStore wuiEditStudentWuiStore (sinfo,student) student
     return [formExp editStudentWuiForm]

--- A WUI form to edit Student entity.
--- The default values for the fields are stored in the
--- `wuiEditStudentWuiStore`.
editStudentWuiForm :: HtmlFormDef ((UserSessionInfo,Student), WuiStore Student)
editStudentWuiForm =
  pwui2FormDef "Controller.Student.editStudentWuiForm"
    wuiEditStudentWuiStore
    (\ (sinfo,student) -> wStudentType sinfo student)
    (\_ student ->
       checkAuthorization
         (studentOperationAllowed (UpdateEntity student)) $ \_ ->
           transactionController (updateStudentT student)
             (nextInProcessOr (showStudentController student) Nothing))
    (\ (sinfo,_) ->
          renderWUI sinfo "Studierendendaten bearbeiten" "Change"
                    "?Student/list" ())

---- The data stored for executing the WUI form.
wuiEditStudentWuiStore ::
  Global (SessionStore ((UserSessionInfo,Student), WuiStore Student))
wuiEditStudentWuiStore =
  global emptySessionStore (Persistent (inDataDir "wuiEditStudentWuiStore"))


--- Transaction to persist modifications of a given Student entity
--- to the database.
updateStudentT :: Student -> DBAction ()
updateStudentT student = updateStudent student


-----------------------------------------------------------------------
--- Deletes a given Student entity (after asking for confirmation)
--- and proceeds with the list controller.
deleteStudentController :: Student -> Controller
deleteStudentController student =
  checkAuthorization (studentOperationAllowed (DeleteEntity student)) $ \si ->
    confirmDeletionPage si $ concat
      ["Student \"", studentToShortView student, "\" wirklich löschen?"]

--- Deletes a given Student entity and proceeds with the list controller.
destroyStudentController :: Student -> Controller
destroyStudentController student =
  checkAuthorization (studentOperationAllowed (DeleteEntity student)) $ \_ ->
    transactionController (deleteStudentT student)
                          listStudentController

--- Transaction to delete a given Student entity.
deleteStudentT :: Student -> DBAction ()
deleteStudentT student = deleteStudent student

-----------------------------------------------------------------------
--- Login as a given Student (only for administrator).
loginAsStudentController :: Student -> Controller
loginAsStudentController student = do
  sinfo <- getUserSessionInfo
  if isAdminSession sinfo
    then do let email = studentEmail student
            loginToStudentSession email
            setPageMessage (translate sinfo "Logged in as: " ++ email)
            redirectController "?Student/showcourses"
    else return [h3 [htxt $ "Operation not allowed!"]]

-----------------------------------------------------------------------
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
  return $ sendLoginCodeView sinfo (formExp sendLoginCodeForm)

sendLoginCodeForm :: HtmlFormDef UserSessionInfo
sendLoginCodeForm =
  HtmlFormDef "Controller.Student.sendLoginCodeForm" getUserSessionInfo
    (sendLoginCodeFormView redirectToDefaultController)

--- Login to the system.
loginController :: Controller
loginController = do
  sinfo <- getUserSessionInfo
  case studentLoginOfSession sinfo of
    Just _  -> return [h3 [htxt $ "Operation not allowed!"]]
    Nothing -> return $ studentLoginView sinfo (formExp loginForm)

loginForm :: HtmlFormDef UserSessionInfo
loginForm =
  HtmlFormDef "Controller.Student.loginForm" getUserSessionInfo
    (studentLoginFormView redirectToDefaultController
       (redirectController "?Student/showcourses"))

--- Logout the current user.
logoutController :: Controller
logoutController = do
  sinfo <- getUserSessionInfo
  let t = translate sinfo
  case studentLoginOfSession sinfo of
    Nothing -> return [h3 [htxt $ "Operation not allowed!"]]
    Just _  -> do logoutFromSession
                  setPageMessage (t "Logged out")
                  redirectToDefaultController

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
  let t = translate sinfo
  case studentLoginOfSession sinfo of
    Nothing -> return [h3 [htxt $ "Operation not allowed!"]]
    Just _  ->
      return [h2 [htxt $ t "Select semester:"],
              par [formExp selectSemesterForm]]

--- A form to select modules for a semester.
selectSemesterForm :: HtmlFormDef (UserSessionInfo, (String,Int))
selectSemesterForm = HtmlFormDef "Controller.Student.selectSemesterForm"
  readData (selectSemesterFormView selectCourseController
              "select/change modules in semester")
 where
  readData = do
    sinfo <- getUserSessionInfo
    csem <- getCurrentSemester >>= return . nextSemester
    return (sinfo,csem)

    
--- Select modules for a semester.
selectCourseController :: (String,Int) -> Controller
selectCourseController sem = do
  sinfo <- getUserSessionInfo
  case studentLoginOfSession sinfo of
    Nothing    -> return [h3 [htxt $ "Operation not allowed!"]]
    Just email -> do
      putSessionData selectCoursesFormStore (sem,email)
      return [formExp selectCourseSelectionForm]

selectCourseSelectionForm ::
  HtmlFormDef (UserSessionInfo, (String,Int), [ModInstID],
               [(ModInstID,ModDataID,String,String,String)])
selectCourseSelectionForm =
  HtmlFormDef "Controller.Student.selectCourseSelectionForm" readData
    (selectCoursesView redirectToDefaultController
                       storeCourseSelectionController)
 where
  readData = do
    sinfo <- getUserSessionInfo
    (sem,email) <- getSessionData selectCoursesFormStore (("WS",2020),"")
    mimods <- runQ $ queryModInstsOfSemester sem
    stmis  <- runQ $ queryModInstsOfStudentInSem email sem
    return (sinfo,sem,stmis,mimods)

selectCoursesFormStore :: Global (SessionStore ((String,Int), String))
selectCoursesFormStore =
  global emptySessionStore (Persistent (inDataDir "selectCourseFormStore"))


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
          redirectController "?Student/showcourses"
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
