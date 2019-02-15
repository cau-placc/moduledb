module View.Student
  ( wStudent, tuple2Student, student2Tuple, wStudentType, blankStudentView
  , createStudentView, editStudentView, showStudentView, listStudentView
  , sendLoginCodeView, studentLoginView, sendNewTAN
  , showSelectionView, selectSemesterView, selectCoursesView )
where

import List ( groupBy, transpose )
import Mail ( sendMail )
import WUI
import HTML.Base
import Time
import Sort
import HTML.Styles.Bootstrap3
import System.Authentication
import System.Crypto ( randomString )
import System.Helpers
import System.MultiLang
import System.Spicey
import System.SessionInfo
import ConfigMDB ( adminEmail )
import MDB
import MDBExts ( showModDataID )
import View.MDBEntitiesToHtml
import Database.CDBI.Connection ( DBAction )

import SpecialQueries ( queryStudentByEmail )

--- The WUI specification for the entity type Student.
wStudent :: Bool -> WuiSpec (String,String,String,String,ClockTime)
wStudent editall =
  withRendering
   (w5Tuple wRequiredString wRequiredString wString
            (wConstant htxt)
            (wConstant (htxt . calendarTimeToString . toUTCTime)))
   (renderLabels ((if editall then id else take 3)studentLabelList))

--- Transformation from data of a WUI form to entity type Student.
tuple2Student :: Student -> (String,String,String,String,ClockTime) -> Student
tuple2Student studentToUpdate (email,name,first,tAN,lastLogin) =
  setStudentEmail
   (setStudentName
     (setStudentFirst
       (setStudentTAN (setStudentLastLogin studentToUpdate lastLogin) tAN)
       first)
     name)
   email

--- Transformation from entity type Student to a tuple
--- which can be used in WUI specifications.
student2Tuple :: Student -> (String,String,String,String,ClockTime)
student2Tuple student =
  (studentEmail student
  ,studentName student
  ,studentFirst student
  ,studentTAN student
  ,studentLastLogin student)

--- WUI Type for editing or creating Student entities.
--- Includes fields for associated entities.
wStudentType :: Student -> WuiSpec Student
wStudentType student =
  transformWSpec (tuple2Student student,student2Tuple) (wStudent True)

--- Supplies a WUI form to create a new Student entity.
--- The fields of the entity have some default values.
blankStudentView
  :: UserSessionInfo
  -> ClockTime
  -> ((String,String,String,String,ClockTime) -> Controller)
  -> Controller -> [HtmlExp]
blankStudentView sinfo ctime controller cancelcontroller =
  createStudentView sinfo "stu...@mail.uni-kiel.de" "" "" "123456"
                    ctime controller cancelcontroller

--- Supplies a WUI form to create a new Student entity.
--- Takes default values to be prefilled in the form fields.
createStudentView
  :: UserSessionInfo
  -> String
  -> String
  -> String
  -> String
  -> ClockTime
  -> ((String,String,String,String,ClockTime) -> Controller)
  -> Controller -> [HtmlExp]
createStudentView
    sinfo
    defaultEmail
    defaultName
    defaultFirst
    defaultTAN
    defaultLastLogin
    controller
    cancelcontroller =
  renderWuiForm (wStudent False)
   (defaultEmail,defaultName,defaultFirst,defaultTAN,defaultLastLogin)
   controller
   cancelcontroller
   (t "Register as new student")
   (t "Register")
 where t = translate sinfo

--- Supplies a WUI form to edit the given Student entity.
--- Takes also associated entities and a list of possible associations
--- for every associated entity type.
editStudentView
  :: UserSessionInfo
  -> Student -> (Student -> Controller) -> Controller -> [HtmlExp]
editStudentView _ student controller cancelcontroller =
  renderWuiForm (wStudentType student) student controller cancelcontroller
   "Studierendendaten bearbeiten"
   "Ändern"

--- Supplies a view to show the details of a Student.
showStudentView :: UserSessionInfo -> Student -> [HtmlExp]
showStudentView _ student =
  studentToDetailsView student
   ++ [hrefButton "?Student/list" [htxt "Zur Studierendenliste"]]

--- Compares two Student entities. This order is used in the list view.
leqStudent :: Student -> Student -> Bool
leqStudent x1 x2 =
  (studentName x1, studentFirst x1, studentEmail x1)
   <= (studentName x2, studentFirst x2, studentEmail x2)

--- Supplies a list view for a given list of Student entities.
--- Shows also show/edit/delete buttons if the user is logged in.
--- The arguments are the session info and the list of Student entities.
listStudentView :: UserSessionInfo -> [Student] -> [HtmlExp]
listStudentView sinfo students =
  [h1 [htxt "Liste der registrierten Studierenden"]
  ,spTable
    ([take 3 studentLabelList] ++
     map listStudent (mergeSortBy leqStudent students))]
  where
    listStudent student =
      studentToListView student
       ++ (if not (isAdminSession sinfo)
              then []
              else [[hrefButton ("?Student/show/" ++ showStudentKey student)
                      [htxt "Anzeigen"]]
                   ,[hrefButton ("?Student/edit/" ++ showStudentKey student)
                      [htxt "Ändern"]]
                   ,[hrefButton ("?Student/delete/" ++ showStudentKey student)
                      [htxt "Löschen"]]
                   ,[hrefButton ("?Student/rlogin/" ++ showStudentKey student)
                      [htxt "Anmelden"]]])

-----------------------------------------------------------------------------
--- View to login.
studentLoginView :: Controller -> Controller -> UserSessionInfo -> [HtmlExp]
studentLoginView dfltcontroller landingcontroller sinfo =
  [par (studentLoginExplanation sinfo),
   h3 [htxt $ t "Login as student"],
   spTable [[[htxt $ t "Email address:"], [textfield emailfield ""]],
            [[htxt $ t "Login code:"],    [textfield codefield ""]]],
   par [spPrimButton (t "Login") loginHandler, nbsp,
        hrefPrimButton "?Student/sendlogin"
                       [htxt $ t "Forgot your login data?"]],
   hrule,
   par [hrefPrimButton "?Student/new" [htxt $ t "Register as new student"]]
  ]
 where
  emailfield,codefield free

  t = translate sinfo

  loginHandler env = do
    let email = env emailfield
        code  = env codefield
    estudents <- runQ $ queryStudentByEmail email 
    let students = filter (\s -> studentTAN s == code) estudents
    if null students
      then do setPageMessage $ t "Wrong login data!"
              nextInProcessOr dfltcontroller Nothing >>= getForm
      else do loginToStudentSession email
              ctime <- getClockTime
              runT (updateStudent (setStudentLastLogin (head students) ctime))
              setPageMessage (t "Logged in as: " ++ email)
              landingcontroller >>= getForm

------------------------------------------------------------------------
-- send new login code to student
sendLoginCodeView :: Controller -> UserSessionInfo -> [HtmlExp]
sendLoginCodeView controller sinfo =
  [h1 [htxt $ t "Send new login code"],
   par [htxt $ sendCodeCmt sinfo],
   par [htxt $ t "Your email address used for registration: ",
               textfield emailref ""],
   hrule,
   spPrimButton (t "Send new login code") sendHandler,
   spButton (t "Cancel")
            (const (cancelOperation >> controller >>= getForm))]
 where
  emailref free

  t = translate sinfo

  sendHandler env = do
    students <- runQ $ queryCondStudent (\u -> studentEmail u == env emailref)
    if null students
     then displayError (unknownUser sinfo) >>= getForm
     else sendNewTAN sinfo (head students) >>= getForm

-- send a new TAN to a student
sendNewTAN :: UserSessionInfo -> Student -> Controller
sendNewTAN sinfo student = do
  newcode <- randomString 6
  sendMail adminEmail
           (studentEmail student)
           (t "Login data for module database")
           (studentLoginEmailText sinfo (studentEmail student) newcode)
  runT $ updateStudent (setStudentTAN student newcode)
  return [h1 [htxt $ t "Acknowledgment"],
          h3 [htxt $ t "Your new login code has been sent to:" ++ " " ++
                     studentEmail student]]
 where
  t = translate sinfo

------------------------------------------------------------------------
-- view to show selected modules
showSelectionView :: UserSessionInfo
                  -> [(ModDataID,String,String,String,String,Int)]
                  -> [HtmlExp]
showSelectionView sinfo mis =
  [par (studentExplanation sinfo),
   hrule,
   hrefPrimButton "?Student/select" [htxt (t "Select/change modules")],
   hrule,
   h2 [htxt $ t "Your selected modules:"]] ++
  (if null semgroups
     then []
     else
       concatMap (\cs -> [h3 [htxt $ showLongSemester (semOf (head cs))],
                          spTable (map (\c -> [showCourse c]) cs)])
                 semgroups)
 where
  t = translate sinfo

  semgroups = groupBy sameSem (mergeSortBy leqCourse mis)

  leqCourse (_,c1,g1,e1,t1,y1) (_,c2,g2,e2,t2,y2) =
    (y1,t1,langSelect sinfo e1 g1,c1) <= (y2,t2,langSelect sinfo e2 g2,c2)

  semOf (_,_,_,_,s,y) = (s,y)

  sameSem x y = semOf x == semOf y

  showCourse (mdkey,code,gtitle,etitle,_,_) =
    [hrefModule ("?ModData/show/"++showModDataID mdkey)
                [htxt $ langSelect sinfo etitle gtitle,
                 htxt $ " (" ++ code ++ ")"]]

-- view to select modules
selectCoursesView :: Controller -> ([ModInstID] -> [ModInstID] -> Controller)
                  -> UserSessionInfo -> (String,Int) -> [ModInstID]
                  -> [(ModInstID,ModDataID,String,String,String)] -> [HtmlExp]
selectCoursesView cancelcontroller storecontroller sinfo sem stmis mititles =
  [h1 [htxt $ t "Select the modules you like to attend in " ++
              showSemester sem],
   spTable (map (map listModInst) selMatrix),
   spPrimButton (t "Store") storeHandler,
   spButton (t "Cancel")
            (const (cancelOperation >> cancelcontroller >>= getForm))]
 where
  t = translate sinfo

  leqMIT (_,_,c1,g1,e1) (_,_,c2,g2,e2) =
    (langSelect sinfo e1 g1, c1) <= (langSelect sinfo e2 g2, c2)

  selList = map (\x -> (x,unknown)) (mergeSortBy leqMIT mititles)
  selMatrix =
    if n <= 20
      then map (\x -> [x]) selList
      else transpose [take (n `div` 2) selList, drop (n `div` 2) selList]
   where n = length selList

  listModInst ((mi,mdkey,code,gtitle,etitle),ref) =
    [(if mi `elem` stmis then checkedbox else checkbox) ref "True", nbsp,
     withELink $ hrefModule
       ("?ModData/show/"++showModDataID mdkey)
       [htxt $ langSelect sinfo etitle gtitle ++ " (" ++ code ++ ")"]]

  storeHandler env = do
    let selected = concatMap (\ ((mi,_,_,_,_),ref) ->
                                if null (env ref) then [] else [mi])
                             selList
    storecontroller stmis selected >>= getForm

------------------------------------------------------------------------
