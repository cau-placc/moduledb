module View.Student
  ( wStudent, tuple2Student, student2Tuple, wStudentType
  , showStudentView, listStudentView
  , sendLoginCodeView, sendLoginCodeFormView
  , studentLoginView, studentLoginFormView, sendNewTAN
  , showSelectionView, selectCoursesView )
 where

import Data.List ( groupBy, sortBy, transpose )
import Data.Time

import System.Mail ( sendMail )
import HTML.WUI
import HTML.Base
import HTML.Styles.Bootstrap4

import Config.EntityRoutes
import System.Authentication
import Crypto.Hash ( randomString )
import System.Helpers
import System.MultiLang
import System.Spicey
import System.SessionInfo
import ConfigMDB ( adminEmail )
import MDB
import MDB.Exts    ( showModDataID )
import MDB.Queries ( queryStudentByEmail )
import View.MDBEntitiesToHtml
import Database.CDBI.Connection ( DBAction )


--- The WUI specification for the entity type Student.
wStudent :: UserSessionInfo -> Bool
         -> WuiSpec (String,String,String,String,ClockTime)
wStudent sinfo editall =
  withRendering
   (w5Tuple wRequiredString wRequiredString wString
            (wConstant htxt)
            (wConstant (htxt . calendarTimeToString . toUTCTime)))
   (renderLabels ((if editall then id else take 3) (studentLabelList sinfo)))

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
wStudentType :: UserSessionInfo -> Student -> WuiSpec Student
wStudentType sinfo student =
  transformWSpec (tuple2Student student,student2Tuple) (wStudent sinfo True)

-----------------------------------------------------------------------------
--- Supplies a view to show the details of a Student.
showStudentView :: UserSessionInfo -> Student -> [BaseHtml]
showStudentView sinfo student =
  studentToDetailsView sinfo student
   ++ [hrefScndSmButton "?Student/list" [htxt "Zur Studierendenliste"]]

--- Compares two Student entities. This order is used in the list view.
leqStudent :: Student -> Student -> Bool
leqStudent x1 x2 =
  (studentName x1, studentFirst x1, studentEmail x1)
   <= (studentName x2, studentFirst x2, studentEmail x2)

--- Supplies a list view for a given list of Student entities.
--- Shows also show/edit/delete buttons if the user is logged in.
--- The arguments are the session info and the list of Student entities.
listStudentView :: UserSessionInfo -> [Student] -> [BaseHtml]
listStudentView sinfo students =
  [h1 [htxt "Liste der registrierten Studierenden"]
  ,spTable
    ([take 3 labels ++ [labels!!4]] ++
     map listStudent (sortBy leqStudent students))
  ,emphasize [htxt $ "Anzahl der Studierenden: " ++ show (length students)]
  ]
  where
    labels = studentLabelList sinfo

    listStudent student =
      studentToListView student ++
      (if not (isAdminSession sinfo)
         then []
         else [[hrefPrimBadge (showRoute   student) [htxt "Anzeigen"]]
              ,[hrefPrimBadge (editRoute   student) [htxt "Ändern"]]
              ,[hrefPrimBadge (deleteRoute student) [htxt "Löschen"]]
              ,[hrefPrimBadge (entityRoute "rlogin" student) [htxt "Anmelden"]]
              ])

-----------------------------------------------------------------------------
--- View to login as a student.
studentLoginView :: UserSessionInfo -> BaseHtml -> [BaseHtml]
studentLoginView sinfo loginform =
  [par $ studentLoginExplanation sinfo,
   h3 [htxt $ t "Login as student"],
   loginform,
   hrule,
   par [hrefPrimSmButton "?Student/new" [htxt $ t "Register as new student"]]
  ]
 where
  t = translate sinfo

--- A form view to login as a student.
studentLoginFormView :: Controller -> Controller -> UserSessionInfo -> [HtmlExp]
studentLoginFormView dfltcontroller landingcontroller sinfo =
  [spTable [[[htxt $ t "Email address:"], [textField emailfield ""]],
            [[htxt $ t "Login code:"],    [textField codefield ""]]],
   par [spPrimButton (t "Login") loginHandler, nbsp,
        hrefPrimSmButton "?Student/sendlogin"
                         [htxt $ t "Forgot your login data?"]]
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
              nextInProcessOr dfltcontroller Nothing >>= getPage
      else do loginToStudentSession email
              ctime <- getClockTime
              runT (updateStudent (setStudentLastLogin (head students) ctime))
              setPageMessage (t "Logged in as" ++  " '" ++ email ++ "'")
              landingcontroller >>= getPage

------------------------------------------------------------------------
-- send new login code to student
sendLoginCodeView :: UserSessionInfo -> BaseHtml -> [BaseHtml]
sendLoginCodeView sinfo sendlogincodeform =
  [h1 [htxt $ t "Send new login code"],
   par [htxt $ sendCodeCmt sinfo],
   sendlogincodeform
  ]
 where
  t = translate sinfo

sendLoginCodeFormView :: Controller -> UserSessionInfo -> [HtmlExp]
sendLoginCodeFormView controller sinfo =
  [par [htxt $ t "Your email address used for registration: ",
               textField emailref ""],
   hrule,
   spPrimButton (t "Send new login code") sendHandler,
   spButton (t "Cancel")
            (const (cancelOperation >> controller >>= getPage))]
 where
  emailref free

  t = translate sinfo

  sendHandler env = do
    students <- runQ $ queryCondStudent (\u -> studentEmail u == env emailref)
    if null students
     then displayError (unknownUser sinfo) >>= getPage
     else sendNewTAN sinfo (head students) >>= getPage

-- send a new TAN to a student
sendNewTAN :: UserSessionInfo -> Student -> Controller
sendNewTAN sinfo student = do
  newcode <- randomString 6
  runT (updateStudent (setStudentTAN student newcode)) >>=
    either (\error -> displayError (show error))
      (\_ -> do
        sendMail adminEmail
                 (studentEmail student)
                 (t "Login data for module database")
                 (studentLoginEmailText sinfo (studentEmail student) newcode)
        return [h1 [htxt $ t "Acknowledgment"],
                h3 [htxt $ t "Your new login code has been sent to:" ++ " " ++
                           studentEmail student]])
 where
  t = translate sinfo

------------------------------------------------------------------------
-- view to show selected modules
showSelectionView :: UserSessionInfo
                  -> [(ModDataID,String,String,String,String,Int)]
                  -> [BaseHtml]
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

  semgroups = groupBy sameSem (sortBy leqCourse mis)

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
                  -> (UserSessionInfo, (String,Int), [ModInstID],
                      [(ModInstID,ModDataID,String,String,String)]) -> [HtmlExp]
selectCoursesView cancelcontroller storecontroller (sinfo,sem,stmis,mititles) =
  [h1 [htxt $ t "Select the modules you like to attend in " ++
              showSemester sem],
   blockstyle "table-responsive"
     [spTable (addBadge2TableData (map (map listModInst) selMatrix))],
   spPrimButton (t "Store") storeHandler,
   spButton (t "Cancel")
            (const (cancelOperation >> cancelcontroller >>= getPage))]
 where
  t = translate sinfo

  addBadge2TableData xss =
    map (\xs -> map (\x -> [style "badge badge-light" x]) xs) xss

  leqMIT (_,_,c1,g1,e1) (_,_,c2,g2,e2) =
    (langSelect sinfo e1 g1, c1) <= (langSelect sinfo e2 g2, c2)

  selList = map (\x -> (x,unknown)) (sortBy leqMIT mititles)
  selMatrix =
    if n <= 20
      then map (\x -> [x]) selList
      else transpose [take (n `div` 2) selList, drop (n `div` 2) selList]
   where n = length selList

  listModInst ((mi,mdkey,code,gtitle,etitle),ref) =
    [(if mi `elem` stmis then checkedBox else checkBox) ref "True", nbsp,
     withELink $ hrefModule
       ("?ModData/show/"++showModDataID mdkey)
       [htxt $ langSelect sinfo etitle gtitle ++ " (" ++ code ++ ")"]]

  storeHandler env = do
    let selected = concatMap (\ ((mi,_,_,_,_),ref) ->
                                if null (env ref) then [] else [mi])
                             selList
    storecontroller stmis selected >>= getPage

------------------------------------------------------------------------
