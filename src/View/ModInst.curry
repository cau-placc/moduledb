module View.ModInst (
 wModInst, tuple2ModInst, modInst2Tuple, wModInstsType,
 showModInstView, listModInstView, leqModInst,
 singleModInstView
 ) where

import HTML.WUI
import HTML.Base
import HTML.Styles.Bootstrap4
import Time
import Sort
import System.SessionInfo
import System.Spicey
import MDB
import View.MDBEntitiesToHtml
import ConfigMDB
import System.Helpers
import View.User(leqUser)

--- The WUI specification for a term/year/user tuple.
wModInst :: Int -> [User] -> WuiSpec (String,Int,User)
wModInst curyear userList =
  withRendering
   (wTriple wTerm (wCurrentYear curyear)
            (wSelect userToShortView (sortBy leqUser userList)))
   (renderLabels (map (\s -> [stringToHtml s]) ["Semester","Jahr","Dozent"]))

--- The WUI specification for a list of term/year/user tuples with a deletion
--- option.
--- If the second argument is true, the year can be arbitrary.
wListModInst :: Int -> Bool -> [User]
             -> WuiSpec [Either (String,Int,User,Bool) (String,Int,User)]
wListModInst curyear someyear userList =
  wList
   (wEither
     (w4Tuple wTerm (if someyear then wYear else (wCurrentYear curyear))
              (wSelect userToShortView (sortBy leqUser userList))
              (wCheckBool [htxt "Semester löschen"]))
     (wTriple (wConstant htxt)
              (wConstant (htxt . show))
              (wSelect userToShortView (sortBy leqUser userList)))
   )

--- Transformation from data of a WUI form to entity type ModInst.
tuple2ModInst :: ModInst -> (String,Int,User) -> ModInst
tuple2ModInst modInstToUpdate (term,year,user) =
  setModInstTerm
   (setModInstYear
     (setModInstUserLecturerModsKey
       modInstToUpdate
       (userKey user))
     year)
   term

--- Transformation from entity type ModInst to a tuple
--- which can be used in WUI specifications.
modInst2Tuple :: [User] -> ModInst -> (String,Int,User)
modInst2Tuple users modInst =
  (modInstTerm modInst,
   modInstYear modInst,
   head (filter (\u -> userKey u == modInstUserLecturerModsKey modInst) users))

--- WUI type for editing a list of ModInst entities.
--- If the second argument is true, the year can be arbitrary.
--- Includes fields for associated entities.
wModInstsType :: Int -> Bool -> [ModInst] -> [User]
              -> WuiSpec [Either (ModInst,Bool) ModInst]
wModInstsType curyear someyear insts userList =
  transformWSpec
   (\modinsts -> map tuple2modinst (zip insts modinsts),
    map modinst2tuple)
   (wListModInst curyear someyear userList)
 where
   tuple2modinst (mi, Left  (t,y,u,d)) = Left  (tuple2ModInst mi (t,y,u),d)
   tuple2modinst (mi, Right (t,y,u))   = Right (tuple2ModInst mi (t,y,u))

   modinst2tuple (Left (mi,b)) = let (x,y,z) = (modInst2Tuple userList mi)
                                 in Left (x,y,z,b)
   modinst2tuple (Right mi)    = Right (modInst2Tuple userList mi)


--- Supplies a view to show the details of a ModInst.
showModInstView :: ModInst -> ModData -> User -> Controller -> [HtmlExp]
showModInstView modInst relatedModData relatedUser controller =
  modInstToDetailsView modInst relatedModData relatedUser ++
   [spButton "back to ModInst list" (nextController controller)]

--- Compares two ModInst entities. This order is used in the list view.
leqModInst :: ModInst -> ModInst -> Bool
leqModInst x1 x2 =
  (modInstYear x1,modInstTerm x1) <= (modInstYear x2,modInstTerm x2)

--- Supplies a list view for a given list of ModInst entities.
--- Shows also buttons to show, delete, or edit entries.
--- The arguments are the list of ModInst entities
--- and the controller functions to show, delete and edit entities.
listModInstView :: [ModInst] -> [HtmlExp]
listModInstView modInsts =
  [h1 [htxt "ModInst list"]
  ,spTable
    ([take 2 modInstLabelList] ++
     map listModInst (sortBy leqModInst modInsts))]
  where listModInst :: ModInst -> [[HtmlExp]]
        listModInst modInst = modInstToListView modInst

--- Supplies a view for a single ModInst entity.
--- Shows also the master programs and AdvisorStudyPrograms
--- where this instance is used.
singleModInstView :: UserSessionInfo -> ModInst -> ModData -> User
                  -> [MasterProgram] -> [AdvisorStudyProgram] -> [HtmlExp]
singleModInstView sinfo modinst moddata lecturer mprogs sprogs =
  [h1 [htxt $ "Modul \""++ modDataNameG moddata ++ "\" im " ++
              showSemester modinstsem],
   h3 [htxt "Dozent: ", userToHtmlView lecturer]] ++
   (if null mprogs && null sprogs
    then [par [htxt notusedcmt]]
    else [par [htxt usedcmt],
          ulist
           (map (\mp ->
                   [ehrefInfoBadge
                      ("?MasterProgram/show/" ++ showMasterProgramKey mp)
                      [htxt (masterProgramName mp), htxt " (",
                       htxt "Beginn : ",
                       htxt (showSemester (masterProgramTerm mp,
                                           masterProgramYear mp)),
                       htxt ")"]])
                mprogs ++
            map (\sp ->
                   [ehrefInfoBadge
                      ("?AdvisorStudyProgram/show/" ++
                       showAdvisorStudyProgramKey sp)
                      [htxt (advisorStudyProgramName sp), htxt " (",
                       htxt "Beginn : ",
                       htxt (showSemester (advisorStudyProgramTerm sp,
                                           advisorStudyProgramYear sp)),
                       htxt ")"]])
                 sprogs)]) ++
   (if Just (userLogin lecturer) == userLoginOfSession sinfo ||
       isAdminSession sinfo
      then [ehrefPrimBadge
              ("?ModData/number/" ++ showModDataKey moddata ++
               "/" ++ showSemesterCode modinstsem)
              [htxt $ "Anzahl der registrierten Studierenden"], nbsp]
      else []) ++
   (if isAdminSession sinfo
      then [ehrefPrimBadge
              ("?ModData/studs/" ++ showModDataKey moddata ++
               "/" ++ showSemesterCode modinstsem)
              [htxt $ "Registrierte Studierende"]]
      else [])
 where
  modinstsem = (modInstTerm modinst,modInstYear modinst)

  usedcmt =
   "Dieses Modul ist für dieses Semester in folgenden Masterprogrammen eingeplant:"

  notusedcmt =
   "Dieses Modul ist für dieses Semester in keinem Masterprogramm des Masterstudiengangs Informatik eingeplant."
