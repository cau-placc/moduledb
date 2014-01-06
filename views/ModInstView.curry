module ModInstView (
 wModInst, tuple2ModInst, modInst2Tuple,
 addModInstView,
 editModInstView, showModInstView, listModInstView, leqModInst,
 singleModInstView
 ) where

import WUI
import HTML
import Time
import Sort
import Spicey
import MDB
import MDBEntitiesToHtml
import ConfigMDB
import Helpers
import UserView(leqUser)

--- The WUI specification for a term/year/user tuple.
wModInst :: [User] -> WuiSpec (String,Int,User)
wModInst userList =
  withRendering
   (wTriple wTerm wYear (wSelect userToShortView (mergeSort leqUser userList)))
   (renderLabels (map (\s -> [stringToHtml s]) ["Semester","Jahr","Dozent"]))

--- The WUI specification for a list of term/year/user tuples with a deletion
--- option.
wListModInst :: [User] -> WuiSpec [(String,Int,User,Bool)]
wListModInst userList =
  wList
   (w4Tuple wTerm wYear (wSelect userToShortView (mergeSort leqUser userList))
           (wCheckBool [htxt "Semester löschen"]) )

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

--- WUI Type for editing a list of ModInst entities.
--- Includes fields for associated entities.
wModInstsType :: ModData -> [ModInst] -> [User] -> WuiSpec [(ModInst,Bool)]
wModInstsType _ insts userList =
  transformWSpec
   (\modinsts -> map (\ (mi,(t,y,u,d)) -> (tuple2ModInst mi (t,y,u),d))
                     (zip insts modinsts),
    map (\ (mi,b) -> add4 b (modInst2Tuple userList mi)))
   (wListModInst userList)
 where
   add4 b (x,y,z) = (x,y,z,b)

--- Supplies a WUI form to create a new ModInst entity.
--- Takes default values to be prefilled in the form fields.
addModInstView :: User -> [User]
               -> (Bool -> (String,Int,User) -> Controller) -> [HtmlExp]
addModInstView defaultUser possibleUsers storecontroller =
  let initdata = (currentTerm,currentYear,defaultUser)
      
      wuiframe = wuiEditForm "Neues Semester hinzufügen" "Hinzufügen"
                             (storecontroller False initdata)
      
      (hexp ,handler) = wuiWithErrorForm
                         (wModInst possibleUsers) initdata
                         (nextControllerForData (storecontroller True))
                         (wuiFrameToForm wuiframe)
   in wuiframe hexp handler

--- Supplies a WUI form to edit the given ModInst entity.
--- Takes also associated entities and a list of possible associations
--- for every associated entity type.
editModInstView :: ModData -> [ModInst] -> [User]
                -> (Bool -> [(ModInst,Bool)] -> Controller) -> [HtmlExp]
editModInstView md insts possibleUsers controller =
  let wuiframe = wuiEditFormWithText
                    "Semesterangaben ändern" "Änderungen speichern"
                    [par [htxt modinstcomment]]
                    (controller False (map (\i -> (i,False)) insts))
      
      (hexp ,handler) = wuiWithErrorForm
                         (wModInstsType md insts possibleUsers)
                         (map (\i -> (i,False)) insts)
                         (nextControllerForData (controller True))
                         (wuiFrameToForm wuiframe)
   in wuiframe hexp handler
 where
  modinstcomment = "Anmerkung: Alte Veranstaltungen und Veranstaltungen, "++
    "die schon in Masterprogrammen eingeplant sind, können nicht "++
    "verändert werden!"

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
listModInstView
 :: [ModInst] -> (ModInst -> Controller) -> (ModInst -> Controller)
  -> (ModInst -> Bool -> Controller) -> [HtmlExp]
listModInstView modInsts showModInstController editModInstController
                deleteModInstController =
  [h1 [htxt "ModInst list"]
  ,spTable
    ([take 2 modInstLabelList] ++
     map listModInst (mergeSort leqModInst modInsts))]
  where listModInst :: ModInst -> [[HtmlExp]]
        listModInst modInst =
          modInstToListView modInst ++
           [[spSmallButton "show"
              (nextController (showModInstController modInst))
            ,spSmallButton "edit"
              (nextController (editModInstController modInst))
            ,spSmallButton "delete"
              (confirmNextController
                (h3
                  [htxt
                    (concat
                      ["Really delete entity \"",modInstToShortView modInst
                      ,"\"?"])])
                (deleteModInstController modInst))]]

--- Supplies a list view for a given list of ModInst entities.
--- Shows also buttons to show, delete, or edit entries.
--- The arguments are the list of ModInst entities
--- and the controller functions to show, delete and edit entities.
singleModInstView :: ModInst -> ModData -> User -> [MasterProgram] -> [HtmlExp]
singleModInstView modinst moddata user mprogs =
  [h1 [htxt $ "Modul \""++ modDataNameG moddata ++ "\" im " ++
              showSemester modinstsem],
   h3 [htxt "Dozent: ", userToHtmlView user]] ++
   if null mprogs
   then [par [htxt notusedcmt]]
   else [par [htxt usedcmt],
         ulist
          (map (\mp -> [htxt "Masterprogramm ",
                   ehref ("?MasterProgram/show/"++showMasterProgramKey mp)
                          [htxt (masterProgramName mp), htxt " (",
                           htxt "Beginn : ",
                           htxt (showSemester (masterProgramTerm mp,
                                               masterProgramYear mp)),
                           htxt ")"]])
           mprogs),
         spEHref ("?ModData/number/"++showModDataKey moddata++
                    "/"++showSemesterCode modinstsem)
                 [htxt $ "Anzahl der Masterstudierenden"]]
 where
  modinstsem = (modInstTerm modinst,modInstYear modinst)

  usedcmt =
   "Dieses Modul ist für dieses Semester in folgenden Masterprogrammen eingeplant:"

  notusedcmt =
   "Dieses Modul ist für dieses Semester in keinem Masterprogramm des Masterstudiengangs Informatik eingeplant."
