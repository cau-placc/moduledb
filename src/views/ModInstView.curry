module ModInstView (
 wModInst, tuple2ModInst, modInst2Tuple,
 addModInstView,
 editModInstView, showModInstView, listModInstView, leqModInst,
 singleModInstView
 ) where

import WUI
import HTML.Base
import Time
import Sort
import Spicey
import MDB
import MDBEntitiesToHtml
import ConfigMDB
import Helpers
import UserView(leqUser)

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

--- Supplies a WUI form to create a new ModInst entity.
--- Takes default values to be prefilled in the form fields.
addModInstView :: User -> [User] -> (String,Int)
               -> (Bool -> (String,Int,User) -> Controller) -> [HtmlExp]
addModInstView defaultUser possibleUsers (curterm,curyear) storecontroller =
  let initdata = (curterm,curyear+1,defaultUser)
      
      wuiframe = wuiEditForm "Neues Semester hinzufügen" "Hinzufügen"
                             (storecontroller False initdata)
      
      (hexp ,handler) = wuiWithErrorForm
                         (wModInst (curyear+1) possibleUsers) initdata
                         (nextControllerForData (storecontroller True))
                         (wuiFrameToForm wuiframe)
   in wuiframe hexp handler

--- Supplies a WUI form to edit the ModInst entities given in the
--- third argument. If the flag for a ModInst entity is true,
--- one can complete change the entity, otherwise one can only
--- change the lecturer of this entity.
--- Takes also associated entities and a list of possible associations
--- for every associated entity type.
editModInstView :: Bool -> (String,Int) -> [(Bool,ModInst)] -> [User]
                -> (Bool -> [(ModInst,Bool)] -> Controller) -> [HtmlExp]
editModInstView admin (_,curyear) binsts possibleUsers controller =
  let insts    = map snd binsts
      wuiframe = wuiEditFormWithText
                    "Semesterangaben ändern" "Änderungen speichern"
                    [par [htxt modinstcomment]]
                    (controller False (map (\i -> (i,False)) insts))
      
      (hexp ,handler) = wuiWithErrorForm
                         (wModInstsType curyear admin insts possibleUsers)
                         (map (\ (b,i) -> if b then Left (i,False) else Right i)
                              binsts)
                         (nextControllerForData
                          (controller True . map (either id (\i -> (i,False)))))
                         (wuiFrameToForm wuiframe)
   in wuiframe hexp handler
 where
  modinstcomment =
    "Anmerkung: Veranstaltungen innerhalb des Planungszeitraumes " ++
    "(bis zu den beiden nächsten Semestern) und Veranstaltungen, "++
    "die schon in Masterprogrammen eingeplant sind, können nicht "++
    "verändert werden! In Ausnahmefällen kontaktieren Sie die " ++
    "Studiengangskoordinatoren."

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
     map listModInst (sortBy leqModInst modInsts))]
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

--- Supplies a view for a single ModInst entity.
--- Shows also the master programs and AdvisorStudyPrograms
--- where this instance is used.
singleModInstView :: ModInst -> ModData -> User -> [MasterProgram]
                  -> [AdvisorStudyProgram] -> [HtmlExp]
singleModInstView modinst moddata user mprogs sprogs =
  [h1 [htxt $ "Modul \""++ modDataNameG moddata ++ "\" im " ++
              showSemester modinstsem],
   h3 [htxt "Dozent: ", userToHtmlView user]] ++
   (if null mprogs && null sprogs
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
                mprogs ++
            map (\sp ->
                 [htxt "Masterprogramm ",
                  ehref
                   ("?AdvisorStudyProgram/show/"++showAdvisorStudyProgramKey sp)
                        [htxt (advisorStudyProgramName sp), htxt " (",
                         htxt "Beginn : ",
                         htxt (showSemester (advisorStudyProgramTerm sp,
                                             advisorStudyProgramYear sp)),
                         htxt ")"]])
                 sprogs)]) ++
   [spEHref ("?ModData/number/"++showModDataKey moddata++
             "/"++showSemesterCode modinstsem)
            [htxt $ "Anzahl der Masterstudierenden"]]
 where
  modinstsem = (modInstTerm modinst,modInstYear modinst)

  usedcmt =
   "Dieses Modul ist für dieses Semester in folgenden Masterprogrammen eingeplant:"

  notusedcmt =
   "Dieses Modul ist für dieses Semester in keinem Masterprogramm des Masterstudiengangs Informatik eingeplant."
