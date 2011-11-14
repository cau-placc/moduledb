module ModDescrView (
 wModDescr, tuple2ModDescr, modDescr2Tuple, wModDescrType,
 editModDescrView, showModDescrView, listModDescrView
 ) where

import WUI
import HTML
import Time
import Sort
import Spicey
import MDB
import MDBEntitiesToHtml
import Helpers

wLanguage = wSelect id ["Deutsch","Englisch"]

--- The WUI specification for the entity type ModDescr.
--- It also includes fields for associated entities.
wModDescr :: WuiSpec (String,String,String,String,String,String,String,
                      String,String,String,String)
wModDescr =
  withRendering
   (w11Tuple wLanguage wPara wPara wPara wPara wPara wPara
             wPara wPara wPara wPara)
   (renderLabels modDescrLabelList)
 where
  wPara = wTextArea (6,70)

--- Transformation from data of a WUI form to entity type ModDescr.
tuple2ModDescr
 :: ModDescr
  -> (String,String,String,String,String,String,String,String,String,String
     ,String)
  -> ModDescr
tuple2ModDescr modDescrToUpdate (language ,shortDesc ,objectives ,contents
                                 ,prereq ,exam ,methods ,use ,literature
                                 ,links ,comments) =
  setModDescrLanguage
   (setModDescrShortDesc
     (setModDescrObjectives
       (setModDescrContents
         (setModDescrPrereq
           (setModDescrExam
             (setModDescrMethods
               (setModDescrUse
                 (setModDescrLiterature
                   (setModDescrLinks
                     (setModDescrComments
                       modDescrToUpdate
                       comments)
                     links)
                   literature)
                 use)
               methods)
             exam)
           prereq)
         contents)
       objectives)
     shortDesc)
   language

--- Transformation from entity type ModDescr to a tuple
--- which can be used in WUI specifications.
modDescr2Tuple
 :: ModDescr
  -> (String,String,String,String,String,String,String,String,String,String
     ,String)
modDescr2Tuple modDescr =
  (modDescrLanguage modDescr,modDescrShortDesc modDescr
  ,modDescrObjectives modDescr,modDescrContents modDescr
  ,modDescrPrereq modDescr,modDescrExam modDescr,modDescrMethods modDescr
  ,modDescrUse modDescr,modDescrLiterature modDescr,modDescrLinks modDescr
  ,modDescrComments modDescr)

--- WUI Type for editing or creating ModDescr entities.
wModDescrType :: ModDescr -> WuiSpec ModDescr
wModDescrType modDescr =
  transformWSpec (tuple2ModDescr modDescr,modDescr2Tuple)
                 wModDescr

--- Supplies a WUI form to edit the given ModDescr entity.
--- Takes also associated entities and a list of possible associations
--- for every associated entity type.
editModDescrView :: ModDescr -> (Bool -> ModDescr -> Controller) -> [HtmlExp]
editModDescrView modDescr controller =
  let initdata = modDescr
      
      wuiframe = wuiEditFormWithText
                   "Modulbeschreibung ändern" "Änderungen speichern"
                   [par [htxt "Bitte auch die allgemeinen ",
                         ehref "edit_infos.html"
                               [htxt "Hinweise zu Modulbeschreibungen"],
                         htxt " beachten!"]]
                   (controller False initdata)
      
      (hexp ,handler) = wuiWithErrorForm
                         (wModDescrType modDescr)
                         initdata (nextControllerForData (controller True))
                         (wuiFrameToForm wuiframe)
   in wuiframe hexp handler

--- Supplies a view to show the details of a ModDescr.
showModDescrView :: ModDescr -> ModData -> Controller -> [HtmlExp]
showModDescrView modDescr relatedModData controller =
  modDescrToDetailsView modDescr relatedModData ++
   [button "back to ModDescr list" (nextController controller)]

--- Compares two ModDescr entities. This order is used in the list view.
leqModDescr :: ModDescr -> ModDescr -> Bool
leqModDescr x1 x2 =
  (modDescrLanguage x1,modDescrShortDesc x1,modDescrObjectives x1
  ,modDescrContents x1,modDescrPrereq x1,modDescrExam x1,modDescrMethods x1
  ,modDescrUse x1,modDescrLiterature x1,modDescrLinks x1,modDescrComments x1)
   <=
   (modDescrLanguage x2,modDescrShortDesc x2,modDescrObjectives x2
   ,modDescrContents x2,modDescrPrereq x2,modDescrExam x2,modDescrMethods x2
   ,modDescrUse x2,modDescrLiterature x2,modDescrLinks x2,modDescrComments x2)

--- Supplies a list view for a given list of ModDescr entities.
--- Shows also buttons to show, delete, or edit entries.
--- The arguments are the list of ModDescr entities
--- and the controller functions to show, delete and edit entities.
listModDescrView
 :: [ModDescr] -> (ModDescr -> Controller) -> (ModDescr -> Controller)
  -> (ModDescr -> Bool -> Controller) -> [HtmlExp]
listModDescrView modDescrs showModDescrController editModDescrController
                 deleteModDescrController =
  [h1 [htxt "ModDescr list"]
  ,table
    ([take 11 modDescrLabelList] ++
     map listModDescr (mergeSort leqModDescr modDescrs))]
  where listModDescr :: ModDescr -> [[HtmlExp]]
        listModDescr modDescr =
          modDescrToListView modDescr ++
           [[button "show" (nextController (showModDescrController modDescr))
            ,button "edit" (nextController (editModDescrController modDescr))
            ,button "delete"
              (confirmNextController
                (h3
                  [htxt
                    (concat
                      ["Really delete entity \"",modDescrToShortView modDescr
                      ,"\"?"])])
                (deleteModDescrController modDescr))]]
