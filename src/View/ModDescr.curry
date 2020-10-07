module View.ModDescr (
 wModDescr, tuple2ModDescr, modDescr2Tuple, wModDescrType,
 showModDescrView, listModDescrView
 ) where

import HTML.WUI
import HTML.Base
import Time
import Sort
import System.Spicey
import MDB
import View.MDBEntitiesToHtml
import System.Helpers
import System.SessionInfo

wLanguage :: UserSessionInfo -> WuiSpec String
wLanguage sinfo =
  if isAdminSession sinfo then wSelect id ["Deutsch","Englisch"]
                          else wConstant htxt

--- The WUI specification for the entity type ModDescr.
--- It also includes fields for associated entities.
wModDescr :: UserSessionInfo
          -> WuiSpec (String,String,String,String,String,String,String,
                      String,String,String,String)
wModDescr sinfo =
  withRendering
   (w11Tuple (wLanguage sinfo) wPara wPara wPara wPara wPara wPara
             wPara wPara wPara wPara)
   (renderLabels modDescrLabelList)
 where
  wPara = wTextArea (6,70) `withRendering` renderWithFormControl

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
wModDescrType :: UserSessionInfo -> ModDescr -> WuiSpec ModDescr
wModDescrType sinfo modDescr =
  transformWSpec (tuple2ModDescr modDescr,modDescr2Tuple)
                 (wModDescr sinfo)

-- --- Supplies a WUI form to edit the given ModDescr entity.
-- --- Takes also associated entities and a list of possible associations
-- --- for every associated entity type.
-- editModDescrView :: UserSessionInfo -> ModDescr
--                  -> (Bool -> ModDescr -> Controller)
--                  -> [HtmlExp]
-- editModDescrView sinfo modDescr controller =
--   let initdata = modDescr
--       
--       wuiframe = wuiEditFormWithText
--                    "Modulbeschreibung ändern" "Änderungen speichern"
--                    [par [htxt "Bitte auch die allgemeinen ",
--                          ehref "edit_infos.html"
--                                [htxt "Hinweise zu Modulbeschreibungen"],
--                          htxt " beachten!"]]
--                    (controller False initdata)
--       
--       (hexp ,handler) = wuiWithErrorForm
--                          (wModDescrType sinfo modDescr)
--                          initdata (nextControllerForData (controller True))
--                          (wuiFrameToForm wuiframe)
--    in wuiframe hexp handler

--- Supplies a view to show the details of a ModDescr.
showModDescrView :: ModDescr -> ModData -> [BaseHtml]
showModDescrView modDescr relatedModData =
  modDescrToDetailsView modDescr relatedModData

--- Compares two ModDescr entities. This order is used in the list view.
leqModDescr :: ModDescr -> ModDescr -> Bool
leqModDescr x1 x2 =
  (modDescrLanguage x1,modDescrShortDesc x1,modDescrObjectives x1)
   <=
   (modDescrLanguage x2,modDescrShortDesc x2,modDescrObjectives x2)

--- Supplies a list view for a given list of ModDescr entities.
--- Shows also buttons to show, delete, or edit entries.
--- The arguments are the list of ModDescr entities
--- and the controller functions to show, delete and edit entities.
listModDescrView :: [ModDescr] -> [BaseHtml]
listModDescrView modDescrs =
  [h1 [htxt "ModDescr list"]
  ,spTable
    ([take 11 modDescrLabelList] ++
     map listModDescr (sortBy leqModDescr modDescrs))]
  where listModDescr :: ModDescr -> [[BaseHtml]]
        listModDescr modDescr = modDescrToListView modDescr
