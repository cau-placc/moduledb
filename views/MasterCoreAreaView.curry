module MasterCoreAreaView (
 wMasterCoreArea, tuple2MasterCoreArea, masterCoreArea2Tuple,
 wMasterCoreAreaType, blankMasterCoreAreaView, createMasterCoreAreaView,
 editMasterCoreAreaView, showMasterCoreAreaView,
 leqMasterCoreArea, listMasterCoreAreaView
 ) where

import WUI
import HTML
import Time
import Sort
import Spicey
import MDB
import MDBEntitiesToHtml
import Helpers

--- The WUI specification for the entity type MasterCoreArea.
wMasterCoreArea :: WuiSpec (String,String,String,String,Int)
wMasterCoreArea =
  withRendering
   (w5Tuple wRequiredString wRequiredString (wTextArea (10,60))
            wRequiredString wInt)
   (renderLabels masterCoreAreaLabelList)

--- Transformation from data of a WUI form to entity type MasterCoreArea.
tuple2MasterCoreArea
 :: MasterCoreArea -> (String,String,String,String,Int) -> MasterCoreArea
tuple2MasterCoreArea masterCoreAreaToUpdate (name ,shortName ,description
                                             ,areaKey ,position) =
  setMasterCoreAreaName
   (setMasterCoreAreaShortName
     (setMasterCoreAreaDescription
       (setMasterCoreAreaAreaKey
         (setMasterCoreAreaPosition masterCoreAreaToUpdate position) areaKey)
       description)
     shortName)
   name

--- Transformation from entity type MasterCoreArea to a tuple
--- which can be used in WUI specifications.
masterCoreArea2Tuple :: MasterCoreArea -> (String,String,String,String,Int)
masterCoreArea2Tuple masterCoreArea =
  (masterCoreAreaName masterCoreArea,masterCoreAreaShortName masterCoreArea
  ,masterCoreAreaDescription masterCoreArea
  ,masterCoreAreaAreaKey masterCoreArea,masterCoreAreaPosition masterCoreArea)

--- WUI Type for editing or creating MasterCoreArea entities.
--- Includes fields for associated entities.
wMasterCoreAreaType :: MasterCoreArea -> WuiSpec MasterCoreArea
wMasterCoreAreaType masterCoreArea =
  transformWSpec (tuple2MasterCoreArea masterCoreArea,masterCoreArea2Tuple)
   wMasterCoreArea

--- Supplies a WUI form to create a new MasterCoreArea entity.
--- The fields of the entity have some default values.
blankMasterCoreAreaView
 :: (Bool -> (String,String,String,String,Int) -> Controller) -> [HtmlExp]
blankMasterCoreAreaView controller =
  createMasterCoreAreaView [] [] [] [] 1 controller

--- Supplies a WUI form to create a new MasterCoreArea entity.
--- Takes default values to be prefilled in the form fields.
createMasterCoreAreaView
 :: String -> String -> String -> String -> Int
  -> (Bool -> (String,String,String,String,Int) -> Controller) -> [HtmlExp]
createMasterCoreAreaView defaultName defaultShortName defaultDescription
                         defaultAreaKey defaultPosition controller =
  let initdata = (defaultName,defaultShortName,defaultDescription
                 ,defaultAreaKey,defaultPosition)
      
      wuiframe = wuiEditForm "new MasterCoreArea" "create"
                  (controller False initdata)
      
      (hexp ,handler) = wuiWithErrorForm wMasterCoreArea initdata
                         (nextControllerForData (controller True))
                         (wuiFrameToForm wuiframe)
   in wuiframe hexp handler

--- Supplies a WUI form to edit the given MasterCoreArea entity.
--- Takes also associated entities and a list of possible associations
--- for every associated entity type.
editMasterCoreAreaView
 :: MasterCoreArea -> (Bool -> MasterCoreArea -> Controller) -> [HtmlExp]
editMasterCoreAreaView masterCoreArea controller =
  let initdata = masterCoreArea
      
      wuiframe = wuiEditForm "edit MasterCoreArea" "change"
                  (controller False initdata)
      
      (hexp ,handler) = wuiWithErrorForm (wMasterCoreAreaType masterCoreArea)
                         initdata (nextControllerForData (controller True))
                         (wuiFrameToForm wuiframe)
   in wuiframe hexp handler

--- Supplies a view to show the details of a MasterCoreArea.
showMasterCoreAreaView :: MasterCoreArea -> Controller -> [HtmlExp]
showMasterCoreAreaView masterCoreArea controller =
  masterCoreAreaToDetailsView masterCoreArea ++
   [button "back to MasterCoreArea list" (nextController controller)]

--- Compares two MasterCoreArea entities. This order is used in the list view.
leqMasterCoreArea :: MasterCoreArea -> MasterCoreArea -> Bool
leqMasterCoreArea x1 x2 =
  masterCoreAreaPosition x1 <= masterCoreAreaPosition x2

--- Supplies a list view for a given list of MasterCoreArea entities.
--- Shows also buttons to show, delete, or edit entries.
--- The arguments are the list of MasterCoreArea entities
--- and the controller functions to show, delete and edit entities.
listMasterCoreAreaView
 :: Bool -> [MasterCoreArea] -> (MasterCoreArea -> Controller)
  -> (MasterCoreArea -> Controller) -> (MasterCoreArea -> Bool -> Controller)
  -> [HtmlExp]
listMasterCoreAreaView admin masterCoreAreas showMasterCoreAreaController
                       editMasterCoreAreaController
                       deleteMasterCoreAreaController =
  [h1 [htxt "Schwerpunktbereiche im Masterstudium"]] ++
  if admin
  then [table ([take 5 masterCoreAreaLabelList] ++
               map listMasterCoreArea
                   (mergeSort leqMasterCoreArea masterCoreAreas))]
  else concatMap (\mca -> [h2 [htxt (masterCoreAreaName mca ++ " (" ++
                                     masterCoreAreaShortName mca ++ ")")],
                           par [HtmlText
                                (latex2html (masterCoreAreaDescription mca))]])
                 (mergeSort leqMasterCoreArea masterCoreAreas)
  where listMasterCoreArea :: MasterCoreArea -> [[HtmlExp]]
        listMasterCoreArea masterCoreArea =
          masterCoreAreaToListView masterCoreArea ++
           [[button "show"
              (nextController (showMasterCoreAreaController masterCoreArea))] ++
            if not admin then [] else
            [button "edit"
              (nextController (editMasterCoreAreaController masterCoreArea))
            
            ,button "delete"
              (confirmNextController
                (h3
                  [htxt
                    (concat
                      ["Really delete entity \""
                      ,masterCoreAreaToShortView masterCoreArea,"\"?"])])
                (deleteMasterCoreAreaController masterCoreArea))]]
