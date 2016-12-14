module MasterProgInfoView (
 wMasterProgInfo, tuple2MasterProgInfo, masterProgInfo2Tuple,
 wMasterProgInfoType,
 editMasterProgInfoView, showMasterProgInfoView, listMasterProgInfoView
 ) where

import WUI
import HTML
import Time
import Sort
import Spicey
import MDB
import MDBEntitiesToHtml
import ConfigMDB
import ReadShowTerm
import Helpers
import List

--- Reads the string-encoded module instances and return them as a
--- list of the form [(catkey,recommended,moddatakey,term,year)]
--- where each sublist are module instances of the same category
--- and contains additional empty instances
readAndExtendProgMods :: (String,Int) -> String
                      -> [[(String,Bool,String,String,Int)]]
readAndExtendProgMods (curterm,curyear) s =
  [filterTag "IG",filterTag "TG",filterTag "IS",filterTag "MV"]
 where
   xs = readQTerm s
   filterTag ct = let mis = filter (\ (t,_,_,_,_) -> ct==t) xs
                   in if length mis == 0 then mis++[emptyMI ct,emptyMI ct]
                                         else mis++[emptyMI ct]
   emptyMI ct = (ct,False,"",curterm,curyear)

--- The WUI specification for the entity type MasterProgInfo.
--- It also includes fields for associated entities.
wMasterProgInfo :: (String,Int) -> [(ModInst,ModData,[Category])]
                -> WuiSpec (String,String,String,String,String,String)
wMasterProgInfo cursem@(curterm,curyear) modinsts =
  withRendering
   (w6Tuple wProgMods wPara wPara wPara wPara wPara)
   (renderLabels masterProgInfoLabelList)
 where
  wProgMods :: WuiSpec String
  wProgMods =
     transformWSpec (\ (e1,e2,e3,e4) -> showQTerm (filterCodes
                                                    (concat [e1,e2,e3,e4])),
                     \ s -> let (e1:e2:e3:e4:_) = readAndExtendProgMods cursem s
                            in (e1,e2,e3,e4))
    (w4Tuple (wList (modsems2wSelect "IG" igmods))
             (wList (modsems2wSelect "TG" tgmods))
             (wList (modsems2wSelect "IS" ismods))
             (wList (modsems2wSelect "MV" mvmods))
     `withRendering` (renderModBorder ["IG","TG","IS","MV"]))

  -- render a tuple as a bordered table with string tags:
  renderModBorder :: [String] -> [HtmlExp] -> HtmlExp
  renderModBorder tags hexps =
     spTable (map (\(t,h)->[[bold [htxt t]],[h]]) (zip tags hexps))


  -- omit all non-module codes in a list of module instances
  filterCodes = filter (\ (_,_,mc,_,_) -> not (null mc))

  modsems2wSelect catkey cs =
   transformWSpec
    (\ (p,(ck,mdk,s,y))->(ck,p,mdk,s,y), \ (ck,p,mdk,s,y)->(p,(ck,mdk,s,y)))
    (wPair
      (wSelectBool "Pflicht" "Empfehlung"
         `withRendering` shorttextinputRendering)
      (wSelect showModSem
               ((catkey,"",curterm,curyear) :cs)
         `withRendering` largetextinputRendering))

  showModSem (_,mc,term,year) =
    if null mc then ""
               else showModTitle mc ++ " (" ++ showSemester (term,year) ++ ")"

  showModTitle mc =
    let mbmi = find (\ (_,md,_) -> showModDataKey md == mc) modinsts
     in maybe mc (\ (_,md,_) -> modDataCode md++": "++modDataNameG md++", "++
                                showDiv10 (modDataECTS md)++" ECTS") mbmi

  leqModSem (mi1,mod1,_) (mi2,mod2,_) =
    let code1 = modDataCode mod1
        code2 = modDataCode mod2
        sem1  = (modInstYear mi1, modInstTerm mi1)
        sem2  = (modInstYear mi2, modInstTerm mi2)
     in code1 < code2 || (code1 == code2 && sem1 <= sem2)

  sortModSem ms = mergeSortBy leqModSem ms

  transModInst c (mi,md,_) = (c,showModDataKey md,modInstTerm mi,modInstYear mi)

  igmods = retrieveCat "IG"
  tgmods = retrieveCat "TG"
  ismods = retrieveCat "IS"
  mvmods = retrieveCat "MV"

  retrieveCat catkey = map (transModInst catkey) $
   sortModSem (filter (\ (_,_,cats) -> catkey `elem` map categoryShortName cats)
                      modinsts)

  wPara = wTextArea (4,70)

--- Transformation from data of a WUI form to entity type MasterProgInfo.
tuple2MasterProgInfo
 :: MasterProgInfo
  -> (String,String,String,String,String,String)
  -> MasterProgInfo
tuple2MasterProgInfo masterProgInfoToUpdate (progModules ,praktikum ,seminar
                                             ,thesis ,allgGrundlagen
                                             ,anwendungsfach) =
  setMasterProgInfoProgModules
   (setMasterProgInfoPraktikum
     (setMasterProgInfoSeminar
       (setMasterProgInfoThesis
         (setMasterProgInfoAllgGrundlagen
           (setMasterProgInfoAnwendungsfach
               masterProgInfoToUpdate
             anwendungsfach)
           allgGrundlagen)
         thesis)
       seminar)
     praktikum)
   progModules

--- Transformation from entity type MasterProgInfo to a tuple
--- which can be used in WUI specifications.
masterProgInfo2Tuple :: MasterProgInfo
                     -> (String,String,String,String,String,String)
masterProgInfo2Tuple masterProgInfo =
  (masterProgInfoProgModules masterProgInfo
  ,masterProgInfoPraktikum masterProgInfo,masterProgInfoSeminar masterProgInfo
  ,masterProgInfoThesis masterProgInfo
  ,masterProgInfoAllgGrundlagen masterProgInfo
  ,masterProgInfoAnwendungsfach masterProgInfo)

--- WUI Type for editing or creating MasterProgInfo entities.
--- Includes fields for associated entities.
wMasterProgInfoType :: (String,Int) -> MasterProgInfo
                    -> [(ModInst,ModData,[Category])]
                    -> WuiSpec MasterProgInfo
wMasterProgInfoType cursem masterProgInfo modinsts =
  transformWSpec
   (tuple2MasterProgInfo masterProgInfo,masterProgInfo2Tuple)
   (wMasterProgInfo cursem modinsts)

--- Supplies a WUI form to edit the given MasterProgInfo entity.
--- Takes also associated entities and a list of possible associations
--- for every associated entity type.
editMasterProgInfoView
 :: MasterProgInfo -> (String,Int) -> [(ModInst,ModData,[Category])]
 -> (Bool -> MasterProgInfo -> Controller) -> [HtmlExp]
editMasterProgInfoView masterProgInfo cursem modinsts controller =
  let initdata = masterProgInfo
      
      wuiframe = wuiEditFormWithText
                   "Modulempfehlungen" "Ändern"
                   [par [htxt "Bitte auch die ",
                         ehref "editprog_infos.html"
                               [htxt "Hinweise zu Masterprogrammen"],
                         htxt " beachten!"]]
                   (controller False initdata)
      
      (hexp ,handler) = wuiWithErrorForm
                         (wMasterProgInfoType cursem masterProgInfo modinsts)
                         initdata (nextControllerForData (controller True))
                         (wuiFrameToForm wuiframe)
   in wuiframe hexp handler

--- Supplies a view to show the details of a MasterProgInfo.
showMasterProgInfoView
 :: MasterProgInfo -> MasterProgram -> Controller -> [HtmlExp]
showMasterProgInfoView masterProgInfo relatedMasterProgram controller =
  masterProgInfoToDetailsView masterProgInfo relatedMasterProgram ++
   [spButton "back to MasterProgInfo list" (nextController controller)]

--- Compares two MasterProgInfo entities. This order is used in the list view.
leqMasterProgInfo :: MasterProgInfo -> MasterProgInfo -> Bool
leqMasterProgInfo x1 x2 =
  (masterProgInfoProgModules x1,masterProgInfoPraktikum x1
  ,masterProgInfoSeminar x1,masterProgInfoThesis x1
  ,masterProgInfoAllgGrundlagen x1,masterProgInfoAnwendungsfach x1) <=
   (masterProgInfoProgModules x2,masterProgInfoPraktikum x2
   ,masterProgInfoSeminar x2,masterProgInfoThesis x2
   ,masterProgInfoAllgGrundlagen x2,masterProgInfoAnwendungsfach x2)

--- Supplies a list view for a given list of MasterProgInfo entities.
--- Shows also buttons to show, delete, or edit entries.
--- The arguments are the list of MasterProgInfo entities
--- and the controller functions to show, delete and edit entities.
listMasterProgInfoView
 :: [MasterProgInfo] -> (MasterProgInfo -> Controller)
  -> (MasterProgInfo -> Controller) -> (MasterProgInfo -> Bool -> Controller)
  -> [HtmlExp]
listMasterProgInfoView masterProgInfos showMasterProgInfoController
                       editMasterProgInfoController
                       deleteMasterProgInfoController =
  [h1 [htxt "MasterProgInfo list"]
  ,spTable
    ([take 6 masterProgInfoLabelList] ++
     map listMasterProgInfo (mergeSortBy leqMasterProgInfo masterProgInfos))]
  where listMasterProgInfo :: MasterProgInfo -> [[HtmlExp]]
        listMasterProgInfo masterProgInfo =
          masterProgInfoToListView masterProgInfo ++
           [[spSmallButton "show"
              (nextController (showMasterProgInfoController masterProgInfo))
            ,spSmallButton "edit"
              (nextController (editMasterProgInfoController masterProgInfo))
            
            ,spSmallButton "delete"
              (confirmNextController
                (h3
                  [htxt
                    (concat
                      ["Really delete entity \""
                      ,masterProgInfoToShortView masterProgInfo,"\"?"])])
                (deleteMasterProgInfoController masterProgInfo))]]
