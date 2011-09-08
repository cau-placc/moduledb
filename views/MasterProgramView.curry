module MasterProgramView (
 wMasterProgram, tuple2MasterProgram, masterProgram2Tuple, wMasterProgramType,
 blankMasterProgramView, editMasterProgramView,
 showMasterProgramView, listMasterProgramView, singleMasterProgramView
 ) where

import WUI
import HTML
import Time
import Sort
import Spicey
import MDB
import MDBEntitiesToHtml
import Helpers
import ReadShowTerm
import ConfigMDB
import List
import Maybe
import MasterCoreAreaView

--- The WUI specification for the entity type MasterProgram.
--- It also includes fields for associated entities.
wMasterProgram
 :: Bool -> [MasterCoreArea] -> [User]
  -> WuiSpec (String,String,Int,String,String,String,Bool,MasterCoreArea,User)
wMasterProgram admin masterCoreAreaList userList =
  withRendering
   (w9Tuple wReqStr wTrm wYr wStr wStr wStr
     wVisible
     (wSelect masterCoreAreaToShortView masterCoreAreaList)
     wAdvisor)
   (renderLabels masterProgramLabelList)
 where
  wTrm = if admin then wTerm else wConstant stringToHtml
  wYr  = if admin then wYear else wConstant intToHtml

  wAdvisor = if admin then wSelect userToShortView userList
                      else wConstant (stringToHtml . userToShortView)

  wStr = wTextArea (6,70)
  wReqStr = wRequiredStringSize 70

--- Transformation from data of a WUI form to entity type MasterProgram.
tuple2MasterProgram
 :: MasterProgram
  -> (String,String,Int,String,String,String,Bool,MasterCoreArea,User)
  -> MasterProgram
tuple2MasterProgram masterProgramToUpdate (name ,term ,year ,desc ,prereq
                                           ,comments ,visible ,masterCoreArea
                                           ,user) =
  setMasterProgramName
   (setMasterProgramTerm
     (setMasterProgramYear
       (setMasterProgramDesc
         (setMasterProgramPrereq
           (setMasterProgramComments
             (setMasterProgramVisible
               (setMasterProgramUserAdvisingKey
                 (setMasterProgramMasterCoreAreaAreaProgramsKey
                   masterProgramToUpdate (masterCoreAreaKey masterCoreArea))
                 (userKey user))
               visible)
             comments)
           prereq)
         desc)
       year)
     term)
   name

--- Transformation from entity type MasterProgram to a tuple
--- which can be used in WUI specifications.
masterProgram2Tuple
 :: MasterCoreArea -> User -> MasterProgram
  -> (String,String,Int,String,String,String,Bool,MasterCoreArea,User)
masterProgram2Tuple masterCoreArea user masterProgram =
  (masterProgramName masterProgram,masterProgramTerm masterProgram
  ,masterProgramYear masterProgram,masterProgramDesc masterProgram
  ,masterProgramPrereq masterProgram,masterProgramComments masterProgram
  ,masterProgramVisible masterProgram,masterCoreArea,user)

--- WUI Type for editing or creating MasterProgram entities.
--- Includes fields for associated entities.
wMasterProgramType
 :: Bool -> MasterProgram -> MasterCoreArea -> User -> [MasterCoreArea]
 -> [User] -> WuiSpec MasterProgram
wMasterProgramType admin masterProgram masterCoreArea user masterCoreAreaList
                   userList =
  transformWSpec
   (tuple2MasterProgram masterProgram,masterProgram2Tuple masterCoreArea user)
   (wMasterProgram admin masterCoreAreaList userList)

--- Supplies a WUI form to create a new MasterProgram entity.
blankMasterProgramView
 :: User -> [MasterProgram] -> [MasterCoreArea]
  -> (Bool -> (String,Maybe MasterProgram,String,Int,String,String,
               String,Bool,MasterCoreArea,User)
      -> Controller)
  -> [HtmlExp]
blankMasterProgramView user mprogs possibleMasterCoreAreas controller =
  let initdata = ("",Nothing,currentTerm,currentYear,
                  head possibleMasterCoreAreas)
      
      storeTitleController ::
         Bool -> (String,Maybe MasterProgram,String,Int,MasterCoreArea)
              -> Controller
      storeTitleController store (title,mprog,term,year,mcarea) =
         controller store (title,mprog,term,year,"","","",False,mcarea,user)

      wuiframe = wuiEditForm "Neues Masterprogramm" "Masterprogram anlegen"
                             (storeTitleController False initdata)
      
      (hexp ,handler) = wuiWithErrorForm
                         (wMasterProgramTitle mprogs possibleMasterCoreAreas)
                         initdata (nextControllerForData
                                  (storeTitleController True))
                         (wuiFrameToForm wuiframe)
   in wuiframe hexp handler

wMasterProgramTitle :: [MasterProgram] -> [MasterCoreArea]
           -> WuiSpec (String,Maybe MasterProgram,String,Int,MasterCoreArea)
wMasterProgramTitle mprogs possibleMasterCoreAreas =
  w5Tuple (wStringSize 70)
          (wSelect (maybe "" masterProgramToShortView)
                   (Nothing : map Just mprogs))
          wTerm wYear
          (wSelect masterCoreAreaToShortView possibleMasterCoreAreas)
  `withCondition` (\ (t,mp,_,_,_) -> not (null t && mp==Nothing))
  `withError` "Fehler: Titel eingeben oder altes Masterprogramm auswaehlen!"
  `withRendering`
    renderLabels
     [[textstyle "label label_for_type_string" "Titel (neues Masterprogramm)"]
     ,[textstyle "label label_for_type_string" "oder kopiere dieses Programm:"]
     ,[textstyle "label label_for_type_string" "Beginn im Semester"]
     ,[textstyle "label label_for_type_int" "Beginn im Jahr"]
     ,[textstyle "label label_for_type_relation" "Masterbereich"]]

--- Supplies a WUI form to edit the given MasterProgram entity.
--- Takes also associated entities and a list of possible associations
--- for every associated entity type.
editMasterProgramView
 :: Bool -> MasterProgram -> MasterCoreArea -> User -> [MasterCoreArea]
  -> [User] -> (Bool -> MasterProgram -> Controller)
  -> [HtmlExp]
editMasterProgramView admin masterProgram relatedMasterCoreArea relatedUser
                possibleMasterCoreAreas possibleUsers controller =
  let initdata = masterProgram
      
      wuiframe = wuiEditFormWithText
                   "Masterprogrammbeschreibung" "Änderungen speichern"
                   [par [htxt "Bitte auch die ",
                         ehref "editprog_infos.html"
                               [htxt "Hinweise zu Masterprogrammen"],
                         htxt " beachten!"]]
                   (controller False initdata)
      
      (hexp ,handler) = wuiWithErrorForm
                         (wMasterProgramType admin masterProgram
                           relatedMasterCoreArea relatedUser
                           possibleMasterCoreAreas possibleUsers)
                         initdata (nextControllerForData (controller True))
                         (wuiFrameToForm wuiframe)
   in wuiframe hexp handler

--- Supplies a view to show the details of a MasterProgram.
showMasterProgramView
 :: MasterProgram -> MasterCoreArea -> User -> Controller -> [HtmlExp]
showMasterProgramView masterProgram relatedMasterCoreArea relatedUser
                      controller =
  masterProgramToDetailsView masterProgram relatedMasterCoreArea relatedUser
   ++ [button "back to MasterProgram list" (nextController controller)]

--- Compares two MasterProgram entities. This order is used in the list view.
leqMasterProgram :: MasterProgram -> MasterProgram -> Bool
leqMasterProgram x1 x2 =
  (masterProgramYear x1, masterProgramTerm x1, masterProgramName x1) <=
  (masterProgramYear x2, masterProgramTerm x2, masterProgramName x2)

--- Supplies a list view for a given list of MasterProgram entities.
--- Shows also buttons to show, delete, or edit entries.
--- The arguments are the list of MasterProgram entities
--- and the controller functions to show, delete and edit entities.
listMasterProgramView
 :: Bool -> [MasterProgram] -> [MasterCoreArea] -> (MasterProgram -> Controller)
  -> (MasterProgram -> Controller) -> (MasterProgram -> Bool -> Controller)
  -> [HtmlExp]
listMasterProgramView admin masterPrograms allcoreareas
    showMasterProgramController editMasterProgramController
    deleteMasterProgramController =
  [h1 [htxt "Programme im Masterstudiengang Informatik"]] ++
  categorizeMasterProgs
   (if admin then adminMasterProgram
    else (\mp -> [masterProgramToListView mp]))
   allMasterProgs
 where
   allMasterProgs = reverse (mergeSort leqMasterProgram masterPrograms)

   categorizeMasterProgs formatprog allprogs =
     if null allprogs then [] else
     let prog1 = head allprogs
      in catSems (masterProgramTerm prog1,masterProgramYear prog1) allprogs
    where
     catSems sem progs = if null progs then [] else
       [h2 [htxt ("Beginn: " ++ showSemester sem)]] ++
       (if (fst sem == "SS") then [par [italic [htxt ssCmt]]] else []) ++
       let (semprogs,remprogs) =
               span (\mp -> (masterProgramTerm mp,masterProgramYear mp) == sem)
                    progs
           mcakeys = nub $
                       map masterProgramMasterCoreAreaAreaProgramsKey semprogs
           mcas = map (\k -> fromJust (find (\a -> masterCoreAreaKey a == k)
                                            allcoreareas))
                      mcakeys
        in concatMap
             (\mca ->
               [h3 [href "?listMasterCoreArea"
                     [htxt ("Schwerpunktbereich: "++masterCoreAreaName mca)]],
                ulist
                 (map formatprog
                   (filter (\p -> masterProgramMasterCoreAreaAreaProgramsKey p
                                  == masterCoreAreaKey mca) semprogs))])
             (mergeSort leqMasterCoreArea mcas) ++
           catSems (prevSemester sem) remprogs

   ssCmt = "Bei Beginn im Sommersemester können auch Programme der "++
           "benachbarten Wintersemester gewählt werden. "++
           "Bei der Anpassung berät Sie der Academic Advisor."

   adminMasterProgram :: MasterProgram -> [HtmlExp]
   adminMasterProgram masterProgram =
      [button "show"
              (nextController (showMasterProgramController masterProgram)),
       button "edit"
              (nextController (editMasterProgramController masterProgram)),
       button "delete"
          (confirmNextController
            (h3
              [htxt
                (concat
                  ["Masterprogramm \""
                  ,masterProgramToShortView masterProgram,"\" löschen?"])])
            (deleteMasterProgramController masterProgram)),
       masterProgramToListView masterProgram]

--- Supplies a view for a given MasterProgram entity.
singleMasterProgramView
 :: Bool -> Bool -> User -> MasterProgram -> MasterProgInfo
  -> [(String,Bool,ModData,String,Int)] -> MasterCoreArea
  -> (MasterProgram -> Controller)
  -> (MasterProgram -> Controller) -> (MasterProgram -> Bool -> Controller)
  -> (MasterProgInfo -> Controller) -> [HtmlExp]
singleMasterProgramView admin editallowed advisor mprog mpinfo modinfo mcarea _
   editMasterProgramController deleteMasterProgramController
   editMasterProgInfoController =
  [h1 [htxt ("Masterprogramm "++masterProgramName mprog)]] ++
  (if masterProgramVisible mprog then []
   else [h3 [htxt "(nicht öffentlich sichtbar)"]]) ++
  [h2 [htxt ("Schwerpunktbereich: "++masterCoreAreaName mcarea)],
   h2 [htxt $ "Beginn: " ++ showSemester (startSem,startYear) ++
              " / Research advisor: " ++ userToShortView advisor],
   par $ (if admin || editallowed
          then [button "Beschreibung ändern"
                       (nextController (editMasterProgramController mprog))]
          else []) ++
         (if admin
          then [button "Masterprogramm löschen"
                 (confirmNextController
                    (h3 [htxt (concat
                         ["Masterprogramm \"",
                          masterProgramToShortView mprog,"\" löschen?"])])
                    (deleteMasterProgramController mprog))]
          else [])] ++
  [h3 [htxt "Beschreibung:"],
   par [HtmlText (latex2html (masterProgramDesc mprog))],
   h3 [htxt "Voraussetzungen:"],
   par [HtmlText (latex2html (masterProgramPrereq mprog))],
   h3 [htxt "Kommentare:"],
   par [HtmlText (latex2html (masterProgramComments mprog))],
   h2 [htxt "Masterprogrammübersicht"],
   par $ if admin || editallowed
         then [button "Modulempfehlungen ändern"
                      (nextController (editMasterProgInfoController mpinfo))]
         else [],
   semTable,
   h2 [htxt "Masterprogrammübersicht nach Studienbereichen"]
  ] ++
  concatMap (\ (ms,ml) -> [h4 [htxt $ ml++" ("++ms++")"]] ++
              let mods = filter (\ (c,_,_,_,_) -> c==ms) modinfo
               in if null mods then []
                  else [ulist (map (\m -> formatMods m) mods)])
            masterStudienbereiche ++
  concatMap (\ (title,cnt) -> [h4 [htxt $ title++":"],
                               par [HtmlText (latex2html cnt)]])
            (zip descTitles
                 (map (\sel -> sel mpinfo)
                   [masterProgInfoPraktikum,
                    masterProgInfoSeminar,masterProgInfoThesis,
                    masterProgInfoAllgGrundlagen,masterProgInfoAnwendungsfach]))
 where
   startSem = masterProgramTerm mprog
   startYear = masterProgramYear mprog

   descTitles = ["Praktikum","Seminar","Masterarbeit",
                 "Allgemeine Grundlagen","Anwendungsfach"]

   semTable = table $
     map (\ (s,mss) -> [[bold [htxt $ showSemester s]],
                        concatMap showProgMod mss])
         (map filterSem (take 3 (iterate nextSemester (startSem,startYear))))
    where
     filterSem s = (s, filter (\ (_,_,_,sm,yr)->(sm,yr)==s) modinfo)

     showProgMod (_,p,mod,_,_) =
       [nbsp,
        (opt2tag p) [ehref ("?listModData/"++showModDataKey mod)
                           [htxt (modDataCode mod)]]]

   opt2tag p = if p then bold else italic

   formatMods (_,p,md,_,_) =
     [opt2tag p [modtitle, htxt $ if p then " (Pflicht)" else " (empfohlen)"]]
    where
      modtitle = ehref ("?listModData/"++showModDataKey md)
                       [htxt $ modDataCode md ++": "++ modDataNameG md]


masterStudienbereiche =
  [("IG","Vertiefende Informatik-Grundlagen"),
   ("TG","Vertiefende theoretische Grundlagen"),
   ("IS","Informatik der Systeme"),
   ("MV","Mastervertiefungsbereich")]

