module View.MasterProgram (
 showMasterProgramView, listMasterProgramView, singleMasterProgramView
 ) where

import List
import Maybe
import Sort

import HTML.Base
import HTML.Styles.Bootstrap4

import System.Spicey
import MDB
import MDBExts
import View.MDBEntitiesToHtml
import System.Helpers
import View.MasterCoreArea
import System.SessionInfo
import System.MultiLang


--- Supplies a view to show the details of a MasterProgram.
showMasterProgramView :: MasterProgram -> MasterCoreArea -> User -> [BaseHtml]
showMasterProgramView masterProgram relatedMasterCoreArea relatedUser =
  masterProgramToDetailsView masterProgram relatedMasterCoreArea relatedUser

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
  :: UserSessionInfo -> Bool
  -> [(MasterProgramID,String,String,Int,Bool,MasterCoreAreaID)]
  -> [MasterCoreArea] -> [BaseHtml]
listMasterProgramView sinfo listall mpinfos allcoreareas =
  [h1 [htxt $ t "Master programs in computer science"]] ++
  categorizeMasterProgs mpListView sortedmpinfos
 where
   t = translate sinfo

   mpListView (mpkey,name,_,_,vis,_) =
     [href ("?MasterProgram/show/"++masterProgramKeyToString mpkey)
           [if vis then stringToHtml name
                   else italic [stringToHtml name]]]

   sortedmpinfos = reverse (mergeSortBy leqMP mpinfos)
     where leqMP (_,name1,term1,year1,_,_) (_,name2,term2,year2,_,_) =
             (year1,term1,name1) <= (year2,term2,name2)

   categorizeMasterProgs formatprog allmpinfos =
     if null allmpinfos then [] else
     let (_,_,term,year,_,_) = head allmpinfos
      in catSems (term,year) allmpinfos ++
         if listall then [] else
          [hrule,
           par [hrefPrimSmButton "?MasterProgram/listall"
                                 [htxt $ t "Show all master programs"]]]
    where
     catSems sem progs = if null progs then [] else
       [hrule, h2 [htxt $ t "Start: " ++ showLongSemester sem]] ++
       (if (fst sem == "SS") then [par [italic [htxt $ ssComment sinfo]]]
                             else []) ++
       let (semprogs,remprogs) =
               span (\ (_,_,term,year,_,_) -> (term,year) == sem)
                    progs
           mcakeys = nub $ map (\ (_,_,_,_,_,mcakey) -> mcakey) semprogs
           mcas = map (\k -> fromJust (find (\a -> masterCoreAreaKey a == k)
                                            allcoreareas))
                      mcakeys
        in concatMap
             (\mca ->
               [h3 [ehref "?MCA/list"
                          [htxt $ t "Core area: " ++ masterCoreAreaName mca]],
                ulist
                 (map formatprog
                   (filter (\ (_,_,_,_,_,mcak) -> mcak == masterCoreAreaKey mca)
                           semprogs))])
             (mergeSortBy leqMasterCoreArea mcas) ++
           catSems (prevSemester sem) remprogs


--- Supplies a view for a given MasterProgram entity.
singleMasterProgramView
  :: User -> MasterProgram -> MasterProgInfo
  -> [(String,Bool,ModData,String,Int)] -> MasterCoreArea -> String
  -> [BaseHtml]
singleMasterProgramView advisor mprog mpinfo modinfo mcarea
   xmlurl =
  [h1 [htxt (masterProgramName mprog), nbsp,
       ehrefScndBadge xmlurl [htxt "XML"]]] ++
  (if masterProgramVisible mprog then []
   else [h4 [htxt "(nicht öffentlich sichtbar)"]]) ++
  [h3 [htxt ("Masterprogramm im Schwerpunktbereich: "++masterCoreAreaName mcarea)],
   h3 [htxt $ "Beginn: " ++ showSemester (startSem,startYear) ++
              " / Research advisor: " ++ userToShortView advisor],
   h4 [htxt "Beschreibung:"],
   par [htmlText (docText2html (masterProgramDesc mprog))],
   h4 [htxt "Voraussetzungen:"],
   par [htmlText (docText2html (masterProgramPrereq mprog))],
   h4 [htxt "Kommentare:"],
   par [htmlText (docText2html (masterProgramComments mprog))],
   h3 [htxt "Masterprogrammübersicht"],
   semTable,
   h3 [htxt "Masterprogrammübersicht nach Studienbereichen"]
  ] ++
  concatMap (\ (ms,ml) -> [h4 [htxt $ ml++" ("++ms++")"]] ++
              let mods = filter (\ (c,_,_,_,_) -> c==ms) modinfo
               in if null mods then []
                  else [ulist (map (\m -> formatMods m) mods)])
            masterStudienbereiche ++
  concatMap (\ (title,cnt) -> [h4 [htxt $ title++":"],
                               par [htmlText (docText2html cnt)]])
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

   semTable = spTable $
     map (\ (s,mss) -> [[bold [htxt $ showSemester s]],
                        concatMap showProgMod mss])
         (map filterSem (take 3 (iterate nextSemester (startSem,startYear))))
    where
     filterSem s = (s, filter (\ (_,_,_,sm,yr)->(sm,yr)==s) modinfo)

     showProgMod (_,p,mod,_,_) =
       [nbsp,
        (opt2tag p) [smallHrefModule ("?ModData/show/"++showModDataKey mod)
                                     [htxt (modDataCode mod)]]]

   opt2tag p = if p then bold else italic

   formatMods (_,p,md,_,_) =
     [opt2tag p [modtitle, htxt $ if p then " (Pflicht)" else " (empfohlen)"]]
    where
      modtitle = ehref ("?ModData/show/"++showModDataKey md)
                       [htxt $ modDataCode md ++": "++ modDataNameG md]


masterStudienbereiche :: [(String, String)]
masterStudienbereiche =
  [("IG","Vertiefende Informatik-Grundlagen"),
   ("TG","Vertiefende theoretische Grundlagen"),
   ("IS","Informatik der Systeme"),
   ("MV","Mastervertiefungsbereich")]

