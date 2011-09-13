--------------------------------------------------------------------------
--- This module implements the views related to searching moduls.
--------------------------------------------------------------------------

module SearchView(searchPageView,showModulePlanView,showExamOverview)
 where

import Spicey
import HTML
import Char
import List
import Helpers
import MDB
import MDBEntitiesToHtml
import Sort
import ModDataView

-----------------------------------------------------------------------------
--- A view for searching modules.
searchPageView :: (String -> Controller)
               -> ((String,Int) -> (String,Int) -> Controller)
               -> ((String,Int) -> Controller)
               -> [HtmlExp]
searchPageView searchcontroller showPlanController showExamController =
  [h1 [htxt "Modulsuche"],
   blockstyle "widelist" [ulist $
      [[htxt "Alle Module mit Zeichenfolge ",
        textfield scode "" `addAttr` ("size","20"),
        htxt " im Modulcode oder -titel ",
        button "suchen" searchHandler],
       [htxt "Planung aller Module von ",
        selectionInitial fromsem semSelection lowerSemesterSelection,
        htxt " bis ",
        selectionInitial tosem   semSelection upperSemesterSelection, nbsp,
        button "anzeigen" showPlan],
       [htxt "Prüfungsanforderungen aller Module im ",
        selectionInitial insem semSelection lowerSemesterSelection,
        button "anzeigen" showExams]]]]
 where
  scode,fromsem,tosem,insem free

  searchHandler env = searchcontroller (map toLower (env scode)) >>= getForm

  semSelection = map (\(s,i) -> (showSemester s,show i))
                     (zip semesterSelection [0..])

  showPlan env =
    let start = maybe 0 id (findIndex (\(_,i) -> i==(env fromsem)) semSelection)
        stop  = maybe 0 id (findIndex (\(_,i) -> i==(env tosem  )) semSelection)
     in showPlanController (semesterSelection!!start) (semesterSelection!!stop)
         >>= getForm

  showExams env =
    let semi = maybe 0 id (findIndex (\(_,i) -> i==(env insem)) semSelection)
     in showExamController (semesterSelection!!semi)
         >>= getForm

-----------------------------------------------------------------------------
--- Supplies a view for a given list of modules and their instances.
showModulePlanView :: [(String,[(ModData,[Maybe ModInst])])]
                   -> [String] -> [User] -> [HtmlExp]
showModulePlanView catmods semheaders users =
  [h1 [htxt "Alle Module"],
   table (concatMap
                 (\ (c,mods) ->
                   ([catStyle c] : map (\s->[catStyle s]) ("ECTS":semheaders)) :
                   map (\ (md,mis) -> modDataToCompactListView md ++
                                      map (maybe [] showModInst) mis)
                       (mergeSort (\ (m1,_) (m2,_) -> leqModData m1 m2) mods))
                 catmods)]
  where
   showModInst mi =
     let miuserkey = modInstUserLecturerModsKey mi
         showUser u = let name = userName u
                       in if length name > 6 then take 5 name ++ "." else name
      in [italic
           [ehref ("?listModInst/"++showModInstKey mi)
                  [htxt (maybe "???" showUser
                               (find (\u -> userKey u == miuserkey) users))]]]

   catStyle c = style "category" [htxt c]

-----------------------------------------------------------------------------
--- Supplies a view for the examination requirements of a given list of modules.
showExamOverview :: (String,Int) -> [(ModData,String)] -> [HtmlExp]
showExamOverview sem mods =
  [h1 [htxt $ "Prüfungsanforderungen aller Module im " ++ showSemester sem],
   headedTable $
     [[htxt "Modul:"],[htxt "Prüfungsanforderungen:"]] :
     map (\ (m,e) -> [[htxt (modDataCode m ++": "++ modDataNameG m)],
                     [HtmlText (latex2html e)]])
         (mergeSort (\ (m1,_) (m2,_) -> leqModData m1 m2) mods)]

-----------------------------------------------------------------------------
