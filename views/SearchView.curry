--------------------------------------------------------------------------
--- This module implements the views related to searching moduls.
--------------------------------------------------------------------------

module SearchView(searchPageView,showExamOverview)
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
searchPageView :: Maybe String -> (String -> Controller)
               -> ((String,Int) -> Controller) -> Controller
               -> [HtmlExp]
searchPageView login searchcontroller showExamController showAllMods =
  [h1 [htxt "Modulsuche"],
   blockstyle "widelist" [ulist $
      [[htxt "Alle Module mit Zeichenfolge ",
        textfield scode "" `addAttr` ("size","20"),
        htxt " im Modulcode oder -titel ",
        button "suchen" searchHandler],
       [htxt "Alle Module ", button "anzeigen" (nextController showAllMods)]] ++
      if login==Nothing
      then []
      else [[htxt "Prüfungsanforderungen aller Module im ",
            selectionInitial insem semSelection lowerSemesterSelection,
            button "anzeigen" showExams]]
      ]]
 where
  scode,insem free

  searchHandler env = searchcontroller (map toLower (env scode)) >>= getForm

  semSelection = map (\(s,i) -> (showSemester s,show i))
                     (zip semesterSelection [0..])

  showExams env =
    let semi = maybe 0 id (findIndex (\(_,i) -> i==(env insem)) semSelection)
     in showExamController (semesterSelection!!semi)
         >>= getForm

-----------------------------------------------------------------------------
--- Supplies a view for the examination requirements of a given list of modules.
showExamOverview :: (String,Int) -> [(ModData,String)] -> [HtmlExp]
showExamOverview sem mods =
  [h1 [htxt $ "Prüfungsanforderungen aller Module im " ++ showSemester sem],
   headedTable $
     [[htxt "Modul:"],[htxt "Prüfungsanforderungen:"]] :
     map (\ (m,e) -> [[htxt (modDataCode m ++": "++ modDataNameG m)],
                     [HtmlText (docText2html e)]])
         (mergeSort (\ (m1,_) (m2,_) -> leqModData m1 m2) mods)]

-----------------------------------------------------------------------------
