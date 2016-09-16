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
import MultiLang
import SessionInfo

-----------------------------------------------------------------------------
--- A view for searching modules.
searchPageView :: UserSessionInfo -> (String -> Controller)
               -> ((String,Int) -> Controller) -> [HtmlExp]
searchPageView sinfo searchcontroller showExamController =
  [h1 [htxt $ t "Search modules"],
   blockstyle "widelist" [ulist $
      [[htxt $ t "Search all modules containing", nbsp,
        textfield scode "" `addAttr` ("size","20"), nbsp,
        htxt $ t "in the module code or title", nbsp,
        spButton (t "search") searchHandler],
       [htxt $ t "Show", nbsp,
        spHref "?search/all"     [htxt $ t "all modules"], nbsp,
        spHref "?search/english" [htxt $ t "all English modules"]]] ++
      if userLoginOfSession sinfo == Nothing
      then []
      else [[htxt "Prüfungsanforderungen aller Module im ",
            spShortSelectionInitial insem semSelection lowerSemesterSelection,
            spButton (t "show") showExams]]
      ]]
 where
  scode,insem free

  t = translate sinfo

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
   spHeadedTable $
     [[htxt "Modul:"],[htxt "Prüfungsanforderungen:"]] :
     map (\ (m,e) -> [[htxt (modDataCode m ++": "++ modDataNameG m)],
                     [HtmlText (docText2html e)]])
         (mergeSortBy (\ (m1,_) (m2,_) -> leqModData m1 m2) mods)]

-----------------------------------------------------------------------------
