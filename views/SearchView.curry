--------------------------------------------------------------------------
--- This module implements the views related to searching moduls.
--------------------------------------------------------------------------

module SearchView(searchPageView,selectUserView,showExamOverview)
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
   h2 [htxt $ t "Search for individual modules:"],
   par [htxt $ t "Search all modules containing", nbsp,
        textfield scode "" `addAttr` ("size","20"), nbsp,
        htxt $ t "in the module code or title", nbsp,
        spPrimButton (t "Search!") searchHandler],
   h2 [htxt $ t "Show module selections:"],
   par [hrefPrimButton "?search/all"      [htxt $ t "All modules"],
        nbsp,
        hrefPrimButton "?search/english"  [htxt $ t "All English modules"],
        nbsp,
        hrefPrimButton "?search/usermods" [htxt $ t "All modules of a person"]
       ]] ++
  if userLoginOfSession sinfo == Nothing
    then []
    else [h2 [htxt "Prüfungsanforderungen:"],
          par [htxt "Prüfungsanforderungen aller Module im ",
            spShortSelectionInitial insem semSelection lowerSemesterSelection,
            spPrimButton (t "show") showExams]]
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
--- A view to select a user and apply the given controller to the selected user.
selectUserView :: UserSessionInfo -> [User] -> (User -> Controller) -> [HtmlExp]
selectUserView sinfo users usercontroller =
  [h1 [htxt $ t "Show modules of a person"],
   htxt $ t "Select person:",
   selectionInitial seluser userSelection 2,
   spPrimButton (t "show modules") selectUser]
 where
  seluser free
  
  t = translate sinfo

  userSelection = map (\(u,i) -> (userToShortView u, show i))
                      (zip users [0..])

  selectUser env =
    let ui = maybe 0 id (findIndex (\(_,i) -> i==(env seluser)) userSelection)
    in usercontroller (users!!ui) >>= getForm

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
