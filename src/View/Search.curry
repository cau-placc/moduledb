--------------------------------------------------------------------------
--- This module implements the views related to searching moduls.
--------------------------------------------------------------------------

module View.Search(searchPageView,selectUserView
                  ,showExamOverview,showAllModuleResponsibleView
                  ) where

import System.Spicey
import HTML.Base
import Char
import List
import System.Helpers
import MDB
import View.MDBEntitiesToHtml
import Sort
import View.ModData
import System.MultiLang
import System.SessionInfo

-----------------------------------------------------------------------------
--- A view for searching modules.
searchPageView :: UserSessionInfo -> (String,Int) -> (String -> Controller)
               -> ((String,Int) -> Controller)
               -> ((String,Int) -> Controller)
               -> ((String,Int) -> Controller)
               -> ((String,Int) -> Controller)
               -> [HtmlExp]
searchPageView sinfo cursem searchcontroller showSemModsController
               showExamController showModSemResponsibles
               showHandbookController =
  [h1 [htxt $ t "Search modules"],
   h2 [htxt $ t "Search for individual modules:"],
   par [htxt $ t "Search all modules containing", nbsp,
        textfield scode "" `addAttr` ("size","20"), nbsp,
        htxt $ t "in the module code or title", nbsp,
        spPrimButton (t "Search!") searchHandler],
   h2 [htxt $ t "Show module selections:"],
   par $
    [hrefPrimButton "?search/all"      [htxt $ t "All modules"], nbsp,
     hrefPrimButton "?search/english"  [htxt $ t "All English modules"], nbsp,
     hrefPrimButton "?search/usermods" [htxt $ t "All modules of a person"]] ++
    if isAdminSession sinfo
      then [nbsp, hrefPrimButton "?search/allresp"
                                 [htxt $ t "Alle Modulverantwortlichen"]]
      else [],
   h2 [htxt $ t "Show semester modules:"],
   par $ [ spShortSelectionInitial insem semSelection
                                   (findSemesterSelection cursem cursem)
         , spPrimButton (t "show modules") (showSem showSemModsController) ] ++
         (if userLoginOfSession sinfo == Nothing
            then []
            else [ spPrimButton (t "show examination requirements")
                                (showSem showExamController)]) ++
         (if isAdminSession sinfo
            then [ spPrimButton (t "persons in charge")
                                (showSem showModSemResponsibles)
                 , spPrimButton (t "format modules (PDF)")
                                (showSem showHandbookController)]
            else [])
  ]
 where
  scode,insem free

  t = translate sinfo

  searchHandler env = searchcontroller (map toLower (env scode)) >>= getForm

  semSelection = map (\(s,i) -> (showSemester s,show i))
                     (zip (semesterSelection cursem) [0..])

  showSem controller env =
    let semi = maybe 0 id (findIndex (\(_,i) -> i==(env insem)) semSelection)
    in controller (semesterSelection cursem !! semi) >>= getForm

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
--- Supplies a view for the examination requirements of a given list of modules.
showAllModuleResponsibleView :: String -> [User] -> [HtmlExp]
showAllModuleResponsibleView title users =
  [h1 [htxt title],
   htxt (intercalate ", " (map userInfo users))]
 where
   userInfo u = unwords [userFirst u, userName u,
                         '<' : userEmail u ++ ">"]

-----------------------------------------------------------------------------