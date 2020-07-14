--------------------------------------------------------------------------
--- This module implements the views related to searching moduls.
--------------------------------------------------------------------------

module View.Search
  ( searchPageView, searchModulesView, showSemModsView
  , selectUserView, selectUserFormView
  , showExamOverview, showAllModuleResponsibleView
  ) where

import System.Spicey
import HTML.Base
import HTML.Styles.Bootstrap4
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
--- A page view for searching modules.
searchPageView :: UserSessionInfo -> HtmlExp -> HtmlExp -> [HtmlExp]
searchPageView sinfo searchform showsemmodsform =
  [h1 [htxt $ t "Search modules"],
   h2 [htxt $ t "Search for individual modules:"],
   par [searchform],
   h2 [htxt $ t "Show modules:"],
   par $
    [hrefPrimSmButton "?search/all"      [htxt $ t "All modules"], nbsp,
     hrefPrimSmButton "?search/english"  [htxt $ t "All English modules"], nbsp,
     hrefPrimSmButton "?search/usermods" [htxt $ t "All modules of a person"]] ++
    if isAdminSession sinfo
      then [nbsp, hrefPrimSmButton "?search/allresp"
                                   [htxt $ t "Alle Modulverantwortlichen"]]
      else [],
   h2 [htxt $ t "Show semester modules:"],
   par [showsemmodsform ]
  ]
 where
  t = translate sinfo

--- A form view with a semester selection to show the modules of a semester.
showSemModsView :: ((String,Int) -> Controller)
                -> ((String,Int) -> Controller)
                -> ((String,Int) -> Controller)
                -> ((String,Int) -> Controller)
                -> (UserSessionInfo, (String,Int)) -> [HtmlExp]
showSemModsView showSemModsController showExamController
                showModSemResponsibles showHandbookController (sinfo,cursem) =
  [ spShortSelectionInitial insem semSelection
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
 where
  insem free

  t = translate sinfo

  semSelection = map (\(s,i) -> (showSemester s,show i))
                     (zip (semesterSelection cursem) [0..])

  showSem controller env =
    let semi = maybe 0 id (findIndex (\(_,i) -> i==(env insem)) semSelection)
    in controller (semesterSelection cursem !! semi) >>= getPage


searchModulesView :: (String -> Controller) -> UserSessionInfo -> [HtmlExp]
searchModulesView searchcontroller sinfo =
  [htxt $ t "Search all modules containing", nbsp,
   textField scode "" `addAttr` ("size","20"), nbsp,
   htxt $ t "in the module code or title", nbsp,
   spPrimButton (t "Search!") searchHandler]
 where
  scode free
  t = translate sinfo

  searchHandler env = searchcontroller (map toLower (env scode)) >>= getPage


-----------------------------------------------------------------------------
--- A view to select a user and apply the given controller to the selected user.
selectUserView :: UserSessionInfo -> HtmlExp -> [HtmlExp]
selectUserView sinfo selectuserform =
  [h1 [htxt $ t "Show modules of a person"],
   par [selectuserform]]
 where
  t = translate sinfo

--- A form view to select a user and apply the given controller
--- to the selected user.
selectUserFormView :: (User -> Controller) -> (UserSessionInfo, [User])
                   -> [HtmlExp]
selectUserFormView usercontroller (sinfo, users) =
  [h1 [htxt $ t "Show modules of a person"],
   htxt $ t "Select person:",
   selectionInitial seluser userSelection 2,
   spPrimButton (t "show modules") selectUser]
 where
  seluser free
  
  t = translate sinfo

  userSelection = map (\ (u,i) -> (userToShortView u, show i))
                      (zip users [0..])

  selectUser env =
    let ui = maybe 0 id (findIndex (\ (_,i) -> i == env seluser) userSelection)
    in usercontroller (users!!ui) >>= getPage

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
