--------------------------------------------------------------------------
--- This main module of a spicey application.
--------------------------------------------------------------------------

module Main where

import Data.List
import HTML.WUI
import HTML.Base
import Text.CSV

import Config.ControllerMapping
import Config.RoutesData
import Controller.ModData
import Controller.AdvisorStudyProgram ( showXmlAdvisorStudyProgram
                                      , showAllXmlAdvisorStudyPrograms)
import Controller.MasterProgram       ( showXmlMasterProgram
                                      , showAllXmlMasterPrograms)
import MDB
import MDBExts
import System.Helpers
import System.MultiLang
import System.Logging       ( logUrlParameter )
import System.Routes
import System.Processes
import System.SessionInfo
import System.Spicey
import View.MDBEntitiesToHtml


dispatcher :: IO HtmlPage
dispatcher = do
  -- get url
  (url,ctrlparams) <- getControllerURL
  controller <- nextControllerRefInProcessOrForUrl url >>=
                maybe displayUrlError getController
  saveLastUrl (url ++ concatMap ("/"++) ctrlparams)
  form <- getPage controller
  return form

--- Main function: check for old form of URL parameters or call the dispatcher
main :: IO HtmlPage
main = do
  params <- getUrlParameter
  logUrlParameter params
  case params of
    ('M':s1:s2:code) -> if [s1,s2] `elem`
                            ["BS","BW","B2","MS","MW","ME","M2","NF","EX","OI"]
                        -- for compatibility with old URLs...
                        then showModDataWithCode (urlencoded2string code)
                                                                 >>= getPage
                        else dispatcher
    ('m':'o':'d':'=':code) -> showModDataWithCode (urlencoded2string code)
                                                                 >>= getPage
    ('x':'m':'l':'=':code) -> showXmlModule (urlencoded2string code)
    "xml"                  -> showXmlIndex
    ('x':'m':'l':'p':'r':'o':'g':'=':code)
        -> if code=="all"
           then showAllXmlMasterPrograms
           else maybe (displayUrlError >>= getPage)
                      showXmlMasterProgram
                      (readMasterProgramKey (urlencoded2string code))
    ('x':'m':'l':'a':'p':'r':'o':'g':'=':code)
        -> if code=="all"
           then showAllXmlAdvisorStudyPrograms
           else maybe (displayUrlError >>= getPage)
                      showXmlAdvisorStudyProgram
                      (readAdvisorStudyProgramKey (urlencoded2string code))
    ['l','a','n','g',l1,l2] -> setLanguage [l1,l2]
    "csv"    -> allModuleCSV
    "saveDB" -> storeTermDB >>
                return (answerEncText "iso-8859-1" "DB saved to term files")
    "ping"   -> pingAnswer -- to check whether the MDB server is alive
    _        -> dispatcher

setLanguage :: String -> IO HtmlPage
setLanguage langcode = do
  let lang = if langcode=="EN" then English else German
  updateUserSessionInfo (setLanguageOfSession lang)
  setPageMessage $ if lang==English then "Language: English"
                                    else "Sprache: Deutsch"
  getLastUrl >>= redirectController >>= getPage

-- Send an alive answer (to check whether the MDB server is alive)
pingAnswer :: IO HtmlPage
pingAnswer = return (answerEncText "iso-8859-1" "ALIVE")

-------------------------------------------------------------------------
-- Script for generating an answer page with module infos in CSV format.
allModuleCSV :: IO HtmlPage
allModuleCSV = do
  studyprogs <- runQ queryAllStudyPrograms
  runQ queryAllModDatas >>= modules2CSV studyprogs

-- Generates module infos as CSV string.
modules2CSV :: [StudyProgram] -> [ModData] -> IO HtmlPage
modules2CSV studyprogs mods = do
  csvmods <- mapM mod2csv (filter isNotCopy mods)
  return $ answerEncText "utf-8" $ showCSV csvmods
 where
  isNotCopy m = take 5 (reverse (modDataCode m)) /= "ypoc-"

  mod2csv m = do
    modinsts <- runQ $ queryInstancesOfMod (modDataKey m)
    cats <- runJustT $ getModDataCategories m
    responsibleUser <- runJustT (getResponsibleUser m)
    return
      [modDataCode m,modDataNameG m,modDataNameE m,
       if null (modDataURL m) then userToShortView responsibleUser else "",
       showDiv10 (modDataECTS m),
       concat . intersperse "|" . sortCats . nub . map categoryWithProg $ cats,
       concat . intersperse "|" . map (showSemester . modInstSemester)
              $ modinsts]

  -- Format a category in the form "<cat short name>+<prog short name>"
  -- (as discussed with Corinna Dort)
  categoryWithProg cat =
    categoryShortName cat ++ "+" ++
    let pkey = categoryStudyProgramProgramCategoriesKey cat
    in maybe "???" -- this case should not occur
             studyProgramShortName
             (find (\p -> studyProgramKey p == pkey) studyprogs)

  -- Sorting categories according to a wish of Corinna Ohlsen:
  sortCats = sortBy leqCat
    where leqCat c1 c2 = c1=="G" || (c1=="A" && c2/="G") ||
                         (c1/="G" && c1/="A" && c1 <= c2)

-------------------------------------------------------------------------
-- For benchmarking:
--benchMDB :: IO ()
--benchMDB = allModuleCSV >>= print . length . show
--> lussac/pakcs: 13.8s
--> lussac/kics2: 0.88s
--> lascombes/pakcs: 7.26
--> lascombes/kics2: 0.6
--> belair/pakcs: 5.5s (result: 57332)
--> belair/kics2: 0.7s (result: 57332)

-------------------------------------------------------------------------
