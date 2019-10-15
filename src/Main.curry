--------------------------------------------------------------------------
--- This main module of a spicey application.
--------------------------------------------------------------------------

module Main where

import Config.ControllerMapping
import System.Spicey
import HTML.WUI
import HTML.Base
import System.Routes
import Config.RoutesData
import System.Processes
import Controller.ModData
import Controller.AdvisorStudyProgram ( showXmlAdvisorStudyProgram
                                      , showAllXmlAdvisorStudyPrograms)
import Controller.MasterProgram (showXmlMasterProgram,showAllXmlMasterPrograms)
import ConfigMDB ( baseLoginURL )
import MDB
import MDBExts
import View.MDBEntitiesToHtml
import System.Helpers
import List
import Sort
import System.MultiLang
import System.SessionInfo

import Text.CSV

dispatcher :: IO HtmlPage
dispatcher = do
  -- get url
  (url0,ctrlparams) <- getControllerURL
  -- if the URL starts with langEN? or langDE?, we set the language and
  -- drop this part of the URL:
  url <- if take 4 url0 == "lang" && url0!!6 == '?'
           then setLanguage (take 2 (drop 4 url0)) >> return (drop 7 url0)
           else return url0
  
  controller <- nextControllerRefInProcessOrForUrl url >>=
                maybe (displayError "Illegal URL!")
                      getController

  saveLastUrl (url ++ concatMap ("/"++) ctrlparams)
  form <- getPage controller
  return form

--- Main function: check for old form of URL parameters or call the dispatcher
main :: IO HtmlPage
main = do
  --sinfo <- getUserSessionInfo
  if True --userLoginOfSession sinfo == Nothing
    then noLoginMain
    else return $ HtmlPage "forward to login session"
           [pageMetaInfo [("http-equiv","refresh"),
                          ("content","1; url=" ++ baseLoginURL)]]
           [par [htxt $ "Since you are logged in, " ++
                        "you will be forwarded to the correct URL..."]]

--- Main function: check for old form of URL parameters or call the dispatcher
noLoginMain :: IO HtmlPage
noLoginMain = do
  params <- getUrlParameter
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
           else maybe (displayError "Illegal URL" >>= getPage)
                      showXmlMasterProgram
                      (readMasterProgramKey (urlencoded2string code))
    ('x':'m':'l':'a':'p':'r':'o':'g':'=':code)
        -> if code=="all"
           then showAllXmlAdvisorStudyPrograms
           else maybe (displayError "Illegal URL" >>= getPage)
                      showXmlAdvisorStudyProgram
                      (readAdvisorStudyProgramKey (urlencoded2string code))
    ['l','a','n','g',l1,l2] -> setLanguage [l1,l2] >> dispatcher
    "csv"    -> allModuleCSV
    "csvold" -> allModuleCSV_OLD
    "saveDB" -> storeTermDB >>
                return (answerEncText "iso-8859-1" "DB saved to term files")
    "ping"   -> pingAnswer -- to check whether the MDB server is alive
    _        -> dispatcher

setLanguage :: String -> IO ()
setLanguage langcode = do
  let lang = if langcode=="EN" then English else German
  updateUserSessionInfo (setLanguageOfSession lang)
  setPageMessage $ if lang==English then "Language: English"
                                    else "Sprache: Deutsch"
  getLastUrl >>= setEnviron "QUERY_STRING"

-- Send an alive answer (to check whether the MDB server is alive)
pingAnswer :: IO HtmlPage
pingAnswer = return (answerEncText "iso-8859-1" "ALIVE")

-------------------------------------------------------------------------
-- Script for generating csv of module infos:
allModuleCSV :: IO HtmlPage
allModuleCSV = do
  studyprogs <- runQ queryAllStudyPrograms
  runQ queryAllModDatas >>= moduleCSV studyprogs

-- Generate csv of module infos:
moduleCSV :: [StudyProgram] -> [ModData] -> IO HtmlPage
moduleCSV studyprogs mods = do
  csvfields <- mapIO mod2csv (filter isNotCopy mods)
  return (answerEncText "iso-8859-1" (showCSV csvfields))
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
  sortCats = mergeSortBy leqCat
    where leqCat c1 c2 = c1=="G" || (c1=="A" && c2/="G") ||
                         (c1/="G" && c1/="A" && c1 <= c2)

-------------------------------------------------------------------------
-- Script for generating csv of module infos (old format!)
allModuleCSV_OLD :: IO HtmlPage
allModuleCSV_OLD = do
  studyprogs <- runQ queryAllStudyPrograms
  runQ queryAllModDatas >>= moduleCSV_OLD studyprogs

-- Generate csv of module infos:
moduleCSV_OLD :: [StudyProgram] -> [ModData] -> IO HtmlPage
moduleCSV_OLD studyprogs mods = do
  csvfields <- mapIO mod2csv (filter isNotCopy mods)
  return (answerEncText "iso-8859-1" (showCSV csvfields))
 where
  isNotCopy m = take 5 (reverse (modDataCode m)) /= "ypoc-"

  studyprogsKeysShortNames =
        map (\sp -> (studyProgramKey sp, studyProgramShortName sp)) studyprogs

  mod2csv m = do
    modinsts <- runQ $ queryInstancesOfMod (modDataKey m)
    cats <- runJustT $ getModDataCategories m
    responsibleUser <- runJustT (getResponsibleUser m)
    return
      [modDataCode m,modDataNameG m,modDataNameE m,
       if null (modDataURL m) then userToShortView responsibleUser else "",
       showDiv10 (modDataECTS m),
       concat . intersperse "|" . sortCats . nub . map categoryShortName
                                                 . filterCats $ cats,
       concat . intersperse "|" . map (showSemester . modInstSemester)
              $ modinsts]

  -- Filter out categories that belong to new BSc15 study programs
  -- that are identified by their short name which ends with "(15)".
  -- This is necessary as long as Corinna Ohlsen does not support
  -- a reasonable solution in the StudiDB.
  filterCats =
    filter (\c -> maybe True -- this case should not occur
                        (\shortname -> not ("(15)" `isSuffixOf` shortname))
                        (lookup (categoryStudyProgramProgramCategoriesKey c)
                                studyprogsKeysShortNames))
                                
  -- Sorting categories according to a wish of Corinna Ohlsen:
  sortCats = mergeSortBy leqCat
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
