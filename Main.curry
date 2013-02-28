--------------------------------------------------------------------------
--- This main module of a spicey application.
--------------------------------------------------------------------------

module Main where

import ControllerMapping
import Spicey
import WUI
import HTML
import Routes
import RoutesData
import Processes
import ModDataController
import MasterProgramController(showXmlMasterProgram,showAllXmlMasterPrograms)
import KeyDatabase
import MDB
import MDBEntitiesToHtml
import Helpers
import List
import Sort
import CSV

dispatcher :: IO HtmlForm
dispatcher = do
  -- get url
  (url,ctrlparams) <- getControllerURL
  
  controller <- nextControllerRefInProcessOrForUrl url >>=
                maybe (displayError "Illegal URL!")
                      getController

  saveLastUrl (url ++ concatMap ("/"++) ctrlparams)
  form <- getForm controller
  return form

--- Main function: check for old form of URL parameters or call the dispatcher
main = do
  params <- getUrlParameter
  case params of
    ('M':_:_:code)         -> showModDataWithCode (urlencoded2string code)
                                                                 >>= getForm
    ('m':'o':'d':'=':code) -> showModDataWithCode (urlencoded2string code)
                                                                 >>= getForm
    ('x':'m':'l':'=':code) -> showXmlModule (urlencoded2string code)
    "xml"                  -> showXmlIndex
    ('x':'m':'l':'p':'r':'o':'g':'=':code)
        -> if code=="all"
           then showAllXmlMasterPrograms
           else maybe (displayError "Illegal URL" >>= getForm)
                      showXmlMasterProgram
                      (readMasterProgramKey (urlencoded2string code))
    "csv" -> allModuleCSV
    _     -> dispatcher


-------------------------------------------------------------------------
-- Script for generating csv of module infos:
allModuleCSV :: IO HtmlForm
allModuleCSV = runQ queryAllModDatas >>= moduleCSV

-- Generate csv of module infos:
moduleCSV :: [ModData] -> IO HtmlForm
moduleCSV mods = do
  csvfields <- mapIO mod2csv (filter isNotCopy mods)
  return (answerEncText "iso-8859-1" (showCSV csvfields))
 where
  isNotCopy m = take 5 (reverse (modDataCode m)) /= "ypoc-"

  mod2csv m = do
    modinsts <- runQ $ queryInstancesOfMod (modDataKey m)
    cats <- runJustT $ getModDataCategorys m
    responsibleUser <- runJustT (getResponsibleUser m)
    return
      [modDataCode m,modDataNameG m,modDataNameE m,
       if null (modDataURL m) then userToShortView responsibleUser else "",
       showDiv10 (modDataECTS m),
       concat . intersperse "|" . sortCats . nub . map categoryShortName $ cats,
       concat . intersperse "|" . map (showSemester . modInstSemester)
              $ modinsts]

  -- Sorting categories according to a wish of Corinna Dort:
  sortCats = mergeSort leqCat
    where leqCat c1 c2 = c1=="G" || (c1=="A" && c2/="G") ||
                         (c1/="G" && c1/="A" && c1 <= c2)

-------------------------------------------------------------------------
-- For benchmarking:
benchMDB = allModuleCSV >>= print . length . show
--> lussac/pakcs: 13.8s
--> lussac/kics2: 0.88s
--> lascombes/pakcs: 7.26
--> lascombes/kics2: 0.6
