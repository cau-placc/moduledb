--------------------------------------------------------------------------
--- This module contains a controller for search modules.
--------------------------------------------------------------------------
{-# OPTIONS_FRONTEND -F --pgmF=currypp --optF=foreigncode #-}

module Controller.Search
  ( searchController, searchModuleForm, showSemModsForm, searchUserModules
  , selectUserModulesForm )
 where

import Data.List
import Data.Maybe

import HTML.Base
import System.Spicey
import System.Authentication
import MDB
import MDB.Queries
import Database.CDBI.ER 

import Controller.ModData
import Controller.Category
import View.Category
import View.Search
import View.ModData
import View.User (leqUser)
import View.MDBEntitiesToHtml
import System.Helpers
import System.Logging     ( logSearchTerm )
import System.MultiLang
import System.SessionInfo

-----------------------------------------------------------------------------
--- Controller for the main page.
searchController :: Controller
searchController = do
  args <- getControllerParams
  case args of
    ["all"]      -> showAllModulesController
    ["allresp"]  -> showAllModuleResponsibleController
    ["usermods"] -> selectUserModulesController
    ["english"]  -> showAllEnglishModulesController
    _ -> do sinfo <- getUserSessionInfo
            return $ searchPageView sinfo (formElem searchModuleForm)
                                          (formElem showSemModsForm)
                                    
--- A form with a field to search modules containing a string.
searchModuleForm :: HtmlFormDef UserSessionInfo
searchModuleForm =
  formDefWithID "Controller.Search.searchModuleForm"
    (toFormReader $ getUserSessionInfo) (searchModulesView searchModules)

--- A form with a semester selection to show the modules of a semester.
showSemModsForm :: HtmlFormDef (UserSessionInfo, (String,Int))
showSemModsForm =
  formDefWithID "Controller.Search.showSemModsForm"
    readData
    (showSemModsView showSemModsController showExamController
                     showModSemResponsibleController showHandbookController)
 where
  readData = toFormReader $ do
    sinfo <- getUserSessionInfo
    csem  <- getCurrentSemester
    return (sinfo,csem)

--- Controller for searching modules in the module database.
searchModules :: String -> Controller
searchModules pat = do
  logSearchTerm pat
  sinfo <- getUserSessionInfo
  let t = translate sinfo
      pattern = "%" ++ filter (`notElem` "%_") pat ++ "%"
  mods <- runQ $
           ``sql* Select *
                  From ModData as md
                  Where md.Code like {pattern} Or md.NameG like {pattern}
                                               Or md.NameE like {pattern};''
  let vismods = maybe (filter modDataVisible mods) (const mods)
                      (userLoginOfSession sinfo)
  listCategoryController sinfo
    (Right [htxt $ t "Found modules"])
    [(Right $ "..." ++ t "with pattern" ++ ": " ++ pat, vismods)] []


-- simple generic string pattern matcher:
match :: Eq a => [a] -> [a] -> Bool
match pattern string = loop pattern string pattern string
  where
    loop []     _      _  _  = True
    loop (_:_)  []     _  _  = False
    loop (p:ps) (s:ss) op os = if p==s then loop ps ss op os
                                       else next op os

    next _  [] = False
    next op (_:ss) = loop op ss op ss


--- Controller to select a user in order to list all modules of this user.
selectUserModulesController :: Controller
selectUserModulesController = do
  sinfo <- getUserSessionInfo
  return $ selectUserView sinfo (formElem selectUserModulesForm)

selectUserModulesForm :: HtmlFormDef (UserSessionInfo, [User])
selectUserModulesForm = formDefWithID "Controller.Search.selectUserModulesForm"
  readData (selectUserFormView searchUserModules)
 where
  readData = toFormReader $ do
    sinfo <- getUserSessionInfo
    allUsers <- runQ queryAllUsers
    return (sinfo, sortBy leqUser allUsers)

--- Controller to list all modules of a user.
searchUserModules :: User -> Controller
searchUserModules user = do
  sinfo <- getUserSessionInfo
  let t = translate sinfo
  mods <- runQ $ queryModDataOfUser (userKey user)
  listCategoryController sinfo
    (Right [htxt $ t "Modules of" ++ " " ++ userToShortView user])
    [(Right "", mods)] []

--- Controller to list all (visible) modules.
showAllModulesController :: Controller
showAllModulesController = do
  mods  <- runQ $ fmap (filter modDataVisible) queryAllModDatas
  showModulesController mods []

--- Controller to list all responsible persons of all modules.
showAllModuleResponsibleController :: Controller
showAllModuleResponsibleController = do
  mods  <- runQ $ fmap (filter modDataVisible) queryAllModDatas
  let respkeys = nub (map modDataUserResponsibleKey mods)
  respusers <- runJustT (mapM getUser respkeys)
  return (showAllModuleResponsibleView "Alle Modulverantwortlichen" respusers)

--- Controller to list all responsible persons of the modules
--- in the given semester.
showModSemResponsibleController :: (String,Int) -> Controller
showModSemResponsibleController sem = do
  semmodkeys <- runQ $ queryModKeysOfSem sem
  semmods    <- runJustT $ mapM getModData semmodkeys
  let respkeys = nub (map modDataUserResponsibleKey semmods)
  respusers <- runJustT (mapM getUser respkeys)
  return (showAllModuleResponsibleView
            ("Alle Modulverantwortlichen im " ++ showSemester sem) respusers)

--- Controller to list all (visible) modules taught in English.
showAllEnglishModulesController :: Controller
showAllEnglishModulesController = do
  mods  <- runQ $ fmap (filter modDataVisible) queryAllModDatas
  emods <- mapM checkEnglishMod mods
  showModulesController (concat emods) []
 where
  checkEnglishMod md = do
    moddesc <- runQ $ queryDescriptionOfMod (modDataKey md)
    return (maybe [] (\desc -> if modDescrLanguage desc == "Englisch"
                               then [md]
                               else [])
                     moddesc)

--- Controller to list given modules.
showModulesController :: [ModData] -> [(String,Int)] -> Controller
showModulesController mods semperiod = do
  sinfo <- getUserSessionInfo
  let t = translate sinfo
      (pmods,wmods) = partition isMandatoryModule mods
  listCategoryController sinfo
    (Right [htxt $ t "All modules"])
    [(Right (t "Mandatary modules" ++
             " (Informatik, Wirtschaftsinformatik, Nebenfach)"), pmods),
     (Right $ t "Further modules", wmods)] semperiod

isMandatoryModule :: ModData -> Bool
isMandatoryModule md = modDataCode md `elem` mandatoryModulCodes

mandatoryModulCodes :: [String]
mandatoryModulCodes =
  ["Inf-ADS","Inf-BSKS","Inf-CompSys","Inf-EinfFP","Inf-FortProgP","Inf-IS",
   "Inf-ITSec5","Inf-LogInf","Inf-Math-A","Inf-Math-B","Inf-Math-C",
   "Inf-ProgOO","Inf-SP","Inf-ST","Inf-TGI",
   "WInf-EinfWInf","WInf-BetrStan","WInf-EinfOR",
   "Inf-InfRecht","Inf-DatSchutz",
   "Inf-FD-DiProg", "Inf-I1-2FNF", "Inf-I2-2F", "Inf-SP-2F",
   "infADS2F-01a", "Inf-FPKonz", "Inf-ProgTech" ]


--- Controller to show all modules in the given semester.
showSemModsController :: (String,Int) -> Controller
showSemModsController sem = do
  semmodkeys <- runQ $ queryModKeysOfSem sem
  semmods    <- runJustT $ mapM getModData semmodkeys
  showModulesController semmods [sem]

--- Controller to show all examination requirements of the modules
--- in the given semester.
showExamController :: (String,Int) -> Controller
showExamController sem = do
  semmodkeys <- runQ $ queryModKeysOfSem sem
  semmods    <- runJustT $ mapM getModData semmodkeys
  semexams   <- runJustT $ mapM queryExamOfMod semmodkeys
  return $ showExamOverview sem
              (map (\ (x,Just y) -> (x,y))
                   (filter (\ (m,e) -> modDataVisible m && isJust e)
                           (zip semmods semexams)))

--- Controller to show all modules in the given semester.
showHandbookController :: (String,Int) -> Controller
showHandbookController sem = do
  modkeys <- runQ $ queryModKeysOfSem sem
  mods    <- runJustT $ mapM getModData modkeys
  formatCatModulesForm [("Alle Module im " ++ showLongSemester sem, mods)]

--- Gets the ModData keys of all module instances in a given semester.
queryModKeysOfSem :: (String,Int) -> DBAction [ModDataID]
queryModKeysOfSem (term,year) =
  ``sql* Select Distinct mi.ModDataModuleInstancesKey
         From   ModInst As mi
         Where  mi.Term = {term} And mi.Year = {year};''

-----------------------------------------------------------------------------
