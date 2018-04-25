--------------------------------------------------------------------------
--- This module contains a controller for search modules.
--------------------------------------------------------------------------
{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=foreigncode #-}

module Controller.Search
  ( searchController, searchUserModules )
 where

import Sort (mergeSortBy)
import System.Spicey
import System.Authentication
import MDB
import MDBExts
import Char
import List
import Maybe
import Database.CDBI.ER 

import Controller.ModData
import Controller.Category
import View.Category
import View.Search
import View.ModData
import View.User (leqUser)
import View.MDBEntitiesToHtml
import System.Helpers
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
            csem  <- getCurrentSemester
            return $ searchPageView sinfo csem searchModules
                                    showSemModsController showExamController
                                    showModSemResponsibleController
                                    showHandbookController

--- Controller for searching modules in the module database.
searchModules :: String -> Controller
searchModules pat = do
  sinfo <- getUserSessionInfo
  csem  <- getCurrentSemester
  let t = translate sinfo
      pattern = "%" ++ filter (`notElem` "%_") pat ++ "%"
  mods <- runQ $
           ``sql* Select *
                  From ModData as md
                  Where md.Code like {pattern} Or md.NameG like {pattern}
                                               Or md.NameE like {pattern};''
  let vismods = maybe (filter modDataVisible mods) (const mods)
                      (userLoginOfSession sinfo)
  return (listCategoryView sinfo csem
            (Right [htxt $ t "Found modules"])
            [(Right $ "..." ++ t "with pattern" ++ ": " ++ pat,
              map (\m->(m,[],[])) vismods)]
            [] []
            showCategoryPlanController formatCatModulesForm
            showEmailCorrectionController)


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
  allUsers <- runQ queryAllUsers
  return (selectUserView sinfo (mergeSortBy leqUser allUsers) searchUserModules)


--- Controller to list all modules of a user.
searchUserModules :: User -> Controller
searchUserModules user = do
  sinfo <- getUserSessionInfo
  csem  <- getCurrentSemester
  let t = translate sinfo
  mods <- runQ $ queryModDataOfUser (userKey user)
  return (listCategoryView sinfo csem
               (Right [htxt $ t "Modules of" ++ " " ++ userToShortView user])
               [(Right "",map (\m->(m,[],[])) mods)]
               [] []
               showCategoryPlanController
               formatCatModulesForm showEmailCorrectionController)


--- Controller to list all (visible) modules.
showAllModulesController :: Controller
showAllModulesController = do
  mods  <- runQ $ liftM (filter modDataVisible) queryAllModDatas
  showModulesController mods

--- Controller to list all responsible persons of all modules.
showAllModuleResponsibleController :: Controller
showAllModuleResponsibleController = do
  mods  <- runQ $ liftM (filter modDataVisible) queryAllModDatas
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
  mods  <- runQ $ liftM (filter modDataVisible) queryAllModDatas
  emods <- mapIO checkEnglishMod mods
  showModulesController (concat emods)
 where
  checkEnglishMod md = do
    moddesc <- runQ $ queryDescriptionOfMod (modDataKey md)
    return (maybe [] (\desc -> if modDescrLanguage desc == "Englisch"
                               then [md]
                               else [])
                     moddesc)

--- Controller to list given modules.
showModulesController :: [ModData] -> Controller
showModulesController mods = do
  sinfo <- getUserSessionInfo
  csem  <- getCurrentSemester
  let t = translate sinfo
      (pmods,wmods) = partition isMandatoryModule mods
  return (listCategoryView sinfo csem
            (Right [htxt $ t "All modules"])
            [(Right (t "Mandatary modules" ++
                     " (Informatik, Wirtschaftsinformatik, Nebenfach)"),
              map (\m->(m,[],[])) pmods),
             (Right $ t "Further modules", map (\m->(m,[],[])) wmods)]
            [] []
            showCategoryPlanController formatCatModulesForm
            showEmailCorrectionController)
 where

isMandatoryModule :: ModData -> Bool
isMandatoryModule md = modDataCode md `elem` mandatoryModulCodes

mandatoryModulCodes :: [String]
mandatoryModulCodes =
  ["Inf-ADS","Inf-BS","Inf-DigiSys","Inf-EinfPP","Inf-FortProg","Inf-HWP",
   "Inf-IS",
   "Inf-Ing","Inf-KomSys","Inf-LogInf","Inf-Math-A","Inf-Math-B","Inf-Math-C",
   "Inf-OAR","Inf-PP","Inf-Prog","Inf-SP","Inf-SWT","Inf-TGI",
   "WInf-WInf1","WInf-WInf2","WInf-WInf3","WInf-BetrStan","WInf-ModIS",
   "Inf-InfRecht","Inf-DatSchutz",
   "Math-Num","Inf-IngNum","Inf-NumMeth1","Inf-NumMeth2",
   "Inf-SecCom", -- for Digital Communications
   "Inf-InfNat","NF-Inf-1","NF-Inf-1v","NF-Inf-2"]


--- Controller to show all modules in the given semester.
showSemModsController :: (String,Int) -> Controller
showSemModsController sem = do
  semmodkeys <- runQ $ queryModKeysOfSem sem
  semmods    <- runJustT $ mapM getModData semmodkeys
  showModulesController semmods

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
