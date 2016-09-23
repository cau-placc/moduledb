--------------------------------------------------------------------------
--- This module contains a controller for search modules.
--------------------------------------------------------------------------

module SearchController(searchController,searchUserModules)
 where

import Sort (mergeSortBy)
import Spicey
import Authentication
import MDB
import KeyDatabase
import Char
import List
import Maybe
import ModDataController
import CategoryController
import CategoryView
import SearchView
import ModDataView
import UserView (leqUser)
import Helpers
import MDBEntitiesToHtml
import MultiLang
import SessionInfo

-----------------------------------------------------------------------------
--- Controller for the main page.
searchController :: Controller
searchController = do
  args <- getControllerParams
  case args of
    ["all"]      -> showAllModulesController
    ["usermods"] -> selectUserModulesController
    ["english"]  -> showAllEnglishModulesController
    _ -> do sinfo <- getUserSessionInfo
            return (searchPageView sinfo searchModules showExamController)

--- Controller for searching modules
searchModules :: String -> Controller
searchModules pat = do
  sinfo <- getUserSessionInfo
  let t = translate sinfo
  modcodes <- runQ $ transformQ (filter isMatching) queryModDataCodeName
  mods <- runJustT $ mapT (\ (k,_,_,_) -> getModData k) modcodes
  let vismods = maybe (filter modDataVisible mods) (const mods)
                      (userLoginOfSession sinfo)
  return (listCategoryView sinfo
            (Right [htxt $ t "Found modules"])
            [(Right $ "..." ++ t "with pattern" ++ ": " ++ pat,
              map (\m->(m,[],[])) vismods)]
            [] []
            showCategoryPlanController formatCatModulesForm
            showEmailCorrectionController)
 where
   isMatching (_,code,nameG,nameE) =
     match pat (map toLower code) ||
     match pat (map toLower nameG) ||
     match pat (map toLower nameE)


-- simple generic string pattern matcher:
match :: [a] -> [a] -> Bool
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
  let t = translate sinfo
  mods <- runQ $ queryModDataOfUser (userKey user)
  return (listCategoryView sinfo
               (Right [htxt $ t "Modules of" ++ " " ++ userToShortView user])
               [(Right "",map (\m->(m,[],[])) mods)]
               [] []
               showCategoryPlanController
               formatCatModulesForm showEmailCorrectionController)



--- Controller to list all (visible) modules.
showAllModulesController :: Controller
showAllModulesController = do
  mods  <- runQ $ transformQ (filter modDataVisible) queryAllModDatas
  showModulesController mods

--- Controller to list all (visible) modules taught in English.
showAllEnglishModulesController :: Controller
showAllEnglishModulesController = do
  mods  <- runQ $ transformQ (filter modDataVisible) queryAllModDatas
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
  let t = translate sinfo
      (pmods,wmods) = partition isMandatoryModule mods
  return (listCategoryView sinfo
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


--- Controller to show all examination requirement of the modules
--- in the given semester.
showExamController :: (String,Int) -> Controller
showExamController sem = do
  semmodkeys <- runQ $ transformQ nub (queryModKeysOfSem sem)
  semmods    <- runJustT $ mapT getModData semmodkeys
  semexams   <- runJustT $ mapT (getDB . queryExamOfMod) semmodkeys
  return $ showExamOverview sem
              (map (\ (x,Just y) -> (x,y))
                   (filter (\ (m,e) -> modDataVisible m && isJust e)
                           (zip semmods semexams)))

-----------------------------------------------------------------------------
