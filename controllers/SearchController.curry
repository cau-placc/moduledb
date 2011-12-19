--------------------------------------------------------------------------
--- This module contains a controller for search modules.
--------------------------------------------------------------------------

module SearchController(searchController)
 where

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
import Helpers

-----------------------------------------------------------------------------
--- Controller for the main page.
searchController :: Controller
searchController = do
  login <- getSessionLogin
  return (searchPageView login searchModules showExamController
                         showAllModulesController)

--- Controller for searching modules
searchModules :: String -> Controller
searchModules pat = do
    admin <- isAdmin
    login <- getSessionLogin
    modcodes <- runQ $ transformQ (filter isMatching) queryModDataCodeName
    mods <- runJustT $ mapT (\ (k,_,_) -> getModData k) modcodes
    let vismods = maybe (filter modDataVisible mods) (const mods) login
    return (listCategoryView admin login (Right "Gefundene Module")
              [(Right $ "...mit Muster: "++pat, map (\m->(m,[],[])) vismods)]
              [] [] showCategoryController
              editCategoryController deleteCategoryController
              showCategoryPlanController formatModulesForm)
 where
   isMatching (_,code,name) = match pat (map toLower code) ||
                              match pat (map toLower name)


-- simple generic string pattern matcher:
match pattern string = loop pattern string pattern string
  where
    loop []     _      _  _  = True
    loop (_:_)  []     _  _  = False
    loop (p:ps) (s:ss) op os = if p==s then loop ps ss op os
                                       else next op os

    next _  [] = False
    next op (_:ss) = loop op ss op ss


--- Controller to list all (visible) modules.
showAllModulesController :: Controller
showAllModulesController = do
  admin <- isAdmin
  login <- getSessionLogin
  mods  <- runQ $ transformQ (filter modDataVisible) queryAllModDatas
  let (pmods,wmods) = partition isMandatoryModule mods
  return (listCategoryView admin login
            (Right "Alle Module")
            [(Right "Pflichtmodule (Informatik und Nebenfach)",
              map (\m->(m,[],[])) pmods),
             (Right "Weitere Module", map (\m->(m,[],[])) wmods)]
            [] [] showCategoryController
            editCategoryController deleteCategoryController
            showCategoryPlanController formatModulesForm)

isMandatoryModule :: ModData -> Bool
isMandatoryModule md = modDataCode md `elem` mandatoryModulCodes

mandatoryModulCodes =
  ["Inf-ADS","Inf-BS","Inf-DigiSys","Inf-EinfPP","Inf-FortProg","Inf-HWP",
   "Inf-IS",
   "Inf-Ing","Inf-KomSys","Inf-LogInf","Inf-Math-A","Inf-Math-B","Inf-Math-C",
   "Inf-OAR","Inf-PP","Inf-Prog","Inf-SP","Inf-SWT","Inf-TGI",
   "Math-Num","Inf-IngNum","Inf-NumMeth1","Inf-NumMeth2",
   "NF-Inf-1","NF-Inf-1v","NF-Inf-2","NF-Inf-2-Phys"]


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
