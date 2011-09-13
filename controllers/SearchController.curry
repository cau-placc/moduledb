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
import SearchView
import ModDataView
import Helpers

-----------------------------------------------------------------------------
--- Controller for the main page.
searchController :: Controller
searchController = return (searchPageView searchModules showPlanController
                                          showExamController)

--- Controller for searching modules
searchModules :: String -> Controller
searchModules pat = do
    admin <- isAdmin
    login <- getSessionLogin
    modcodes <- runQ $ transformQ (filter isMatching) queryModDataCodeName
    mods <- runJustT $ mapT (\ (k,_,_) -> getModData k) modcodes
    return (listModDataView admin "Gefundene Module:"
                (maybe (filter modDataVisible mods) (const mods) login)
                showModDataController editModDataController
                deleteModDataController)
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


--- Controller to list all modules together with their instances
--- in the given period.
showPlanController :: (String,Int) -> (String,Int) -> Controller
showPlanController startsem stopsem = do
  users <- runQ queryAllUsers
  mods  <- runQ $ transformQ (filter modDataVisible) queryAllModDatas
  let (pmods,wmods) = partition isMandatoryModule mods
  catmods <- runJustT $ mapT (\ (c,mds) -> mapT getModInsts mds |>>= \mmis ->
                                            returnT (c,mmis))
                           [("Pflichtmodule (Informatik und Nebenfach)",pmods),
                            ("Weitere Module",wmods)]
  return (showModulePlanView catmods (map showSemester semPeriod) users)
 where
   getModInsts md =
     getDB (queryInstancesOfMod (modDataKey md)) |>>= \mis ->
     returnT (md,map (instOfSem mis) semPeriod)

   instOfSem mis sem =
     find (\mi -> (modInstTerm mi,modInstYear mi) == sem) mis

   semPeriod = takeWhile (\s -> leqSemester s stopsem)
                         (iterate nextSemester startsem)

isMandatoryModule :: ModData -> Bool
isMandatoryModule md = modDataCode md `elem` mandatoryModulCodes

mandatoryModulCodes =
  ["Inf-ADS","Inf-BS","Inf-DigiSys","Inf-EinfPP","Inf-FortProg","Inf-HWP",
   "Inf-IS",
   "Inf-Ing","Inf-KomSys","Inf-LogInf","Inf-Math-A","Inf-Math-B","Inf-Math-C",
   "Inf-OAR","Inf-PP","Inf-Prog","Inf-SP","Inf-SWT","Inf-TGI",
   "Math-Num",
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
