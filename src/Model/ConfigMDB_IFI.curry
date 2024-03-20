-------------------------------------------------------------------------------
-- Global configurations for the module data base
-------------------------------------------------------------------------------

module Model.ConfigMDB
  ( adminEmail, getBaseURL, getExamreqsURL, getStorageDir
  , studyPlannerURL, systemHashKey
  )
 where

-- Email address of administrator:
adminEmail :: String
adminEmail = "mh@informatik.uni-kiel.de"

-- The name of the main script of the module system.
baseCGI :: String
baseCGI = "show.cgi"

-- Gets the URL of the main page of the module system (without the script part).
getBasePage :: IO String
getBasePage = return "https://moduldb.informatik.uni-kiel.de/"

-- Returns the URL of the main script of the module system
-- (used to generate external URLs for modules and master programs):
getBaseURL :: IO String
getBaseURL = fmap (++ baseCGI) getBasePage

-- Returns the URL of the main script of the module system
-- (used to generate external URLs for modules and master programs):
getExamreqsURL :: IO String
getExamreqsURL = fmap (++ "examreqs/") getBasePage

-- Returns the directory where all data is stored:
getStorageDir :: IO String
getStorageDir = return "/var/www/mdb/mdb/"

--- The base URL of the study planner
studyPlannerURL :: String
studyPlannerURL = "https://www-ps.informatik.uni-kiel.de/studienplaner/"

--- The system hash key used to encode passwords
--- (compare `System.Authentication`).
systemHashKey :: String
systemHashKey = "4caumdb2" -- change this key for every spicey instance

