-------------------------------------------------------------------------------
-- Global configurations for the module data base
-------------------------------------------------------------------------------

module ConfigMDB
  ( adminEmail, baseCGI, baseURL, storageDir
  , studyPlannerURL, systemHashKey
  )
 where

import System.FilePath ( (</>) )

-- Email address of administrator:
adminEmail :: String
adminEmail = "mh@informatik.uni-kiel.de"

-- The name of the main script of the module system.
baseCGI :: String
baseCGI = "show.cgi"

-- The URL of the main script of the module system
-- (used to generate external URLs for modules and master programs):
baseURL :: String
--baseURL = "https://mdb.ps.informatik.uni-kiel.de/" ++ baseCGI
baseURL = "https://moduldb.informatik.uni-kiel.de/" ++ baseCGI

-- Directory where all data is stored:
storageDir :: String
--storageDir = "/srv/sites/ps.informatik.uni-kiel.de/mdb/mdb/"
storageDir = "/var/www/mdb/mdb/"

--- The base URL of the study planner
studyPlannerURL :: String
studyPlannerURL = "https://www-ps.informatik.uni-kiel.de/studienplaner/"

--- The system hash key used to encode passwords
--- (compare `System.Authentication`).
systemHashKey :: String
systemHashKey = "4caumdb2" -- change this key for every spicey instance

