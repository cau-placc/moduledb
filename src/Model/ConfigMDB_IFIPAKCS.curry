-------------------------------------------------------------------------------
-- Global configurations for the module data base
-------------------------------------------------------------------------------

module ConfigMDB
  ( adminEmail, baseURL, storageDir, sessionDataDir
  , studyPlannerURL, systemHashKey
  )
 where

import FilePath ( (</>) )

-- Email address of administrator:
adminEmail :: String
adminEmail = "mh@informatik.uni-kiel.de"

-- The URL of the main script of the module system
-- (used to generate external URLs for modules and master programs):
baseURL :: String
baseURL = "https://mdb.ps.informatik.uni-kiel.de/pakcs/show.cgi"

-- Directory where all data is stored:
storageDir :: String
storageDir = "/srv/sites/ps.informatik.uni-kiel.de/mdb/mdb/"

-- Directory where global form data is stored during run time:
sessionDataDir :: String
sessionDataDir = storageDir </> "sessiondata_pakcs"

--- The base URL of the study planner
studyPlannerURL :: String
studyPlannerURL = "http://www-ps.informatik.uni-kiel.de/studienplaner/"

--- The system hash key used to encode passwords
--- (compare `System.Authentication`).
systemHashKey :: String
systemHashKey = "4caumdb2" -- change this key for every spicey instance

