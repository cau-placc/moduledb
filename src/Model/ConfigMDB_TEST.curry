-------------------------------------------------------------------------------
-- Globale Konfigurationen fuer Moduldatenbank:
-------------------------------------------------------------------------------

module ConfigMDB
  ( adminEmail, baseURL, baseLoginURL, storageDir, studyPlannerURL, systemHashKey )
 where

-- Email address of administrator:
adminEmail :: String
adminEmail = "mh@informatik.uni-kiel.de"

-- The URL of the main script of the module system
-- (used to generate external URLs for modules and master programs):
baseURL :: String
baseURL = "http://localhost/~mh/mdbtest/show.cgi"

-- The URL of the main script of the module system
-- when a user wants login in (used during the transformation phase).
baseLoginURL :: String
--baseLoginURL = "http://localhost/~mh/mdbtest/pakcs/show.cgi"
baseLoginURL = "https://mdb.ps.informatik.uni-kiel.de/pakcs/show.cgi"

-- directory where all data is stored:
storageDir :: String
storageDir = "/net/medoc/home/mh/home/data/mdbtest/"

--- The base URL of the study planner
studyPlannerURL :: String
studyPlannerURL = "http://www-ps.informatik.uni-kiel.de/studienplaner/"

--- The system hash key used to encode passwords
--- (compare `System.Authentication`).
systemHashKey :: String
systemHashKey = "4caumdb2" -- change this key for every spicey instance

