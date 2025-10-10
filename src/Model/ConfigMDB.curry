-------------------------------------------------------------------------------
-- | Global configurations for the module data base.
-------------------------------------------------------------------------------

module Model.ConfigMDB
  ( isTestSystem, adminEmail, getBaseURL, getExamreqsURL, getStorageDir
  , systemHashKey, pdfDir
  )
 where

import System.Environment ( getEnv )

------------------------------------------------------------------------------
-- | Is the current installation a test system?
--   As a default, it is a test system unless the environment variable
--   `MDBTEST` has value `no`.
isTestSystem :: IO Bool
isTestSystem = do
  mdbtest <- getEnv "MDBTEST"
  return $ mdbtest /= "no"

------------------------------------------------------------------------------
-- | Email address of administrator:
adminEmail :: String
adminEmail = "mh@informatik.uni-kiel.de"

-- | Gets the URL of the main page of the module system
--   (without the script part).
getBasePage :: IO String
getBasePage = do
  testsystem <- isTestSystem
  return $ if testsystem then "http://localhost/~mh/mdbtest/"
                         else "https://moduldb.informatik.uni-kiel.de/"

-- | Returns the URL of the main script of the module system
--   (used to generate external URLs for modules and master programs):
getBaseURL :: IO String
getBaseURL = fmap (++ "show.cgi") getBasePage

-- | Returns the URL of the main script of the module system
--   (used to generate external URLs for modules and master programs):
getExamreqsURL :: IO String
getExamreqsURL = fmap (++ "examreqs/") getBasePage

-- | Returns the directory where all data is stored:
getStorageDir :: IO String
getStorageDir = return "../mdbData"

-- | The system hash key used to encode passwords
--   (compare `System.Authentication`).
systemHashKey :: String
systemHashKey = "4caumdb2" -- change this key for every spicey instance

-- | Directory to store generated PDFs.
pdfDir :: FilePath
pdfDir = "pdfs"

------------------------------------------------------------------------------
