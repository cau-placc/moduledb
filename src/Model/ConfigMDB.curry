-------------------------------------------------------------------------------
-- | Global configurations for the module data base.
-------------------------------------------------------------------------------

module Model.ConfigMDB
  ( isTestSystem, adminEmail, getBaseURL, getExamreqsURL, getStorageDir
  , systemHashKey, pdfDir, inPDFDir, ensureAndCleanPDFDir
  )
 where

import Control.Monad      ( unless, when )
import Data.List          ( isPrefixOf )
import System.Environment ( getEnv )

import Data.Time          ( addMinutes, getClockTime )
import System.Directory   ( createDirectory, doesDirectoryExist
                          , getDirectoryContents, getModificationTime
                          , removeFile )
import System.FilePath    ( (</>) )
import System.Process     ( system )

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
adminEmail = "moduldb@informatik.uni-kiel.de"

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
systemHashKey = "4caumdb2" -- change this key for every Spicey instance

-- | Directory to store generated PDFs.
pdfDir :: FilePath
pdfDir = "pdfs"

------------------------------------------------------------------------------
-- Operations to support mangagement of PDFs.

-- | Prefix a file name with the directory where PDFs are stored.
inPDFDir :: String -> String
inPDFDir filename = pdfDir </> filename

-- | Ensures that the directory to store PDF files exists.
--   If it does not exist, it will be created.
--   Furthermore, `tmp` files in this directory which are older than 60 minutes
--   are deleted.  
ensureAndCleanPDFDir :: IO ()
ensureAndCleanPDFDir = do
  exspd <- doesDirectoryExist pdfDir
  unless exspd $ createDirectory pdfDir
  system $ "chmod 775 " ++ pdfDir
  tmpfiles <- fmap (filter ("tmp" `isPrefixOf`)) $ getDirectoryContents pdfDir
  curtime <- getClockTime
  mapM_ (deleteIfOld curtime) (map (pdfDir </>) tmpfiles)
 where
  deleteIfOld curtime fn =
    catch (do mtime <- getModificationTime fn
              when (addMinutes 60 mtime < curtime) $ removeFile fn)
          (\_ -> return ())

------------------------------------------------------------------------------
