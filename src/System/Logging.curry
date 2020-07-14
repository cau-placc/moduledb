--------------------------------------------------------------------------
--- This module contains operations to log some data.
--------------------------------------------------------------------------
{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=foreigncode #-}

module System.Logging
 where

import IOExts   ( exclusiveIO )
import FilePath ( (</>) )
import System   ( getEnviron )
import Time

import ConfigMDB
import Data.Format -- required in the pre-processed program

--------------------------------------------------------------------------
--- Writes a search term into a log file.
logSearchTerm :: String -> IO ()
logSearchTerm = logEntity (storageDir </> "SEARCH.log")

--- Writes a URL parameter into a log file.
logUrlParameter :: String -> IO ()
logUrlParameter = logEntity (storageDir </> "URLPARAMS.log")

--------------------------------------------------------------------------
--- Writes some entity with date and remote host info into a log file.
logEntity :: Show a => String -> a -> IO ()
logEntity logfile entity = do
  ltime <- getLocalTime
  raddr <- getEnviron "REMOTE_ADDR"
  -- Usually, appendFile should not be subject to race conditions.
  -- In practice, it happened in the written log files so that
  -- we write the file exclusively:
  exclusiveIO (logfile ++ ".lock") $
    appendFile logfile (show (ltime, entity, raddr) ++ "\n")

--------------------------------------------------------------------------
--- Reads the contents of a log file with information written by `logEntity`.
--- The read is safe, i.e., unreadable lines are ignored.
readLogFile :: Read a => String -> IO [a]
readLogFile fname = do
  cnt <- readFile fname
  xss <- mapM safeRead (zip [1..] (lines cnt))
  return $ concat xss
 where
  safeRead (n,s) = do
    catch (return $!! ((:[]) (read s)))
          (\_ -> putStrLn ("READ ERROR IN LINE " ++ show n ++ ": " ++ s) >>
                 return [])

--- Reads and shows the contents of a log file with information
--- written by `logEntity` where the entity is a string.
showLogFile :: String -> IO ()
showLogFile fname =
  readLogFile fname >>= putStrLn . unlines . map showLogLine

showLogLine :: (CalendarTime, String, String) -> String
showLogLine (ct,pat,ip) =
  ``format "%-20s: %-35.35s (%s)", calendarTimeToString ct, show pat, ip''

--------------------------------------------------------------------------
