--------------------------------------------------------------------------
--- This module contains some controller that might be used in in
--- Spicey application.
--- In particular, it provides a controller for login/out and
--- a controller to start selected user processes.
--------------------------------------------------------------------------

module Controller.SpiceySystem
  ( loginController, processListController, historyController )
 where

import Numeric
import System.Spicey
import HTML.Session
import Config.UserProcesses
import System.Processes
import View.SpiceySystem
import System.Authentication
import System.SessionInfo

-----------------------------------------------------------------------------
--- Controller for login/logout.
loginController :: Controller
loginController = do
  sinfo <- getUserSessionInfo
  return $ loginView redirectToDefaultController sinfo

-----------------------------------------------------------------------------
--- Controller for showing and selecting user processes.
processListController :: Controller
processListController = do
  args <- getControllerParams
  if null args
   then return $ processListView availableProcesses
   else case (readInt (head args)) of
          [(idInt, _)] -> startProcess
                            (processNames availableProcesses !! (idInt - 1))
          _            -> displayError "could not read process id"

-----------------------------------------------------------------------------
--- Controller for the URL history.
historyController :: Controller
historyController = getLastUrls >>= return . historyView

-----------------------------------------------------------------------------
