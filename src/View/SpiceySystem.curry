--------------------------------------------------------------------------
--- This module implements the views related to the standard controllers
--- in a Spicey application.
--- In particular, it defines a default view for login
--- and a view of a list of user processes.
--------------------------------------------------------------------------

module View.SpiceySystem
  ( loginView, processListView, historyView )
 where

import Database.CDBI.Connection
import HTML.Base

import Config.UserProcesses
import System.Processes
import System.Spicey
import System.SessionInfo

-----------------------------------------------------------------------------
--- View for login/logout. If the passed login name is the empty string,
--- we offer a login dialog, otherwise a logout dialog.
loginView :: Controller -> UserSessionInfo -> [BaseHtml]
loginView _ _ = [h3 [htxt "Illegal URL"]]

-----------------------------------------------------------------------------
--- A view for all processes contained in a given process specification.
processListView :: Processes a -> [BaseHtml]
processListView procs =
  [h1 [htxt "Processes"],
   ulist (map processColumn (zip (processNames procs) [1..]))]
 where
   processColumn (pname, id) =
     [href ("?spiceyProcesses/"++show id) [htxt pname]]

-----------------------------------------------------------------------------
--- A view for all URLs of a session.
historyView :: [String] -> [BaseHtml]
historyView urls =
  [h1 [htxt "History"],
   ulist (map (\url -> [href ("?"++url) [htxt url]])
              (filter (not . null) urls))]

-----------------------------------------------------------------------------
