--------------------------------------------------------------------------
--- This module contains the controller for the main page of this application.
--------------------------------------------------------------------------

module MainController(mainPageController)
 where

import Spicey
import MainView
import MDB
import KeyDatabase
import SessionInfo

-----------------------------------------------------------------------------
--- Controller for the main page.
mainPageController :: Controller
mainPageController = do
  sinfo <- getUserSessionInfo
  studyPrograms <- runQ queryAllStudyPrograms
  return (mainPageView sinfo studyPrograms)

-----------------------------------------------------------------------------
