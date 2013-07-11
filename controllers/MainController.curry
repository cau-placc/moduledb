--------------------------------------------------------------------------
--- This module contains the controller for the main page of this application.
--------------------------------------------------------------------------

module MainController(mainPageController)
 where

import Spicey
import MainView
import MDB
import KeyDatabase
import UserPreferences

-----------------------------------------------------------------------------
--- Controller for the main page.
mainPageController :: Controller
mainPageController = do
  userprefs <- getSessionUserPrefs
  studyPrograms <- runQ queryAllStudyPrograms
  return (mainPageView userprefs studyPrograms)

-----------------------------------------------------------------------------
