--------------------------------------------------------------------------
--- This module contains the controller for the main page of this application.
--------------------------------------------------------------------------

module Controller.Main
  (mainPageController)
 where

import System.Spicey
import View.Main
import Model.ConfigMDB ( getExamreqsURL )
import Model.MDB
import System.SessionInfo

-----------------------------------------------------------------------------
--- Controller for the main page.
mainPageController :: Controller
mainPageController = do
  sinfo <- getUserSessionInfo
  studyPrograms <- runQ queryAllStudyPrograms
  examreqsurl   <- getExamreqsURL
  return (mainPageView sinfo examreqsurl studyPrograms)

-----------------------------------------------------------------------------
