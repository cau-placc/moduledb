----------------------------------------------------------------------------
--- This module defines the data that is associated to a user session
--- and passed to a view so that views can be adapted to user sessions.
---
--- Currently, the session data contains information about the login
--- status of a user and the preferred language.
---
--- @author Michael Hanus
----------------------------------------------------------------------------

module System.SessionInfo (
  UserSessionInfo(..), userLoginOfSession, setUserLoginOfSession,
  isAdminSession,
  Language(..), languageOfSession, setLanguageOfSession,
  getUserSessionInfo, updateUserSessionInfo
 ) where

import System.Session
import Global

--------------------------------------------------------------------------
--- The languages which are currently supported.
data Language = German | English
 deriving Eq

--------------------------------------------------------------------------
--- The data associated to a user session.
--- It contains formation about the login status of a user
--- (this argument of the session data is `Nothing` if the user is not logged
--- in, or `Maybe ln` where `ln` is the login name of the user)
--- and the language preferred in this sesion
data UserSessionInfo = SD (Maybe String) Language

--- The initial (empty) session data
emptySessionInfo :: UserSessionInfo
emptySessionInfo = SD Nothing German

--- Extracts the login status from the user session data.
userLoginOfSession :: UserSessionInfo -> Maybe String
userLoginOfSession (SD login _) = login

--- Sets the login status of the user session data.
setUserLoginOfSession :: Maybe String -> UserSessionInfo -> UserSessionInfo
setUserLoginOfSession login (SD _ lang) = SD login lang

--- Is the current session an administrator session?
isAdminSession :: UserSessionInfo -> Bool
isAdminSession sinfo =
  maybe False (\s -> take 5 s =="admin") (userLoginOfSession sinfo)

--- Extracts the preferred language from the user session data.
languageOfSession :: UserSessionInfo -> Language
languageOfSession (SD _ lang) = lang

--- Sets the perferred language of the user session data.
setLanguageOfSession :: Language -> UserSessionInfo -> UserSessionInfo
setLanguageOfSession lang (SD login _) = SD login lang

--------------------------------------------------------------------------
--- Definition of the session state to store the login name (as a string).
userSessionInfo :: Global (SessionStore UserSessionInfo)
userSessionInfo = global emptySessionStore (Persistent "userSessionInfo")

--- Gets the data of the current user session.
getUserSessionInfo :: IO UserSessionInfo
getUserSessionInfo =
  getSessionData userSessionInfo >>= return . maybe emptySessionInfo id

--- Updates the data of the current user session.
updateUserSessionInfo :: (UserSessionInfo -> UserSessionInfo) -> IO ()
updateUserSessionInfo upd = do
  sd <- getUserSessionInfo
  putSessionData (upd sd) userSessionInfo

--------------------------------------------------------------------------
