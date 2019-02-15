----------------------------------------------------------------------------
--- This library contains operations to support the management of
--- user authentication. It provides operations for password
--- generation, password hashing (based on hashing algorithms from Unix),
--- and storing logins in sessions.
---
--- @author Michael Hanus
----------------------------------------------------------------------------

module System.Authentication (
  getUserHash, randomPassword,
  getSessionLogin, loginToSession, loginToStudentSession, logoutFromSession,
  isAdmin
 ) where

import ConfigMDB          ( systemHashKey )
import System.SessionInfo
import System.Crypto

--------------------------------------------------------------------------
-- Operations for hashing.

--- Gets a hash string for a user name and password. The generated
--- hash string should be stored for the user instead of the password.
getUserHash :: String -> String -> IO String
getUserHash username userpassword =
  getHash (username ++ userpassword ++ systemHashKey)

--- Returns a random password (a hexadecimal string) of a particular length.
--- @param length - length of the desired password
--- @return the random password
randomPassword :: Int -> IO String
randomPassword = randomString


--------------------------------------------------------------------------
-- Operations for storing logins in the current session.

--- Gets the login name of the current session
--- (or the Nothing if there is no login).
getSessionLogin :: IO (Maybe String)
getSessionLogin = getUserSessionInfo >>= return . userLoginOfSession

--- Stores a login name in the current session.
--- The authentication has to be done before!
loginToSession :: String -> IO ()
loginToSession loginname =
  updateUserSessionInfo (setUserLoginOfSession (Just loginname))

--- Stores a login name in the current session.
--- The authentication has to be done before!
loginToStudentSession :: String -> IO ()
loginToStudentSession loginname =
  updateUserSessionInfo (setStudentLoginOfSession (Just loginname))

--- removes the login name from the current session.
logoutFromSession :: IO ()
logoutFromSession = do
  updateUserSessionInfo (setUserLoginOfSession Nothing)
  updateUserSessionInfo (setStudentLoginOfSession Nothing)

--------------------------------------------------------------------------
-- Returns true if admin is logged in
isAdmin :: IO Bool
isAdmin = getUserSessionInfo >>= return . isAdminSession

--------------------------------------------------------------------------
