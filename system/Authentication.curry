----------------------------------------------------------------------------
--- This library contains operations to support the management of
--- user authentication. It provides operations for password
--- generation, password hashing (based on hashing algorithms from Unix),
--- and storing logins in sessions.
---
--- @author Michael Hanus
----------------------------------------------------------------------------

module Authentication (
  getUserHash, randomPassword,
  getSessionLogin, loginToSession, logoutFromSession,
  isAdmin
 ) where

import IO
import IOExts
import Session
import Global
import Crypto
import Distribution

--------------------------------------------------------------------------
-- Operations for hashing.

--- Gets a hash string for a user name and password. The generated
--- hash string should be stored for the user instead of the password.
getUserHash :: String -> String -> IO String
getUserHash username userpassword = do
  let systemkey = "4caumdb2" -- change this key for every spicey instance
  getHash (username++userpassword++systemkey)

--- Returns a random password (a hexadecimal string) of a particular length.
--- @param length - length of the desired password
--- @return the random password
randomPassword :: Int -> IO String
randomPassword = randomString

--------------------------------------------------------------------------
-- Operations for storing logins in the current session.

--- Definition of the session state to store the login name (as a string).
sessionLogin :: Global (SessionStore String)
sessionLogin = global emptySessionStore (Persistent "sessionLogin")

--- Gets the login name of the current session
--- (or the Nothing if there is no login).
getSessionLogin :: IO (Maybe String)
getSessionLogin = getSessionData sessionLogin

--- Stores a login name in the current session.
--- The authentication has to be done before!
loginToSession :: String -> IO ()
loginToSession loginname = putSessionData loginname sessionLogin

--- removes the login name from the current session.
logoutFromSession :: IO ()
logoutFromSession = removeSessionData sessionLogin

--------------------------------------------------------------------------
-- Returns true if admin is logged in
isAdmin :: IO Bool
isAdmin = getSessionLogin >>= return . maybe False (=="admin")

--------------------------------------------------------------------------
