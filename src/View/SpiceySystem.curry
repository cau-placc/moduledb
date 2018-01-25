--------------------------------------------------------------------------
--- This module implements the views related to the standard controllers
--- in a Spicey application.
--- In particular, it defines a default view for login
--- and a view of a list of user processes.
--------------------------------------------------------------------------

module View.SpiceySystem(loginView,processListView,historyView)
 where

import Config.UserProcesses
import System.Processes
import System.Spicey
import System.Authentication
import System.Transaction
import MDB
import Controller.Default ( defaultController )
import Mail
import Time
import ConfigMDB
import System.SessionInfo
import System.MultiLang

-----------------------------------------------------------------------------
--- View for login/logout. If the passed login name is the empty string,
--- we offer a login dialog, otherwise a logout dialog.
loginView :: Controller -> UserSessionInfo -> [HtmlExp]
loginView controller sinfo =
  case userLoginOfSession sinfo of
   Nothing -> [h3 [htxt $ t "Login to module database"],
               spTable [[[htxt $ t "Login name:"], [textfield loginfield ""]],
                        [[htxt $ t "Password:"],   [password passfield]]],
               hrule,
               par [spPrimButton (t "Login") loginHandler],
               hrule,
               par [spButton (t "Forgot your login data?")
                             (const sendLoginDataForm)]]
   Just _  -> [h3 [htxt $ t "Really logout?"],
               par [spPrimButton (t "Logout")  (logoutHandler True),
                    spButton (t "Cancel") (logoutHandler False)],
               hrule,
               h3 [htxt $ t "You can also ",
                   spButton (t "change your password") (const passwdForm)]]
 where
  loginfield,passfield free

  t = translate sinfo

  loginHandler env = do
    let loginname = env loginfield
    hashpass <- getUserHash loginname (env passfield)
    users <- runQ $ queryUserByLoginPass loginname hashpass
    if null users
      then do setPageMessage $ t "Wrong login data!"
              nextInProcessOr controller Nothing >>= getForm
      else do loginToSession loginname
              ctime <- getClockTime
              runT (updateUser (setUserLastLogin (head users) ctime))
              setPageMessage (t "Logged in as: " ++ loginname)
              urls <- getLastUrls
              getForm $
               [h1 [htxt $ t "Login successful"],
                par [htxt $ loginText sinfo loginname],
                par [htxt $ timeoutText sinfo]] ++
                if length urls > 1
                then [par [href ('?':urls!!1) [htxt "Back to last page"]]]
                else []

  logoutHandler confirm _ = do
    if confirm then logoutFromSession >> setPageMessage (t "Logged out")
               else cancelOperation
    nextInProcessOr controller Nothing >>= getForm

--- Gets a user entry with a given login name and (hashed) password:
queryUserByLoginPass :: String -> String -> Query [User]
queryUserByLoginPass login pass = seq login $ seq pass $
  queryCondUser (\u -> userLogin u == login && userPassword u == pass)

-----------------------------------------------------------------------------
-- a form to change the password
passwdForm :: IO HtmlForm
passwdForm = do
  sinfo <- getUserSessionInfo
  let t = translate sinfo
  getForm
    [h1 [htxt $ t "Change password"],
     spTable [[[htxt $ t "Old password:"],        [password oldpass]],
              [[htxt $ t "New password:"],        [password newpass1]],
              [[htxt $ t "Repeat new password:"], [password newpass2]]],
     hrule,
     spPrimButton (t "Change password")
                  (\ e -> passwdHandler t (userLoginOfSession sinfo) e
                                                             >>= getForm),
     nbsp,
     spButton (t "Cancel")
            (const (cancelOperation >> defaultController >>= getForm))]
 where
  oldpass,newpass1,newpass2 free

  passwdHandler _ Nothing _ = cancelOperation >> defaultController
  passwdHandler t (Just lname) env = do
    hashpass <- getUserHash lname (env oldpass)
    users <- runQ $ queryUserByLoginPass lname hashpass
    if null users
     then displayError $ t "Wrong password!"
     else do let u = head users
             newhashpass <- getUserHash lname (env newpass1)
             if env newpass1 /= env newpass2
              then displayError $ t "New passwords are different!"
              else do runT (updateUser (setUserPassword u newhashpass))
                      setPageMessage $ t "Passwort changed"
                      defaultController

------------------------------------------------------------------------
-- send login data to a new user
sendLoginDataForm :: IO HtmlForm
sendLoginDataForm = do
  sinfo <- getUserSessionInfo
  let t = translate sinfo
  getForm
    [h1 [htxt $ t "Send login data"],
     par [htxt $ sendPasswordCmt sinfo],
     par [htxt $ t "Your email address: ", textfield emailref ""],
     hrule,
     spPrimButton (t "Send new password") (sendHandler sinfo),
     spButton (t "Cancel")
            (const (cancelOperation >> defaultController >>= getForm))]
 where
  emailref free

  sendHandler sinfo env = do
    users <- runQ $ queryCondUser (\u -> userEmail u == env emailref)
    if null users
     then displayError (unknownUser sinfo) >>= getForm
     else sendNewEmail sinfo (head users)

  sendNewEmail sinfo user = do
    let t = translate sinfo
    newpass <- randomPassword 10
    hashpass <- getUserHash (userLogin user) newpass
    sendMail adminEmail
      (userEmail user)
      (t "Login data for module database")
      (loginEmailText sinfo (userLogin user) newpass)
    runT $ updateUser (setUserPassword user hashpass)
    getForm [h1 [htxt $ t "Acknowledgment"],
             h3 [htxt $ t "Your new password has been sent"]]

------------------------------------------------------------------------
--- A view for all processes contained in a given process specification.
processListView :: Processes a -> [HtmlExp]
processListView procs =
  [h1 [htxt "Processes"],
   ulist (map processColumn (zip (processNames procs) [1..]))]
 where
   processColumn (pname, id) =
     [href ("?spiceyProcesses/"++show id) [htxt pname]]

-----------------------------------------------------------------------------
--- A view for all URLs of a session.
historyView :: [String] -> [HtmlExp]
historyView urls =
  [h1 [htxt "History"],
   ulist (map (\url -> [href ("?"++url) [htxt url]])
              (filter (not . null) urls))]

-----------------------------------------------------------------------------
