module Controller.User
  ( userController, newUserWuiForm, editUserWuiForm
  , loginForm, sendLoginDataForm, changePasswordForm )
 where

import Data.Time
import Config.EntityRoutes
import System.Helpers
import System.Spicey
import HTML.Base
import HTML.Session
import HTML.WUI
import MDB
import View.User
import System.Authorization
import System.AuthorizedActions
import System.MultiLang
import System.SessionInfo
import Config.UserProcesses
import System.Authentication
import Controller.Search
import View.MDBEntitiesToHtml ---!!!!!NEW!!!!

--- Choose the controller for a User entity according to the URL parameter.
userController :: Controller
userController = do
  args <- getControllerParams
  case args of
    ["list"]   -> listUserController
    ["new"]    -> newUserController
    ["login"]  -> loginController
    ["logout"] -> logoutController
    ["sendlogin"] -> sendLoginDataController
    ["passwd"] -> changePasswordController
    ["show",keystr] -> controllerOnKey keystr showUserController
    ["edit",keystr] -> controllerOnKey keystr editUserController
    ["delete",keystr] -> controllerOnKey keystr askAndDeleteUserController
    ["destroy",keystr] -> controllerOnKey keystr destroyUserController
    ["loginAs",keystr] -> controllerOnKey keystr loginAsUserController
    ["modules",keystr] -> controllerOnKey keystr searchUserModules
    _ -> displayUrlError

-----------------------------------------------------------------------
--- Shows a form to create a new User entity.
newUserController :: Controller
newUserController =
  checkAuthorization (userOperationAllowed NewEntity) $ \sinfo -> do
    ctime <- getClockTime
    setParWuiStore wuiNewUserWuiStore sinfo ("","","","","","","",ctime)
    return [formElem newUserWuiForm]

type NewUser = (String,String,String,String,String,String,String,ClockTime)

--- A WUI form to create a new User entity.
--- The default values for the fields are stored in the
--- `wuiNewUserWuiStore`.
newUserWuiForm :: HtmlFormDef (UserSessionInfo, WuiStore NewUser)
newUserWuiForm =
  pwui2FormDef "Controller.User.newUserWuiForm"
    wuiNewUserWuiStore
    (\_ -> wUser)
    (\_ entity -> checkAuthorization (userOperationAllowed NewEntity) $ \_ ->
                  createUserController entity)
    (\sinfo -> renderWUI sinfo "Neuen Benutzer anlegen" "Anlegen"
                         "?" ())

---- The data stored for executing the WUI form.
wuiNewUserWuiStore :: SessionStore (UserSessionInfo, WuiStore NewUser)
wuiNewUserWuiStore = sessionStore "wuiNewUserWuiStore"


--- Persists a new User entity to the database.
createUserController
 :: (String,String,String,String,String,String,String,ClockTime)
  -> Controller
createUserController
  (login ,name ,first ,title ,email ,url ,password, lastLogin) = do
  transResult <- runT (newUser login name first title email url password
                               lastLogin)
  either (\ error -> displayError (showTError error))
         (\ _ -> nextInProcessOr listUserController Nothing)
         transResult

-----------------------------------------------------------------------
--- Shows a form to edit the given User entity.
editUserController :: User -> Controller
editUserController user =
  checkAuthorization (userOperationAllowed (UpdateEntity user)) $ \sinfo -> do
    setParWuiStore wuiEditUserWuiStore (sinfo,user) user
    return [formElem editUserWuiForm]

--- A WUI form to create a edit User entity.
--- The default values for the fields are stored in the
--- `wuiEditUserWuiStore`.
editUserWuiForm :: HtmlFormDef ((UserSessionInfo,User), WuiStore User)
editUserWuiForm =
  pwui2FormDef "Controller.User.editUserWuiForm"
    wuiEditUserWuiStore
    (\ (_,user) -> wUserType user)
    (\_ user ->
       checkAuthorization (userOperationAllowed (UpdateEntity user)) $ \_ ->
         updateUserController user)
    (\ (sinfo,user) ->
          renderWUI sinfo "Benutzerdaten ändern" "Store"
                    ("?User/show/" ++ showUserKey user) ())

---- The data stored for executing the WUI form.
wuiEditUserWuiStore :: SessionStore ((UserSessionInfo,User), WuiStore User)
wuiEditUserWuiStore = sessionStore "wuiEditUserWuiStore"


--- Persists modifications of a given User entity to the
--- database depending on the Boolean argument. If the Boolean argument
--- is False, nothing is changed.
updateUserController :: User -> Controller
updateUserController user = do
  transResult <- runT (updateUser user)
  either (\ error -> displayError (showTError error))
         (\ _ -> nextInProcessOr (showUserController user) Nothing)
         transResult

-----------------------------------------------------------------------
--- Deletes a given User entity (after asking for acknowledgment)
--- and proceeds with the show controller.
askAndDeleteUserController :: User -> Controller
askAndDeleteUserController user =
  checkAuthorization (userOperationAllowed (DeleteEntity user)) $ \si ->
  confirmDeletionPage si $ concat
    ["Benutzer \"", userToShortView user, "\" wirklich löschen?"]

--- Deletes a given User entity and proceeds with the list controller.
destroyUserController :: User -> Controller
destroyUserController user =
  checkAuthorization (userOperationAllowed (DeleteEntity user)) $ \_ ->
   (runT (deleteUser user) >>=
    either (\ error -> displayError (showTError error))
           (\ _ -> listUserController))

-----------------------------------------------------------------------
--- Login as a given User entity.
loginAsUserController :: User -> Controller
loginAsUserController user =
  checkAuthorization checkAdmin $ \_ -> do
    let loginname = userLogin user
    loginToSession loginname
    setPageMessage ("Angemeldet als: "++loginname)
    redirectToDefaultController

--- Send login data to a user.
sendLoginDataController :: Controller
sendLoginDataController = do
  sinfo <- getUserSessionInfo
  return $ sendLoginDataView sinfo (formElem sendLoginDataForm)

--- A form to login to the system.
sendLoginDataForm :: HtmlFormDef UserSessionInfo
sendLoginDataForm =
  formDefWithID "Controller.User.sendLoginDataForm"
    (toFormReader $ getUserSessionInfo)
    (sendLoginDataFormView redirectToDefaultController)

--- Login to the system.
loginController :: Controller
loginController = do
  sinfo <- getUserSessionInfo
  case userLoginOfSession sinfo of
    Just _  -> return [h3 [htxt $ "Operation not allowed!"]]
    Nothing -> return $ loginView sinfo (formElem loginForm)

--- A form to login to the system.
loginForm :: HtmlFormDef UserSessionInfo
loginForm =
  formDefWithID "Controller.User.loginForm"
    (toFormReader $ getUserSessionInfo)
    (loginFormView redirectToDefaultController)

--- Logout the current user.
logoutController :: Controller
logoutController = do
  sinfo <- getUserSessionInfo
  let t = translate sinfo
  case userLoginOfSession sinfo of
    Nothing -> return [h3 [htxt $ "Operation not allowed!"]]
    Just _  -> do logoutFromSession
                  setPageMessage (t "Logged out")
                  redirectToDefaultController

--- Change password of logged in user.
changePasswordController :: Controller
changePasswordController =
  return [formElem changePasswordForm]

--- A form to change the password of a logged in user.
changePasswordForm :: HtmlFormDef UserSessionInfo
changePasswordForm =
  formDefWithID "Controller.User.changePasswordForm"
    (toFormReader $ getUserSessionInfo)
    (changePasswordFormView redirectToDefaultController)

--- Lists all User entities.
listUserController :: Controller
listUserController =
  checkAuthorization (userOperationAllowed ListEntities) $ \_ -> do
    runQ queryAllUsers >>= return . listUserView

--- Shows a User entity.
showUserController :: User -> Controller
showUserController user =
  checkAuthorization (userOperationAllowed (ShowEntity user)) $ \_ ->
   (do return (showUserView user))
