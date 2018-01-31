module Controller.User (userController) where

import System.Helpers
import System.Spicey
import HTML.Base
import Time
import MDB
import View.User
import Maybe
import System.Authorization
import System.AuthorizedActions
import Config.UserProcesses
import Controller.Default
import System.Authentication
import Controller.Search
import View.MDBEntitiesToHtml ---!!!!!NEW!!!!

--- Choose the controller for a User entity according to the URL parameter.
userController :: Controller
userController = do
  args <- getControllerParams
  case args of
    ["list"] -> listUserController
    ["new"]  -> newUserController
    ["show",keystr] ->
      applyControllerOn (readUserKey keystr) getUser showUserController
    ["edit",keystr] ->
      applyControllerOn (readUserKey keystr) getUser editUserController
    ["delete",keystr] ->
      applyControllerOn (readUserKey keystr) getUser askAndDeleteUserController
    ["login",keystr] ->
      applyControllerOn (readUserKey keystr) getUser loginUserController
    ["modules",keystr] ->
      applyControllerOn (readUserKey keystr) getUser searchUserModules
    _ -> displayError "Illegal URL"


--- Shows a form to create a new User entity.
newUserController :: Controller
newUserController =
  checkAuthorization (userOperationAllowed NewEntity) $ \_ ->
   (do ctime <- getClockTime
       return (blankUserView ctime createUserController))

--- Persists a new User entity to the database.
createUserController
 :: Bool -> (String,String,String,String,String,String,String,ClockTime)
  -> Controller
createUserController False _ = listUserController
createUserController True (login ,name ,first ,title ,email ,url ,password
                           ,lastLogin) =
  do transResult <- runT
                     (newUser login name first title email url password
                       lastLogin)
     flip either (\ _ -> nextInProcessOr listUserController Nothing)
      (\ error -> displayError (showTError error)) transResult

--- Shows a form to edit the given User entity.
editUserController :: User -> Controller
editUserController userToEdit =
  checkAuthorization (userOperationAllowed (UpdateEntity userToEdit)) $ \_ ->
   (do return (editUserView userToEdit updateUserController))

--- Persists modifications of a given User entity to the
--- database depending on the Boolean argument. If the Boolean argument
--- is False, nothing is changed.
updateUserController :: Bool -> User -> Controller
updateUserController False user = showUserController user
updateUserController True user =
  do transResult <- runT (updateUser user)
     flip either (\ _ -> nextInProcessOr (showUserController user) Nothing)
      (\ error -> displayError (showTError error)) transResult

--- Deletes a given User entity (after asking for acknowledgment)
--- and proceeds with the show controller.
askAndDeleteUserController :: User -> Controller
askAndDeleteUserController user =
  confirmControllerOLD
    (h3 [htxt (concat ["Benutzer \"",userToShortView user
                      ,"\" wirklich löschen?"])])
    (\ack -> if ack
             then deleteUserController user
             else showUserController user)

--- Deletes a given User entity and proceeds with the list controller.
deleteUserController :: User -> Controller
deleteUserController user =
  checkAuthorization (userOperationAllowed (DeleteEntity user)) $ \_ ->
   (do transResult <- runT (deleteUser user)
       flip either (\ _ -> listUserController)
        (\ error -> displayError (showTError error)) transResult)

--- Login as a given User entity.
loginUserController :: User -> Controller
loginUserController user =
  checkAuthorization checkAdmin $ \_ -> do
    let loginname = userLogin user
    loginToSession loginname
    setPageMessage ("Angemeldet als: "++loginname)
    defaultController

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
