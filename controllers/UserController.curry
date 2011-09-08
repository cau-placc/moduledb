module UserController (
 newUserController, editUserController, deleteUserController,
 listUserController
 ) where

import Spicey
import KeyDatabase
import HTML
import Time
import MDB
import UserView
import Maybe
import Authorization
import AuthorizedControllers
import UserProcesses

--- Shows a form to create a new User entity.
newUserController :: Controller
newUserController =
  checkAuthorization (userOperationAllowed NewEntity) $
   (do ctime <- getLocalTime
       return (blankUserView ctime createUserController))

--- Persists a new User entity to the database.
createUserController
 :: Bool -> (String,String,String,String,String,String,String,CalendarTime)
  -> Controller
createUserController False _ = listUserController
createUserController True (login ,name ,first ,title ,email ,url ,password
                           ,lastLogin) =
  do transResult <- runT
                     (newUser login name first title email url password
                       lastLogin)
     either (\ _ -> nextInProcessOr listUserController Nothing)
      (\ error -> displayError (showTError error)) transResult

--- Shows a form to edit the given User entity.
editUserController :: User -> Controller
editUserController userToEdit =
  checkAuthorization (userOperationAllowed (UpdateEntity userToEdit)) $
   (do return (editUserView userToEdit updateUserController))

--- Persists modifications of a given User entity to the
--- database depending on the Boolean argument. If the Boolean argument
--- is False, nothing is changed.
updateUserController :: Bool -> User -> Controller
updateUserController False _ = listUserController
updateUserController True user =
  do transResult <- runT (updateUser user)
     either (\ _ -> nextInProcessOr listUserController Nothing)
      (\ error -> displayError (showTError error)) transResult

--- Deletes a given User entity (depending on the Boolean
--- argument) and proceeds with the list controller.
deleteUserController :: User -> Bool -> Controller
deleteUserController _ False = listUserController
deleteUserController user True =
  checkAuthorization (userOperationAllowed (DeleteEntity user)) $
   (do transResult <- runT (deleteUser user)
       either (\ _ -> listUserController)
        (\ error -> displayError (showTError error)) transResult)

--- Lists all User entities with buttons to show, delete,
--- or edit an entity.
listUserController :: Controller
listUserController =
  checkAuthorization (userOperationAllowed ListEntities) $
   (do users <- runQ queryAllUsers
       return
        (listUserView users showUserController editUserController
          deleteUserController))

--- Shows a User entity.
showUserController :: User -> Controller
showUserController user =
  checkAuthorization (userOperationAllowed (ShowEntity user)) $
   (do return (showUserView user listUserController))
