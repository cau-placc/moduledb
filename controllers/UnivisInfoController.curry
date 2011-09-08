module UnivisInfoController (
 newUnivisInfoController, editUnivisInfoController,
 deleteUnivisInfoController, listUnivisInfoController
 ) where

import Spicey
import KeyDatabase
import HTML
import Time
import MDB
import UnivisInfoView
import Maybe
import Authorization
import AuthorizedControllers
import UserProcesses
import Read

--- Shows a form to create a new UnivisInfo entity.
newUnivisInfoController :: Controller
newUnivisInfoController =
  checkAuthorization (univisInfoOperationAllowed NewEntity) $
   (do return (blankUnivisInfoView createUnivisInfoController))

--- Persists a new UnivisInfo entity to the database.
createUnivisInfoController :: Bool -> (String,String,Int,String) -> Controller
createUnivisInfoController False _ = listUnivisInfoController
createUnivisInfoController True (code ,term ,year ,uRL) =
  do transResult <- runT (newUnivisInfo code term year uRL)
     either (\ _ -> nextInProcessOr listUnivisInfoController Nothing)
      (\ error -> displayError (showTError error)) transResult

--- Shows a form to edit the given UnivisInfo entity.
editUnivisInfoController :: UnivisInfo -> Controller
editUnivisInfoController univisInfoToEdit =
  checkAuthorization
   (univisInfoOperationAllowed (UpdateEntity univisInfoToEdit)) $
   (do return
        (editUnivisInfoView univisInfoToEdit updateUnivisInfoController))

--- Persists modifications of a given UnivisInfo entity to the
--- database depending on the Boolean argument. If the Boolean argument
--- is False, nothing is changed.
updateUnivisInfoController :: Bool -> UnivisInfo -> Controller
updateUnivisInfoController False _ = listUnivisInfoController
updateUnivisInfoController True univisInfo =
  do transResult <- runT (updateUnivisInfo univisInfo)
     either (\ _ -> nextInProcessOr listUnivisInfoController Nothing)
      (\ error -> displayError (showTError error)) transResult

--- Deletes a given UnivisInfo entity (depending on the Boolean
--- argument) and proceeds with the list controller.
deleteUnivisInfoController :: UnivisInfo -> Bool -> Controller
deleteUnivisInfoController _ False = listUnivisInfoController
deleteUnivisInfoController univisInfo True =
  checkAuthorization (univisInfoOperationAllowed (DeleteEntity univisInfo)) $
   (do transResult <- runT (deleteUnivisInfo univisInfo)
       either (\ _ -> listUnivisInfoController)
        (\ error -> displayError (showTError error)) transResult)

--- Lists all UnivisInfo entities with buttons to show, delete,
--- or edit an entity.
listUnivisInfoController :: Controller
listUnivisInfoController =
  checkAuthorization (univisInfoOperationAllowed ListEntities) $ do
    args <- getControllerParams
    if length args < 3
     then do univisInfos <- runQ queryAllUnivisInfos
             return (listUnivisInfoView univisInfos showUnivisInfoController
                       editUnivisInfoController deleteUnivisInfoController)
     else do let mcode = head args
                 sem   = (args!!1, Read.readNat (args!!2))
             urls <- runQ $ queryUnivisURL mcode sem
             mods <- runQ $ queryModDataWithCode mcode
             if null mods then return (showUnivisLinks mcode sem Nothing urls)
              else do
                mis <- runQ $ queryInstancesOfMod (modDataKey (head mods))
                let semmis = filter (\mi -> (modInstTerm mi,modInstYear mi)==sem)
                                    mis
                lecturer <- if null semmis then return Nothing else
                  runT (getUser (modInstUserLecturerModsKey (head semmis)))
                   >>= return . either Just (const Nothing)
                return (showUnivisLinks mcode sem lecturer urls)

--- Shows a UnivisInfo entity.
showUnivisInfoController :: UnivisInfo -> Controller
showUnivisInfoController univisInfo =
  checkAuthorization (univisInfoOperationAllowed (ShowEntity univisInfo)) $
   (do return (showUnivisInfoView univisInfo listUnivisInfoController))
