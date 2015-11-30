module MasterCoreAreaController (
 masterCoreAreaController, editMasterCoreAreaController,
 deleteMasterCoreAreaController, listMasterCoreAreaController
 ) where

import Spicey
import KeyDatabase
import HTML
import Time
import MDB
import MasterCoreAreaView
import Maybe
import Authorization
import AuthorizedControllers
import UserProcesses
import Authentication
import MDBEntitiesToHtml

--- Choose the controller for a MasterCoreArea entity according to the URL parameter.
masterCoreAreaController :: Controller
masterCoreAreaController = do
  args <- getControllerParams
  case args of
    ["list"] -> listMasterCoreAreaController
    ["new"]  -> newMasterCoreAreaController
    ["show",s] -> applyControllerOn (readMasterCoreAreaKey s)
                    getMasterCoreArea showMasterCoreAreaController
    ["edit",s] -> applyControllerOn (readMasterCoreAreaKey s)
                    getMasterCoreArea editMasterCoreAreaController
    ["delete",s] -> applyControllerOn (readMasterCoreAreaKey s)
                      getMasterCoreArea askAndDeleteMasterCoreAreaController
    _ -> displayError "Illegal URL for MasterCoreArea"


--- Shows a form to create a new MasterCoreArea entity.
newMasterCoreAreaController :: Controller
newMasterCoreAreaController =
  checkAuthorization (masterCoreAreaOperationAllowed NewEntity) $ \_ ->
   (do return (blankMasterCoreAreaView createMasterCoreAreaController))

--- Persists a new MasterCoreArea entity to the database.
createMasterCoreAreaController
 :: Bool -> (String,String,String,String,Int) -> Controller
createMasterCoreAreaController False _ = listMasterCoreAreaController
createMasterCoreAreaController True (name ,shortName ,description ,areaKey
                                     ,position) =
  do transResult <- runT
                     (newMasterCoreArea name shortName description areaKey
                       (Just position))
     either (\ _ -> nextInProcessOr listMasterCoreAreaController Nothing)
      (\ error -> displayError (showTError error)) transResult

--- Shows a form to edit the given MasterCoreArea entity.
editMasterCoreAreaController :: MasterCoreArea -> Controller
editMasterCoreAreaController masterCoreAreaToEdit =
  checkAuthorization
   (masterCoreAreaOperationAllowed (UpdateEntity masterCoreAreaToEdit)) $ \_ ->
   (do return
        (editMasterCoreAreaView masterCoreAreaToEdit
          updateMasterCoreAreaController))

--- Persists modifications of a given MasterCoreArea entity to the
--- database depending on the Boolean argument. If the Boolean argument
--- is False, nothing is changed.
updateMasterCoreAreaController :: Bool -> MasterCoreArea -> Controller
updateMasterCoreAreaController False mca = showMasterCoreAreaController mca
updateMasterCoreAreaController True mca =
  do transResult <- runT (updateMasterCoreArea mca)
     either (\ _ -> nextInProcessOr (showMasterCoreAreaController mca) Nothing)
      (\ error -> displayError (showTError error)) transResult

--- Deletes a given MasterCoreArea entity (after asking for acknowledgment)
--- and proceeds with the show controller.
askAndDeleteMasterCoreAreaController :: MasterCoreArea -> Controller
askAndDeleteMasterCoreAreaController mca =
  confirmControllerOLD
    (h3 [htxt (concat ["Really delete entity \""
                      ,masterCoreAreaToShortView mca,"\"?"])])
    (\ack -> if ack
             then deleteMasterCoreAreaController mca
             else showMasterCoreAreaController mca)

--- Deletes a given MasterCoreArea entity (depending on the Boolean
--- argument) and proceeds with the list controller.
deleteMasterCoreAreaController :: MasterCoreArea -> Controller
deleteMasterCoreAreaController masterCoreArea =
  checkAuthorization
   (masterCoreAreaOperationAllowed (DeleteEntity masterCoreArea)) $ \_ ->
   (do transResult <- runT (deleteMasterCoreArea masterCoreArea)
       either (\ _ -> listMasterCoreAreaController)
        (\ error -> displayError (showTError error)) transResult)

--- Lists all MasterCoreArea entities with buttons to show, delete,
--- or edit an entity.
listMasterCoreAreaController :: Controller
listMasterCoreAreaController =
  checkAuthorization (masterCoreAreaOperationAllowed ListEntities) $ \_ ->
   (do admin <- isAdmin
       masterCoreAreas <- runQ queryAllMasterCoreAreas
       return (listMasterCoreAreaView admin masterCoreAreas))

--- Shows a MasterCoreArea entity.
showMasterCoreAreaController :: MasterCoreArea -> Controller
showMasterCoreAreaController masterCoreArea =
  checkAuthorization
   (masterCoreAreaOperationAllowed (ShowEntity masterCoreArea)) $ \_ ->
   (do return (showMasterCoreAreaView masterCoreArea))
