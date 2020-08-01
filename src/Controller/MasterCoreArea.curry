module Controller.MasterCoreArea (
 masterCoreAreaController, newMasterCoreAreaForm, editMasterCoreAreaForm,
 listMasterCoreAreaController
 ) where

import Global

import System.Helpers
import System.Spicey
import HTML.Base
import HTML.Session
import HTML.WUI
import Time
import MDB
import View.MasterCoreArea
import Maybe
import System.Authorization
import System.AuthorizedActions
import System.SessionInfo
import Config.UserProcesses
import System.Authentication
import View.MDBEntitiesToHtml

--- Choose the controller for a MasterCoreArea entity according to the URL parameter.
masterCoreAreaController :: Controller
masterCoreAreaController = do
  args <- getControllerParams
  case args of
    ["list"] -> listMasterCoreAreaController
    ["new"]  -> newMasterCoreAreaController
    ["show",s] -> controllerOnKey s showMasterCoreAreaController
    ["edit",s] -> controllerOnKey s editMasterCoreAreaController
    ["delete",s] -> controllerOnKey s deleteMasterCoreAreaController
    ["destroy",s] -> controllerOnKey s destroyMasterCoreAreaController
    _ -> displayError "Illegal URL for MasterCoreArea"

instance EntityController MasterCoreArea where
  controllerOnKey s controller =
    applyControllerOn (readMasterCoreAreaKey s) getMasterCoreArea controller

-----------------------------------------------------------------------
--- Shows a form to create a new MasterCoreArea entity.
newMasterCoreAreaController :: Controller
newMasterCoreAreaController =
  checkAuthorization (masterCoreAreaOperationAllowed NewEntity) $ \sinfo -> do
    setParWuiStore newMasterCoreAreaStore sinfo ("", "", "", "", 1)
    return [formExp newMasterCoreAreaForm]

type NewMasterCoreArea = (String,String,String,String,Int)

--- The form definition to create a new MasterCoreArea entity
--- containing the controller to insert a new MasterCoreArea entity.
newMasterCoreAreaForm ::
  HtmlFormDef (UserSessionInfo, WuiStore NewMasterCoreArea)
newMasterCoreAreaForm =
  pwui2FormDef "Controller.MasterCoreArea.newMasterCoreAreaForm"
    newMasterCoreAreaStore
    (\_ -> wMasterCoreArea)
    (\_ entity ->
       checkAuthorization (masterCoreAreaOperationAllowed NewEntity) $ \_ ->
         createMasterCoreAreaController entity)
    (\sinfo -> renderWUI sinfo "New MasterCoreArea" "Create"
                         "?MasterCoreArea/list" ())

---- The data stored for executing the WUI form.
newMasterCoreAreaStore ::
  Global (SessionStore (UserSessionInfo, WuiStore NewMasterCoreArea))
newMasterCoreAreaStore =
  global emptySessionStore (Persistent (inSessionDataDir "newMasterCoreAreaStore"))

--- Persists a new MasterCoreArea entity to the database.
createMasterCoreAreaController :: NewMasterCoreArea -> Controller
createMasterCoreAreaController (name ,shortName ,description ,areaKey
                               ,position) =
  do transResult <- runT
                     (newMasterCoreArea name shortName description areaKey
                       (Just position))
     flip either (\ _ -> nextInProcessOr listMasterCoreAreaController Nothing)
      (\ error -> displayError (showTError error)) transResult

-----------------------------------------------------------------------
--- Shows a form to edit the given MasterCoreArea entity.
editMasterCoreAreaController :: MasterCoreArea -> Controller
editMasterCoreAreaController mca =
  checkAuthorization
   (masterCoreAreaOperationAllowed (UpdateEntity mca)) $ \sinfo -> do
     setParWuiStore editMasterCoreAreaWuiStore (sinfo,mca) mca
     return [formExp editMasterCoreAreaForm]

--- A WUI form to edit MasterCoreArea entity.
--- The default values for the fields are stored in the
--- `editMasterCoreAreaWuiStore`.
editMasterCoreAreaForm ::
  HtmlFormDef ((UserSessionInfo,MasterCoreArea), WuiStore MasterCoreArea)
editMasterCoreAreaForm =
  pwui2FormDef "Controller.MasterCoreArea.editMasterCoreAreaForm"
    editMasterCoreAreaWuiStore
    (\ (_,masterCoreArea) -> wMasterCoreAreaType masterCoreArea)
    (\_ masterCoreArea ->
       checkAuthorization
         (masterCoreAreaOperationAllowed (UpdateEntity masterCoreArea)) $ \_ ->
           updateMasterCoreAreaController masterCoreArea)
    (\ (sinfo,masterCoreArea) ->
         renderWUI sinfo "Studierendendaten bearbeiten" "Change"
           ("?MasterCoreArea/show/" ++ showMasterCoreAreaKey masterCoreArea) ())

---- The data stored for executing the WUI form.
editMasterCoreAreaWuiStore ::
  Global (SessionStore ((UserSessionInfo,MasterCoreArea),
          WuiStore MasterCoreArea))
editMasterCoreAreaWuiStore =
  global emptySessionStore
         (Persistent (inSessionDataDir "editMasterCoreAreaWuiStore"))

--- Persists modifications of a given MasterCoreArea entity.
updateMasterCoreAreaController :: MasterCoreArea -> Controller
updateMasterCoreAreaController mca =
  runT (updateMasterCoreArea mca) >>=
  either (\ error -> displayError (showTError error))
         (\ _ -> nextInProcessOr (showMasterCoreAreaController mca) Nothing)

--- Deletes a given MasterCoreArea entity (after asking for acknowledgment)
--- and proceeds with the show controller.
deleteMasterCoreAreaController :: MasterCoreArea -> Controller
deleteMasterCoreAreaController mca =
  checkAuthorization
   (masterCoreAreaOperationAllowed (DeleteEntity mca)) $ \si ->
     confirmDeletionPage si $ concat
       ["Really delete entity \"", masterCoreAreaToShortView mca, "\"?"]

--- Deletes a given MasterCoreArea entity (depending on the Boolean
--- argument) and proceeds with the list controller.
destroyMasterCoreAreaController :: MasterCoreArea -> Controller
destroyMasterCoreAreaController masterCoreArea =
  checkAuthorization
   (masterCoreAreaOperationAllowed (DeleteEntity masterCoreArea)) $ \_ ->
   (do transResult <- runT (deleteMasterCoreArea masterCoreArea)
       flip either (\ _ -> listMasterCoreAreaController)
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
