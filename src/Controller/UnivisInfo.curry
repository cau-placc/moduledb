module Controller.UnivisInfo (
 mainUnivisInfoController, loadUnivisDataForm
 ) where

import Global
import Read
import Maybe
import Time

import Controller.ModData
import System.Helpers
import System.Spicey
import HTML.Base
import HTML.Session
import HTML.WUI
import MDB
import MDBExts
import View.UnivisInfo
import System.Authorization
import System.AuthorizedActions
import System.SessionInfo
import Config.UserProcesses
import UnivIS
import System.Authentication

--- Choose the controller for a UnivisInfo entity according to the URL parameter.
mainUnivisInfoController :: Controller
mainUnivisInfoController =
  do args <- getControllerParams
     case args of
      ["list"] -> listUnivisInfoController
      ["load"] -> loadUnivisController
      ["showmod",s,ts,ys] ->
          controllerOnKey s (showModDataUnivisInfoController ts ys)
      ["email",s,ts,ys] ->
          controllerOnKey s (emailModDataUnivisInfoController ts ys)
      _ -> displayUrlError

------------------------------------------------------------------------
--- Shows a form to create a new UnivisInfo entity.
newUnivisInfoController :: Controller
newUnivisInfoController =
  checkAuthorization (univisInfoOperationAllowed NewEntity) $ \sinfo -> do
    setParWuiStore newUnivisInfoStore sinfo ("", "", 0, "")
    return [formExp newUnivisInfoForm]

type NewUnivisInfo = (String,String,Int,String)

--- The form definition to create a new UnivisInfo entity
--- containing the controller to insert a new UnivisInfo entity.
newUnivisInfoForm :: HtmlFormDef (UserSessionInfo, WuiStore NewUnivisInfo)
newUnivisInfoForm =
  pwui2FormDef "Controller.UnivisInfo.newUnivisInfoForm"
    newUnivisInfoStore
    (\_ -> wUnivisInfo)
    (\_ entity ->
      checkAuthorization (univisInfoOperationAllowed NewEntity) $ \_ ->
      createUnivisInfoController entity)
    (\sinfo -> renderWUI sinfo "New UnivisInfo" "Create"
                         "?UnivisInfo/list" ())

---- The data stored for executing the WUI form.
newUnivisInfoStore ::
  Global (SessionStore (UserSessionInfo, WuiStore NewUnivisInfo))
newUnivisInfoStore =
  global emptySessionStore (Persistent (inSessionDataDir "newUnivisInfoStore"))

--- Persists a new UnivisInfo entity to the database.
createUnivisInfoController :: NewUnivisInfo -> Controller
createUnivisInfoController (code ,term ,year ,uRL) =
  runT (newUnivisInfo code term year uRL) >>=
  either (\ error -> displayError (showTError error))
         (\ _ -> nextInProcessOr listUnivisInfoController Nothing)

------------------------------------------------------------------------
--- Shows a form to edit the given UnivisInfo entity.
editUnivisInfoController :: UnivisInfo -> Controller
editUnivisInfoController univisInfo =
  checkAuthorization
   (univisInfoOperationAllowed (UpdateEntity univisInfo)) $ \sinfo -> do
     setParWuiStore editUnivisInfoWuiStore (sinfo,univisInfo) univisInfo
     return [formExp editUnivisInfoWuiForm]

--- A WUI form to edit UnivisInfo entity.
--- The default values for the fields are stored in the
--- `editUnivisInfoWuiStore`.
editUnivisInfoWuiForm ::
  HtmlFormDef ((UserSessionInfo,UnivisInfo), WuiStore UnivisInfo)
editUnivisInfoWuiForm =
  pwui2FormDef "Controller.UnivisInfo.editUnivisInfoWuiForm"
    editUnivisInfoWuiStore
    (\ (_,univisInfo) -> wUnivisInfoType univisInfo)
    (\_ univisInfo ->
       checkAuthorization
         (univisInfoOperationAllowed (UpdateEntity univisInfo)) $ \_ ->
           updateUnivisInfoController univisInfo)
    (\ (sinfo,_) ->
          renderWUI sinfo "Edit UnivisInfo" "Change"
                    "?UnivisInfo/list" ())

---- The data stored for executing the WUI form.
editUnivisInfoWuiStore ::
  Global (SessionStore ((UserSessionInfo,UnivisInfo), WuiStore UnivisInfo))
editUnivisInfoWuiStore =
  global emptySessionStore
         (Persistent (inSessionDataDir "editUnivisInfoWuiStore"))


--- Persists modifications of a given UnivisInfo entity to the
--- database.
updateUnivisInfoController :: UnivisInfo -> Controller
updateUnivisInfoController univisInfo =
  runT (updateUnivisInfo univisInfo) >>=
  either (\ error -> displayError (showTError error))
         (\ _ -> nextInProcessOr listUnivisInfoController Nothing)

------------------------------------------------------------------------
--- Deletes a given UnivisInfo entity (depending on the Boolean
--- argument) and proceeds with the list controller.
deleteUnivisInfoController :: UnivisInfo -> Bool -> Controller
deleteUnivisInfoController _ False = listUnivisInfoController
deleteUnivisInfoController univisInfo True =
  checkAuthorization (univisInfoOperationAllowed (DeleteEntity univisInfo)) $ \_ ->
   (do transResult <- runT (deleteUnivisInfo univisInfo)
       flip either (\ _ -> listUnivisInfoController)
        (\ error -> displayError (showTError error)) transResult)

--- Lists all UnivisInfo entities with buttons to show, delete,
--- or edit an entity.
listUnivisInfoController :: Controller
listUnivisInfoController =
  checkAuthorization (univisInfoOperationAllowed ListEntities) $ \_ -> do
    univisInfos <- runQ queryAllUnivisInfos
    return (listUnivisInfoView univisInfos)

--- Lists all UnivisInfo entities for a module in a given term.
showModDataUnivisInfoController :: String -> String -> ModData -> Controller
showModDataUnivisInfoController terms years mdata =
  checkAuthorization (modDataOperationAllowed (ShowEntity mdata)) $ \_ -> do
    admin <- isAdmin
    let sem = (terms, Read.readNat years)
    urls <- runQ $ queryUnivisURL (modDataCode mdata) sem
    mis <- runQ $ queryInstancesOfMod (modDataKey mdata)
    let semmis = filter (\mi -> (modInstTerm mi,modInstYear mi) == sem)
                        mis
    lecturer <- if null semmis then return Nothing else
      runT (getUser (modInstUserLecturerModsKey (head semmis)))
           >>= return . flip either Just (const Nothing)
    return (showUnivisLinks mdata sem lecturer urls admin)

--- Shows a form to send an email about UnivisInfo entities for a module
--- in a given term.
emailModDataUnivisInfoController :: String -> String -> ModData -> Controller
emailModDataUnivisInfoController terms years mdata =
  checkAuthorization checkAdmin $ \_ -> do
    let sem = (terms, Read.readNat years)
    urls <- runQ $ queryUnivisURL (modDataCode mdata) sem
    let msg = if null urls
                then missingUnivISMessage mdata sem
                else missingMDBMessage mdata sem
    putSessionData emailModuleStore (mdata, msg)
    return [formExp emailModuleMessageForm]

--- Shows a UnivisInfo entity.
showUnivisInfoController :: UnivisInfo -> Controller
showUnivisInfoController univisInfo =
 checkAuthorization (univisInfoOperationAllowed (ShowEntity univisInfo)) $ \_ ->
   (return (showUnivisInfoView univisInfo listUnivisInfoController))

------------------------------------------------------------------------
--- Shows a form to load data from UnivisInfo for a selectable term.
loadUnivisController :: Controller
loadUnivisController =
  checkAuthorization (univisInfoOperationAllowed NewEntity) $ \_ ->
    return [formExp loadUnivisDataForm]

--- A form to load data from UnivisInfo for a selectable term.
loadUnivisDataForm :: HtmlFormDef (String,Int)
loadUnivisDataForm = formDefWithID "Controller.UnivisInfo.loadUnivisDataForm"
  getCurrentSemester (loadUnivisView loadUnivisDataController)

loadUnivisDataController :: (String,Int) -> Controller
loadUnivisDataController sem =
  checkAuthorization (univisInfoOperationAllowed NewEntity) $ \_ ->
    readAndStoreUnivisOfSemester sem >>= \s -> return [h1 [htxt s]]

------------------------------------------------------------------------
