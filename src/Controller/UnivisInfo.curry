module Controller.UnivisInfo (
 mainUnivisInfoController
 ) where

import System.Helpers
import System.Spicey
import HTML.Base
import Time
import MDB
import MDBExts
import View.UnivisInfo
import Maybe
import System.Authorization
import System.AuthorizedActions
import Config.UserProcesses
import Read
import UnivIS
import System.Authentication
import Controller.ModData

--- Choose the controller for a UnivisInfo entity according to the URL parameter.
mainUnivisInfoController :: Controller
mainUnivisInfoController =
  do args <- getControllerParams
     case args of
      ["list"] -> listUnivisInfoController
      ["load"] -> loadUnivisController
      ["showmod",s,ts,ys] -> applyControllerOn (readModDataKey s) getModData
                               (showModDataUnivisInfoController ts ys)
      _ -> displayError "Illegal URL"

--- Shows a form to create a new UnivisInfo entity.
newUnivisInfoController :: Controller
newUnivisInfoController =
  checkAuthorization (univisInfoOperationAllowed NewEntity) $ \_ ->
   (do return (blankUnivisInfoView createUnivisInfoController))

--- Persists a new UnivisInfo entity to the database.
createUnivisInfoController :: Bool -> (String,String,Int,String) -> Controller
createUnivisInfoController False _ = listUnivisInfoController
createUnivisInfoController True (code ,term ,year ,uRL) =
  do transResult <- runT (newUnivisInfo code term year uRL)
     flip either (\ _ -> nextInProcessOr listUnivisInfoController Nothing)
      (\ error -> displayError (showTError error)) transResult

--- Shows a form to edit the given UnivisInfo entity.
editUnivisInfoController :: UnivisInfo -> Controller
editUnivisInfoController univisInfoToEdit =
  checkAuthorization
   (univisInfoOperationAllowed (UpdateEntity univisInfoToEdit)) $ \_ ->
   (do return
        (editUnivisInfoView univisInfoToEdit updateUnivisInfoController))

--- Persists modifications of a given UnivisInfo entity to the
--- database depending on the Boolean argument. If the Boolean argument
--- is False, nothing is changed.
updateUnivisInfoController :: Bool -> UnivisInfo -> Controller
updateUnivisInfoController False _ = listUnivisInfoController
updateUnivisInfoController True univisInfo =
  do transResult <- runT (updateUnivisInfo univisInfo)
     flip either (\ _ -> nextInProcessOr listUnivisInfoController Nothing)
      (\ error -> displayError (showTError error)) transResult

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
    return (listUnivisInfoView univisInfos showUnivisInfoController
                       editUnivisInfoController deleteUnivisInfoController)

--- Lists all UnivisInfo entities for a module in a given term.
showModDataUnivisInfoController :: String -> String -> ModData -> Controller
showModDataUnivisInfoController terms years mdata =
  checkAuthorization (modDataOperationAllowed (ShowEntity mdata)) $ \_ -> do
    admin <- isAdmin
    responsible <- runJustT (getResponsibleUser mdata)
    let sem = (terms, Read.readNat years)
    urls <- runQ $ queryUnivisURL (modDataCode mdata) sem
    mis <- runQ $ queryInstancesOfMod (modDataKey mdata)
    let semmis = filter (\mi -> (modInstTerm mi,modInstYear mi) == sem)
                        mis
    lecturer <- if null semmis then return Nothing else
      runT (getUser (modInstUserLecturerModsKey (head semmis)))
           >>= return . flip either Just (const Nothing)
    return (showUnivisLinks mdata sem lecturer urls admin
               (emailModuleMessageController listUnivisInfoController
                                             mdata responsible))

--- Shows a UnivisInfo entity.
showUnivisInfoController :: UnivisInfo -> Controller
showUnivisInfoController univisInfo =
  checkAuthorization (univisInfoOperationAllowed (ShowEntity univisInfo)) $ \_ ->
   (do return (showUnivisInfoView univisInfo listUnivisInfoController))

--- Shows a form to load data from UnivisInfo for selectable term.
loadUnivisController :: Controller
loadUnivisController =
  checkAuthorization (univisInfoOperationAllowed NewEntity) $ \_ -> do
    csem  <- getCurrentSemester
    return (loadUnivisView csem loadUnivisDataController)

loadUnivisDataController :: (String,Int) -> Controller
loadUnivisDataController sem =
  readAndStoreUnivisOfSemester sem >>= \s -> return [h1 [htxt s]]


