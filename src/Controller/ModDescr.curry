module Controller.ModDescr (
 updateModDescrController, deleteModDescrController,
 listModDescrController
 ) where

import System.Spicey
import HTML.Base
import Time
import MDB
import View.ModDescr
import Maybe
import System.Authorization
import System.AuthorizedActions
import Config.UserProcesses
import System.Helpers

--- Persists a new ModDescr entity to the database.
createModDescrController
 :: Bool
  -> (String,String,String,String,String,String,String,String,String,String
     ,String,ModData)
  -> Controller
createModDescrController False _ = listModDescrController
createModDescrController True (language ,shortDesc ,objectives ,contents
                               ,prereq ,exam ,methods ,use ,literature ,links
                               ,comments ,modData) =
  do transResult <- runT
                     (newModDescrWithModDataDataDescKey language shortDesc
                       objectives contents prereq exam methods use literature
                       links comments (modDataKey modData))
     either (\ error -> displayError (showTError error))
            (\ _ -> nextInProcessOr listModDescrController Nothing)
            transResult
 
--- Persists modifications of a given ModDescr entity to the database.
updateModDescrController :: Controller -> ModDescr -> Controller
updateModDescrController cntcontroller modDescr =
 checkAuthorization (modDescrOperationAllowed (UpdateEntity modDescr)) $ \_ ->
  runT (updateModDescr modDescr) >>=
  either (\ error -> displayError (showTError error))
         (\ _ -> logEvent (UpdateModDescr modDescr) >>
                 nextInProcessOr cntcontroller Nothing)

--- Deletes a given ModDescr entity (depending on the Boolean
--- argument) and proceeds with the list controller.
deleteModDescrController :: ModDescr -> Bool -> Controller
deleteModDescrController _ False = listModDescrController
deleteModDescrController modDescr True =
  checkAuthorization (modDescrOperationAllowed (DeleteEntity modDescr)) $ \_ ->
   (do transResult <- runT (deleteModDescr modDescr)
       either (\ error -> displayError (showTError error))
              (\ _ -> listModDescrController)
              transResult)

--- Lists all ModDescr entities with buttons to show, delete,
--- or edit an entity.
listModDescrController :: Controller
listModDescrController =
  checkAuthorization (modDescrOperationAllowed ListEntities) $ \_ ->
   (do modDescrs <- runQ queryAllModDescrs
       return (listModDescrView modDescrs))

--- Shows a ModDescr entity.
showModDescrController :: ModDescr -> Controller
showModDescrController modDescr =
  checkAuthorization (modDescrOperationAllowed (ShowEntity modDescr)) $ \_ -> do
    dataDescModData <- runJustT (getDataDescModData modDescr)
    return (showModDescrView modDescr dataDescModData)

--- Gets the associated ModData entity for a given ModDescr entity.
getDataDescModData :: ModDescr -> DBAction ModData
getDataDescModData mModData = getModData (modDescrModDataDataDescKey mModData)
