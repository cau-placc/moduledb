module ModDescrController (
 editModDescrController, deleteModDescrController,
 listModDescrController
 ) where

import Spicey
import KeyDatabase
import HTML.Base
import Time
import MDB
import ModDescrView
import Maybe
import Authorization
import AuthorizedControllers
import UserProcesses
import Helpers

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
     either (\ _ -> nextInProcessOr listModDescrController Nothing)
      (\ error -> displayError (showTError error)) transResult

--- Shows a form to edit the given ModDescr entity.
editModDescrController :: Controller -> ModDescr -> Controller
editModDescrController cntcontroller modDescrToEdit =
  checkAuthorization (modDescrOperationAllowed (UpdateEntity modDescrToEdit))
   $ \_ -> return (editModDescrView modDescrToEdit
                              (updateModDescrController cntcontroller))

--- Persists modifications of a given ModDescr entity to the
--- database depending on the Boolean argument. If the Boolean argument
--- is False, nothing is changed.
updateModDescrController :: Controller -> Bool -> ModDescr -> Controller
updateModDescrController cntcontroller False _ = cntcontroller
updateModDescrController cntcontroller True modDescr =
  runT (updateModDescr modDescr) >>=
  either (\ _ -> logEvent (UpdateModDescr modDescr) >>
                 nextInProcessOr cntcontroller Nothing)
         (\ error -> displayError (showTError error))

--- Deletes a given ModDescr entity (depending on the Boolean
--- argument) and proceeds with the list controller.
deleteModDescrController :: ModDescr -> Bool -> Controller
deleteModDescrController _ False = listModDescrController
deleteModDescrController modDescr True =
  checkAuthorization (modDescrOperationAllowed (DeleteEntity modDescr)) $ \_ ->
   (do transResult <- runT (deleteModDescr modDescr)
       either (\ _ -> listModDescrController)
        (\ error -> displayError (showTError error)) transResult)

--- Lists all ModDescr entities with buttons to show, delete,
--- or edit an entity.
listModDescrController :: Controller
listModDescrController =
  checkAuthorization (modDescrOperationAllowed ListEntities) $ \_ ->
   (do modDescrs <- runQ queryAllModDescrs
       return
        (listModDescrView modDescrs showModDescrController
          (editModDescrController listModDescrController)
          deleteModDescrController))

--- Shows a ModDescr entity.
showModDescrController :: ModDescr -> Controller
showModDescrController modDescr =
  checkAuthorization (modDescrOperationAllowed (ShowEntity modDescr)) $ \_ ->
   (do dataDescModData <- runJustT (getDataDescModData modDescr)
       return
        (showModDescrView modDescr dataDescModData listModDescrController))

--- Gets the associated ModData entity for a given ModDescr entity.
getDataDescModData :: ModDescr -> Transaction ModData
getDataDescModData mModData = getModData (modDescrModDataDataDescKey mModData)
