module Controller.ModInst
  ( mainModInstController
  , updateAllModInstController, createModInstController
  ) where

import System.Spicey
import HTML.Base
import Time
import MDB
import MDBExts
import View.ModInst
import View.User
import Maybe
import System.Authorization
import System.AuthorizedActions
import Config.UserProcesses
import List
import Sort
import System.Authentication
import System.Helpers
import ConfigMDB
import System.StudyPlanner

--- Choose the controller for a ModInst entity according to the URL parameter.
mainModInstController :: Controller
mainModInstController =
  do args <- getControllerParams
     case args of
      --[] -> listModInstController
      --["list"] -> listModInstController
      ["show" ,s] ->
       applyControllerOn (readModInstKey s) getModInst showModInstController
      _ -> displayUrlError


-------------------------------------------------------------------------
--- Persists a new ModInst entity to the database.
createModInstController :: ModData -> Controller
                        -> (String,Int,User) -> Controller
createModInstController mdata cntcontroller (term,year,user) =
 checkAuthorization (modDataOperationAllowed (UpdateEntity mdata)) $ \_ -> do
  modinsts <- runQ $ queryInstancesOfMod (modDataKey mdata)
  let mb = find (\mi -> modInstTerm mi == term && modInstYear mi == year)
                modinsts
  if mb==Nothing
   then runT (newModInstWithUserLecturerModsKeyWithModDataModuleInstancesKey
                    term (Just year) (userKey user) (modDataKey mdata)) >>=
        flip either (\ mi -> logEvent (NewModInst mi) >>
                        setPageMessage "Semester hinzugefügt" >>
                        nextInProcessOr cntcontroller Nothing)
               (\ error -> displayError (showTError error))
   else setPageMessage "Semester schon vorhanden!" >> cntcontroller


-------------------------------------------------------------------------
--- Persists modifications of given ModInst entities.
updateAllModInstController :: ModData -> [ModInst] -> Controller
                           -> [(ModInst,Bool)] -> Controller
updateAllModInstController mdata oldinsts cntcontroller modinsts =
 checkAuthorization (modDataOperationAllowed (UpdateEntity mdata)) $ \_ -> do
  takenmodinsts <- getTakenModuleInstances removedModInsts
  if not (null takenmodinsts)
   then -- some module instances to be deleted already taken in study planner:
    do md <- runJustT
               (getModData (modInstModDataModuleInstancesKey (head oldinsts)))
       displayHtmlError
         [h1 [htxt "Fehler: Einige Instanzen nicht veränderbar!"],
          par [htxt "Die folgenden Instanzen können nicht geändert werden, ",
               htxt "da einige Studierende diese schon eingeplant haben ",
               htxt "(vgl. ", spEHref studyPlannerURL [htxt "Studienplaner"],
               htxt "):"],
          par [htxt (unwords
                      (map (showSemester . modInstSemester) takenmodinsts))],
          spHref ("?ModData/show/" ++ showModDataKey md)
                 [htxt "Zurück zum Modul"]]
   else
    runT (mapM (\ (oi,(ni,del)) ->
                 if del
                 then inUse oi |>>= \useoi ->
                   if useoi
                   then returnT [Nothing]
                   else deleteModInst oi |>> returnT [Just (DeleteModInst oi)]
                 else
                  if oi==ni
                  then returnT []
                  else inUse oi |>>= \useoi ->
                    if useoi
                    then returnT [Nothing]
                    else updateModInst ni |>> returnT [Just (UpdateModInst ni)])
             oldnewinsts) >>=
    flip either (\ upds  -> do
               mapIO_ (maybe done logEvent) (concat upds)
               if all isJust (concat upds) then done
                                           else setPageMessage useMsg
               nextInProcessOr cntcontroller Nothing )
           (\ error -> displayError (showTError error))
 where
  oldnewinsts = zip oldinsts modinsts

  -- compute module instances where a semester should be deleted or moved
  removedModInsts =
    map fst
        (filter
           (\ (oi,(ni,del)) -> del || modInstSemester oi /= modInstSemester ni)
           oldnewinsts)

  inUse mi = getMasterProgramKeysOfModInst [mi] |>>= \[mpkeys] ->
             returnT (not (null mpkeys))

  useMsg = "Einige Modulinstanzen können nicht mehr verändert werden, "++
           "da sie inzwischen in einem Masterprogramm verwendet werden!"

--- Shows a form to edit the given ModInst entity.
editModInstController :: ModInst -> Controller
editModInstController _ =
  error "editModInstController: Operation no longer supported"

--- Persists modifications of a given ModInst entity to the
--- database depending on the Boolean argument. If the Boolean argument
--- is False, nothing is changed.
updateModInstController :: Bool -> ModInst -> Controller
updateModInstController False _ = listModInstController
updateModInstController True modInst =
  do transResult <- runT (updateModInst modInst)
     flip either (\ _ -> nextInProcessOr listModInstController Nothing)
      (\ error -> displayError (showTError error)) transResult

--- Deletes a given ModInst entity (depending on the Boolean
--- argument) and proceeds with the list controller.
deleteModInstController :: ModInst -> Bool -> Controller
deleteModInstController _ False = listModInstController
deleteModInstController modInst True =
  checkAuthorization (modInstOperationAllowed (DeleteEntity modInst)) $ \_ ->
   (do transResult <- runT (deleteModInst modInst)
       flip either (\ _ -> listModInstController)
        (\ error -> displayError (showTError error)) transResult)

--- Lists all ModInst entities with buttons to show, delete,
--- or edit an entity.
listModInstController :: Controller
listModInstController =
  checkAuthorization (modInstOperationAllowed ListEntities) $ \sinfo -> do
    args <- getControllerParams
    if null args
     then displayUrlError
     else
       maybe displayUrlError
       (\mik -> do
         mi       <- runJustT $ getModInst mik
         user     <- runJustT $ getUser (modInstUserLecturerModsKey mi)
         moddata  <- runJustT $ getModData (modInstModDataModuleInstancesKey mi)
         [mpkeys] <- runQ $ getMasterProgramKeysOfModInst [mi]
         mps      <- runJustT (mapM getMasterProgram mpkeys)
         spkeys   <- runQ (getAdvisorStudyProgramKeysOfModInst mi)
         sprogs   <- runJustT (mapM getAdvisorStudyProgram spkeys)
         return (singleModInstView sinfo mi moddata user mps sprogs))
       (readModInstKey (head args))

--- Shows a ModInst entity.
showModInstController :: ModInst -> Controller
showModInstController mi =
  checkAuthorization (modInstOperationAllowed (ShowEntity mi)) $ \sinfo ->
   (do user     <- runJustT $ getUser (modInstUserLecturerModsKey mi)
       moddata  <- runJustT $ getModData (modInstModDataModuleInstancesKey mi)
       [mpkeys] <- runQ $ getMasterProgramKeysOfModInst [mi]
       mps      <- runJustT (mapM getMasterProgram mpkeys)
       spkeys   <- runQ (getAdvisorStudyProgramKeysOfModInst mi)
       sprogs   <- runJustT (mapM getAdvisorStudyProgram spkeys)
       return (singleModInstView sinfo mi moddata user mps sprogs))

--- Gets the associated ModData entity for a given ModInst entity.
getModuleInstancesModData :: ModInst -> DBAction ModData
getModuleInstancesModData mModData =
  getModData (modInstModDataModuleInstancesKey mModData)

--- Gets the associated User entity for a given ModInst entity.
getLecturerModsUser :: ModInst -> DBAction User
getLecturerModsUser mUser = getUser (modInstUserLecturerModsKey mUser)
