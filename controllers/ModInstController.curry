module ModInstController (
 mainModInstController,
 addModInstController, editAllModInstController
 ) where

import Spicey
import KeyDatabase
import HTML
import Time
import MDB
import ModInstView
import UserView
import Maybe
import Authorization
import AuthorizedControllers
import UserProcesses
import List
import Sort
import Authentication
import Helpers
import ConfigMDB
import StudyPlanner

--- Choose the controller for a ModInst entity according to the URL parameter.
mainModInstController :: Controller
mainModInstController =
  do args <- getControllerParams
     case args of
      --[] -> listModInstController
      --["list"] -> listModInstController
      ["show" ,s] ->
       applyControllerOn (readModInstKey s) getModInst showModInstController
      _ -> displayError "Illegal URL"

--- Shows a form to add a new ModInst entity for a module.
addModInstController :: ModData -> User -> Controller -> Controller
addModInstController md user cntcontroller =
  checkAuthorization (modInstOperationAllowed NewEntity) $ \_ -> do
    allUsers <- runQ (transformQ (mergeSort leqUser) queryAllUsers)
    return (addModInstView user allUsers
                           (createModInstController md cntcontroller))

--- Persists a new ModInst entity to the database.
createModInstController :: ModData -> Controller -> Bool
                        -> (String,Int,User) -> Controller
createModInstController _ cntcontroller False _ = cntcontroller
createModInstController moddata cntcontroller True (term,year,user) = do
  modinsts <- runQ $ queryInstancesOfMod (modDataKey moddata)
  let mb = find (\mi -> modInstTerm mi == term && modInstYear mi == year)
                modinsts
  if mb==Nothing
   then runT (newModInstWithUserLecturerModsKeyWithModDataModuleInstancesKey
                    term (Just year) (userKey user) (modDataKey moddata)) >>=
        either (\ mi -> logEvent (NewModInst mi) >>
                        setPageMessage "Semester hinzugefügt" >>
                        nextInProcessOr cntcontroller Nothing)
               (\ error -> displayError (showTError error))
   else setPageMessage "Semester schon vorhanden!" >> cntcontroller

--- Shows a form to edit the list of ModInst entities for a given module.
editAllModInstController :: ModData -> Controller -> Controller
editAllModInstController md cntcontroller = do
   --checkAuthorization (modInstOperationAllowed (UpdateEntity md)) $ do
   admin <- isAdmin
   allinsts <- runQ $ transformQ (mergeSort leqModInst . filterModInsts admin)
                                 (queryInstancesOfMod (modDataKey md))
   allmpkeys <- runQ $ getMasterProgramKeysOfModInst allinsts
   allspkeys <- runJustT $
                 mapT (\mi -> getDB (getAdvisorStudyProgramKeysOfModInst mi))
                      allinsts
   allUsers <- runQ (transformQ (mergeSort leqUser) queryAllUsers)
   -- select instances not used in master programs:
   let editinsts = map (\ (mi,_,_) -> mi)
                       (filter (\ (_,mks,sks) -> null mks && null sks)
                               (zip3 allinsts allmpkeys allspkeys))
   return (editModInstView admin editinsts
             allUsers (updateAllModInstController editinsts cntcontroller))
 where
  filterModInsts admin =
   if admin
   then id
   else filter (\mi -> leqSemester (currentTerm,currentYear)
                                   (modInstTerm mi,modInstYear mi))

--- Persists modifications of given ModInst entities to the
--- database depending on the Boolean argument. If the Boolean argument
--- is False, nothing is changed.
updateAllModInstController :: [ModInst] -> Controller
                           -> Bool -> [(ModInst,Bool)] -> Controller
updateAllModInstController _ cntcontroller False _ = cntcontroller
updateAllModInstController oldinsts cntcontroller True modinsts = do
  takenmodinsts <- getTakenModuleInstances removedModInsts
  if not (null takenmodinsts)
   then -- some module instances to be deleted already taken in study planner:
    do mdata <- runJustT
                 (getModData (modInstModDataModuleInstancesKey (head oldinsts)))
       displayHtmlError
         [h1 [htxt "Fehler: Einige Instanzen nicht veränderbar!"],
          par [htxt "Die folgenden Instanzen können nicht geändert werden, ",
               htxt "da einige Studierende diese schon eingeplant haben ",
               htxt "(vgl. ", spEHref studyPlannerURL [htxt "Studienplaner"],
               htxt "):"],
          par [htxt (unwords
                      (map (showSemester . modInstSemester) takenmodinsts))],
          spHref ("?ModData/show/" ++ showModDataKey mdata)
                 [htxt "Zurück zum Modul"]]
   else
    runT (mapT (\ (oi,(ni,del)) ->
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
    either (\ upds  -> do
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

  inUse mi = getDB (getMasterProgramKeysOfModInst [mi]) |>>= \[mpkeys] ->
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
     either (\ _ -> nextInProcessOr listModInstController Nothing)
      (\ error -> displayError (showTError error)) transResult

--- Deletes a given ModInst entity (depending on the Boolean
--- argument) and proceeds with the list controller.
deleteModInstController :: ModInst -> Bool -> Controller
deleteModInstController _ False = listModInstController
deleteModInstController modInst True =
  checkAuthorization (modInstOperationAllowed (DeleteEntity modInst)) $ \_ ->
   (do transResult <- runT (deleteModInst modInst)
       either (\ _ -> listModInstController)
        (\ error -> displayError (showTError error)) transResult)

--- Lists all ModInst entities with buttons to show, delete,
--- or edit an entity.
listModInstController :: Controller
listModInstController =
  checkAuthorization (modInstOperationAllowed ListEntities) $ \_ -> do
    args <- getControllerParams
    if null args
     then displayError "Illegal URL"
     else
       maybe (displayError "Illegal URL")
       (\mik -> do
         mi       <- runJustT $ getModInst mik
         user     <- runJustT $ getUser (modInstUserLecturerModsKey mi)
         moddata  <- runJustT $ getModData (modInstModDataModuleInstancesKey mi)
         [mpkeys] <- runQ $ getMasterProgramKeysOfModInst [mi]
         mps      <- runJustT (mapT getMasterProgram mpkeys)
         spkeys   <- runQ (getAdvisorStudyProgramKeysOfModInst mi)
         sprogs   <- runJustT (mapT getAdvisorStudyProgram spkeys)
         return (singleModInstView mi moddata user mps sprogs))
       (readModInstKey (head args))

--- Shows a ModInst entity.
showModInstController :: ModInst -> Controller
showModInstController mi =
  checkAuthorization (modInstOperationAllowed (ShowEntity mi)) $ \_ ->
   (do user     <- runJustT $ getUser (modInstUserLecturerModsKey mi)
       moddata  <- runJustT $ getModData (modInstModDataModuleInstancesKey mi)
       [mpkeys] <- runQ $ getMasterProgramKeysOfModInst [mi]
       mps      <- runJustT (mapT getMasterProgram mpkeys)
       spkeys   <- runQ (getAdvisorStudyProgramKeysOfModInst mi)
       sprogs   <- runJustT (mapT getAdvisorStudyProgram spkeys)
       return (singleModInstView mi moddata user mps sprogs))

--- Gets the associated ModData entity for a given ModInst entity.
getModuleInstancesModData :: ModInst -> Transaction ModData
getModuleInstancesModData mModData =
  getModData (modInstModDataModuleInstancesKey mModData)

--- Gets the associated User entity for a given ModInst entity.
getLecturerModsUser :: ModInst -> Transaction User
getLecturerModsUser mUser = getUser (modInstUserLecturerModsKey mUser)
