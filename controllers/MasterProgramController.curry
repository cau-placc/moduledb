module MasterProgramController (
 newMasterProgramController, editMasterProgramController,
 deleteMasterProgramController, listMasterProgramController
 ) where

import Spicey
import KeyDatabase
import HTML
import Time
import MDB
import MasterProgramView
import Maybe
import Authorization
import AuthorizedControllers
import UserProcesses
import Authentication
import MasterProgInfoController
import DefaultController
import List
import Helpers

--- Shows a form to create a new MasterProgram entity.
newMasterProgramController :: Controller
newMasterProgramController =
  checkAuthorization (masterProgramOperationAllowed NewEntity) $ do
    allMasterCoreAreas <- runQ queryAllMasterCoreAreas
    allUsers <- runQ queryAllUsers
    login <- getSessionLogin
    let mbuser = maybe Nothing
                       (\ln -> find (\u -> userLogin u == ln) allUsers)
                       login
    maybe (return [h1 [htxt "Illegal operation"]])
          (\u -> do mprogs <- runQ $ queryMasterProgramOfUser (userKey u)
                    return $ blankMasterProgramView u mprogs allMasterCoreAreas
                               createMasterProgramController)
          mbuser

--- Persists a new MasterProgram entity to the database.
createMasterProgramController
 :: Bool -> (String,Maybe MasterProgram,String,Int,String,String,String,
             Bool,MasterCoreArea,User) -> Controller
createMasterProgramController False _ = defaultController
createMasterProgramController True
   (name,mbmprog,term,year,desc,prereq,comments,visible,masterCoreArea,user) =
  runT (maybe
         (newMasterProgramWithUserAdvisingKeyWithMasterCoreAreaAreaProgramsKey
            name term (Just year) desc prereq comments visible
            (userKey user) (masterCoreAreaKey masterCoreArea))
         (\mp ->
          newMasterProgramWithUserAdvisingKeyWithMasterCoreAreaAreaProgramsKey
            (masterProgramName mp) term (Just year)
            (masterProgramDesc mp) (masterProgramPrereq mp)
            (masterProgramComments mp) visible (userKey user)
            (masterProgramMasterCoreAreaAreaProgramsKey mp))
         mbmprog |>>= \mprog ->
        newMasterProgInfoWithMasterProgramProgramInfoKey
          "[]" radv radv radv aadv aadv (masterProgramKey mprog) |>>= \mpi ->
        returnT (mprog,mpi) ) >>=
  either (\ (mp,mpi) ->
           logEvent (NewMasterProgram mp) >>
           logEvent (NewMasterProgInfo mpi) >>
           nextInProcessOr
            (return
              [h2 [htxt "Masterprogramm angelegt. Bitte die weiteren Angaben ",
                   href ("?listMasterProgram/"++showMasterProgramKey mp)
                        [htxt "hier"], htxt " ergänzen!"]])
            Nothing)
         (\ error -> displayError (showTError error))
 where
   radv = "beim Research Advisor"
   aadv = "nach Wahl der Studierenden in Absprache mit dem Academic Advisor"

--- Shows a form to edit the given MasterProgram entity.
editMasterProgramController :: MasterProgram -> Controller
editMasterProgramController mprog =
  checkAuthorization
   (masterProgramOperationAllowed (UpdateEntity mprog)) $
   (do allMasterCoreAreas <- runQ queryAllMasterCoreAreas
       allUsers <- runQ queryAllUsers
       admin    <- isAdmin
       areaProgramsMasterCoreArea <- runJustT
                                      (getAreaProgramsMasterCoreArea mprog)
       advisingUser <- runJustT (getAdvisingUser mprog)
       return
        (editMasterProgramView admin mprog
          areaProgramsMasterCoreArea
          advisingUser allMasterCoreAreas allUsers
          updateMasterProgramController))

--- Persists modifications of a given MasterProgram entity to the
--- database depending on the Boolean argument. If the Boolean argument
--- is False, nothing is changed.
updateMasterProgramController :: Bool -> MasterProgram -> Controller
updateMasterProgramController False _ = listMasterProgramController
updateMasterProgramController True mprog =
  isAdmin >>= \admin ->
  runT ((if masterProgramVisible mprog
         then getDB (queryInfoOfMasterProgram (masterProgramKey mprog)) |>>=
              maybe (returnT "") reasonableMasterProgInfo
         else returnT "") |>>= \reas ->
        if null reas || admin then updateMasterProgram mprog |>> returnT ""
        else updateMasterProgram (setMasterProgramVisible mprog False) |>>
             returnT ("Masterprogramm nicht sichtbar, denn "++reas)) >>=
  either (\ reas -> logEvent (UpdateMasterProgram mprog) >>
                    (if null reas then done else setPageMessage reas) >>
                    nextInProcessOr listMasterProgramController Nothing)
         (\ error -> displayError (showTError error))

--- Deletes a given MasterProgram entity (depending on the Boolean
--- argument) and proceeds with the list controller.
deleteMasterProgramController :: MasterProgram -> Bool -> Controller
deleteMasterProgramController _ False = defaultController
deleteMasterProgramController mprog True =
  checkAuthorization
   (masterProgramOperationAllowed (DeleteEntity mprog)) $
     runT (getDB (queryInfoOfMasterProgram (masterProgramKey mprog)) |>>= \mpi->
           maybe doneT deleteMasterProgInfo mpi |>>
           deleteMasterProgram mprog |>> returnT mpi) >>=
     either (\ mpi -> logEvent (DeleteMasterProgram mprog) >>
                      maybe done (logEvent . DeleteMasterProgInfo) mpi >>
                      defaultController)
            (\ error -> displayError (showTError error))

--- Lists all MasterProgram entities with buttons to show, delete,
--- or edit an entity.
listMasterProgramController :: Controller
listMasterProgramController =
  getControllerParams >>= listMasterProgramControllerWithParams

listMasterProgramControllerWithParams :: [String] -> Controller
listMasterProgramControllerWithParams args =
  checkAuthorization (masterProgramOperationAllowed ListEntities) $ do
    admin <- isAdmin
    login <- getSessionLogin
    if null args
     then do masterPrograms <- runQ queryAllMasterPrograms
             coreareas <- runQ queryAllMasterCoreAreas
             return (listMasterProgramView admin
                       (maybe (filter masterProgramVisible masterPrograms)
                              (const masterPrograms) login)
                       coreareas
                       showMasterProgramController
                       editMasterProgramController
                       deleteMasterProgramController)
     else maybe (displayError "Illegal URL")
            (\mpkey -> do
              mprog <- runJustT $ getMasterProgram mpkey
              runQ (queryInfoOfMasterProgram mpkey) >>=
               maybe (displayError "Illegal URL")
                (\mpinfo -> do
                  let modinfo = progModsOfMasterProgInfo mpinfo
                  tmodinfo <- runJustT $ mapT getMCodeForInfo modinfo
                  mcarea <- runJustT $ getMasterCoreArea
                             (masterProgramMasterCoreAreaAreaProgramsKey mprog)
                  responsibleUser <- runJustT (getAdvisingUser mprog)
                  lname <- getSessionLogin >>= return . maybe "" id
                  let semyr = (masterProgramTerm mprog,masterProgramYear mprog)
                  return
                    (singleMasterProgramView admin
                      (userLogin responsibleUser == lname) responsibleUser
                      mprog mpinfo tmodinfo mcarea
                      showMasterProgramController
                      editMasterProgramController
                      deleteMasterProgramController
                      (editMasterProgInfoController semyr
                                                 listMasterProgramController))
                )
            )
            (readMasterProgramKey (head args))
 where
  getMCodeForInfo (c,b,mk,t,y) =
    getModData (fromJust (readModDataKey mk)) |>>= \mod ->
    returnT (c,b,mod,t,y)

--- Shows a MasterProgram entity.
showMasterProgramController :: MasterProgram -> Controller
showMasterProgramController masterProgram =
  checkAuthorization
   (masterProgramOperationAllowed (ShowEntity masterProgram)) $
   (do areaProgramsMasterCoreArea <- runJustT
                                      (getAreaProgramsMasterCoreArea
                                        masterProgram)
       advisingUser <- runJustT (getAdvisingUser masterProgram)
       return
        (showMasterProgramView masterProgram areaProgramsMasterCoreArea
          advisingUser listMasterProgramController))

--- Gets the associated MasterCoreArea entity for a given MasterProgram entity.
getAreaProgramsMasterCoreArea :: MasterProgram -> Transaction MasterCoreArea
getAreaProgramsMasterCoreArea mMasterCoreArea =
  getMasterCoreArea
   (masterProgramMasterCoreAreaAreaProgramsKey mMasterCoreArea)

--- Gets the associated User entity for a given MasterProgram entity.
getAdvisingUser :: MasterProgram -> Transaction User
getAdvisingUser mUser = getUser (masterProgramUserAdvisingKey mUser)


--- Get module instances of next n semesters from a given one:
queryModInstInSemesters :: (String,Int) -> Int -> Query [ModInst]
queryModInstInSemesters semyear n =
  let nextsems = take n (iterate nextSemester semyear)
   in queryCondModInst (\mi -> (modInstTerm mi,modInstYear mi) `elem` nextsems)

--- Get module instances and their categories
--- of next n semesters from a given one:
getModInstCatsInSemesters :: (String,Int) -> Int
                          -> Transaction [(ModInst,[Category])]
getModInstCatsInSemesters semyear n =
  getDB (queryModInstInSemesters semyear n) |>>= \mis ->
  --mapT (\mi -> getModDataKeyCategorys (modInstModDataModuleInstancesKey mi)
  --              |>>= \cats -> returnT (mi,cats)) mis
  mapT (\mdk -> getModDataKeyCategorys mdk |>>= \cats -> returnT (mdk,cats))
       (nub (map modInstModDataModuleInstancesKey mis)) |>>= \mdkcats ->
  returnT
   (map (\mi -> (mi,fromJust
                  (lookup (modInstModDataModuleInstancesKey mi) mdkcats))) mis)

--- Get module instances (and their categories) belonging to given
--- list of categories (specified by their CatKeys)
--- of next n semesters from a given one:
getCategoryModInstInSemesters :: (String,Int) -> Int -> [String]
                          -> Transaction [(ModInst,[Category])]
getCategoryModInstInSemesters semyear n catkeys =
  getModInstCatsInSemesters semyear n |>>=
  returnT .
    (filter (\ (_,cats) -> any (\c -> categoryCatKey c `elem` catkeys) cats))

--- Get master module instances (and their module data and categories)
--- of next n semesters from a given one:
getMasterModInstInSemesters :: (String,Int) -> Int
                          -> Transaction [(ModInst,ModData,[Category])]
getMasterModInstInSemesters semyear n =
  getCategoryModInstInSemesters semyear n ["IG","TG","IS","MV"] |>>=
  mapT (\ (mi,cats) -> getModData (modInstModDataModuleInstancesKey mi)
                       |>>= \md -> returnT (mi,md,cats))
