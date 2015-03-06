module MasterProgramController (
 mainMasterProgramController,
 showAllXmlMasterPrograms,showXmlMasterProgram) where

import Spicey
import KeyDatabase
import HTML
import Time
import ConfigMDB
import MDB
import MDBExts
import MDBEntitiesToHtml
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
import XML
import SessionInfo
import MultiLang

--- Choose the controller for a MasterProgram entity according to the URL parameter.
mainMasterProgramController :: Controller
mainMasterProgramController =
  do args <- getControllerParams
     case args of
      [] -> listMasterProgramController False
      ["list"] -> listMasterProgramController False
      ["listall"] -> listMasterProgramController True
      ["new"] -> newMasterProgramController
      ["show" ,s] -> applyControllerOn (readMasterProgramKey s) getMasterProgram
                                       showMasterProgramController
      --["edit" ,s] ->
      -- applyControllerOn (readMasterProgramKey s) getMasterProgram
      --  editMasterProgramController
      --["delete" ,s] ->
      -- applyControllerOn (readMasterProgramKey s) getMasterProgram
      --  confirmDeleteMasterProgramController
      _ -> displayError "Illegal URL"

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
                   href ("?MasterProgram/show/"++showMasterProgramKey mp)
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
updateMasterProgramController False mprog = showMasterProgramController mprog
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
                    nextInProcessOr (showMasterProgramController mprog) Nothing)
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
listMasterProgramController :: Bool -> Controller
listMasterProgramController listall =
  checkAuthorizationWI (masterProgramOperationAllowed ListEntities) $ \sinfo ->
   do
    allmpinfos <- runQ queryMasterProgramMainInfos
    let mpinfos = if listall then allmpinfos
                             else filter currentProgram allmpinfos
    coreareas <- runQ queryAllMasterCoreAreas
    return (listMasterProgramView sinfo listall
              (maybe (filter visibleProgram mpinfos)
                     (const mpinfos) (userLoginOfSession sinfo))
              coreareas)
 where
  -- is a master program a current one?
  currentProgram (_,_,term,year,_,_) =
    leqSemester (prevSemester (prevSemester currentSemester)) (term,year)

  visibleProgram (_,_,_,_,vis,_) = vis


-- Transform the module table by replacing the ModData key with
-- the actual ModData:
getMCodeForInfo (c,b,mk,t,y) =
  getModData (fromJust (readModDataKey mk)) |>>= \mod ->
  returnT (c,b,mod,t,y)

--- Shows a MasterProgram entity.
showMasterProgramController :: MasterProgram -> Controller
showMasterProgramController mprog =
  checkAuthorizationWI
   (masterProgramOperationAllowed (ShowEntity mprog)) $ \sinfo -> do
      runQ (queryInfoOfMasterProgram (masterProgramKey mprog)) >>=
       maybe (displayError "Illegal Master Program")
        (\mpinfo -> do
          admin <- isAdmin
          let modinfo = progModsOfMasterProgInfo mpinfo
          tmodinfo <- runJustT $ mapT getMCodeForInfo modinfo
          mcarea <- runJustT $ getMasterCoreArea
                     (masterProgramMasterCoreAreaAreaProgramsKey mprog)
          responsibleUser <- runJustT (getAdvisingUser mprog)
          let semyr = (masterProgramTerm mprog,masterProgramYear mprog)
          return
            (singleMasterProgramView admin
               (Just (userLogin responsibleUser)
                   == userLoginOfSession sinfo)
               responsibleUser
               mprog mpinfo tmodinfo mcarea (xmlURL mprog)
               showMasterProgramController
               editMasterProgramController
               deleteMasterProgramController
               (editMasterProgInfoController semyr
                  (showMasterProgramController mprog)))
        )

--- Gets the associated MasterCoreArea entity for a given MasterProgram entity.
getAreaProgramsMasterCoreArea :: MasterProgram -> Transaction MasterCoreArea
getAreaProgramsMasterCoreArea mMasterCoreArea =
  getMasterCoreArea
   (masterProgramMasterCoreAreaAreaProgramsKey mMasterCoreArea)


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

-------------------------------------------------------------------------
-- Formatting master programs as XML documents:

-- XML URL of a master program:
xmlURL :: MasterProgram -> String
xmlURL mp = baseURL++"?xmlprog="++string2urlencoded (showMasterProgramKey mp)

-- URL of a master program:
masterProgURL :: MasterProgram -> String
masterProgURL mp =
  baseURL++"?MasterProgram/show/"++string2urlencoded (showMasterProgramKey mp)

-- Show XML document containing all visible master programs
showAllXmlMasterPrograms :: IO HtmlForm
showAllXmlMasterPrograms = do
  allmprogs <- runQ $ transformQ (filter masterProgramVisible)
                                 queryAllMasterPrograms
  mpxmls <- mapIO getMasterProgramXML allmprogs
  return (HtmlAnswer "text/xml"
             (showXmlDoc (xml "studyprograms" (catMaybes mpxmls))))

showXmlMasterProgram :: MasterProgramKey -> IO HtmlForm
showXmlMasterProgram mpkey = do
  mprog <- runJustT $ getMasterProgram mpkey
  mbxml <- getMasterProgramXML mprog
  maybe (displayError "Illegal URL" >>= getForm)
        (\xdoc -> return (HtmlAnswer "text/xml" (showXmlDoc xdoc)))
        mbxml

getMasterProgramXML :: MasterProgram -> IO (Maybe XmlExp)
getMasterProgramXML mprog =
  runQ (queryInfoOfMasterProgram (masterProgramKey mprog)) >>=
  maybe (return Nothing)
    (\mpinfo -> do
      let modinfo = progModsOfMasterProgInfo mpinfo
      tmodinfo <- runJustT $ mapT getMCodeForInfo modinfo
      responsibleUser <- runJustT (getAdvisingUser mprog)
      return (Just (mprog2xml mprog responsibleUser tmodinfo)))

mprog2xml :: MasterProgram -> User -> [(String,Bool,ModData,String,Int)]
          -> XmlExp
mprog2xml mprog advisor modinfo =
  XElem "studyprogram" [("ID",showMasterProgramKey mprog)] $
   [xml "title"         [xtxt (masterProgramName mprog)]
   ,xml "advisor"       [xtxt (userToShortView advisor)]
   ,xml "start"         [xtxt (showSemester (startSem,startYear))]
   ,xml "url"           [xtxt (masterProgURL mprog)]
   ,xml "description"   [xtxt (masterProgramDesc mprog)]
   ,xml "prerequisites" [xtxt (masterProgramPrereq mprog)]
   ,xml "comments"      [xtxt (masterProgramComments mprog)]
   ,XElem "degreeprogram" [("key","MSc")] [xtxt "Masterstudiengang Informatik"]
   ] ++ map modinfo2xml modinfo
 where
  modinfo2xml (c,p,md,sm,yr) =
    xml "lecture"
     [xml "mandatory" [xtxt $ if p then "yes" else "no"]
     ,xml "category"  [xtxt ("MSc_"++c)]
     ,xml "code"      [xtxt (modDataCode md)]
     ,xml "semester"  [xtxt (showSemester (sm,yr))]
     ]

  startSem = masterProgramTerm mprog
  startYear = masterProgramYear mprog

-------------------------------------------------------------------------