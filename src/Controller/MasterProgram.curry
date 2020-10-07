module Controller.MasterProgram (
 mainMasterProgramController,
 showAllXmlMasterPrograms,showXmlMasterProgram) where

import Database.CDBI.ER

import System.Spicey
import HTML.Base
import Time
import ConfigMDB
import MDB
import MDBExts
import View.MDBEntitiesToHtml
import View.MasterProgram
import Maybe
import System.Authorization
import System.AuthorizedActions
import Config.UserProcesses
import System.Authentication
import Controller.MasterProgInfo
import List
import System.Helpers
import XML
import System.SessionInfo
import System.MultiLang

--- Choose the controller for a MasterProgram entity according to the URL parameter.
mainMasterProgramController :: Controller
mainMasterProgramController =
  do args <- getControllerParams
     case args of
      [] -> listMasterProgramController False
      ["list"] -> listMasterProgramController False
      ["listall"] -> listMasterProgramController True
      --["new"] -> newMasterProgramController
      ["show" ,s] -> applyControllerOn (readMasterProgramKey s) getMasterProgram
                                       showMasterProgramController
      --["edit" ,s] ->
      -- applyControllerOn (readMasterProgramKey s) getMasterProgram
      --  editMasterProgramController
      --["delete" ,s] ->
      -- applyControllerOn (readMasterProgramKey s) getMasterProgram
      --  confirmDeleteMasterProgramController
      _ -> displayUrlError


--- Lists all MasterProgram entities with buttons to show, delete,
--- or edit an entity.
listMasterProgramController :: Bool -> Controller
listMasterProgramController listall =
  checkAuthorization (masterProgramOperationAllowed ListEntities) $ \sinfo ->
   do
    allmpinfos <- runQ queryMasterProgramMainInfos
    csem       <- getCurrentSemester
    let mpinfos = if listall then allmpinfos
                             else filter (currentProgram csem) allmpinfos
    coreareas <- runQ queryAllMasterCoreAreas
    return (listMasterProgramView sinfo listall
              (maybe (filter visibleProgram mpinfos)
                     (const mpinfos) (userLoginOfSession sinfo))
              coreareas)
 where
  -- is a master program a current one?
  currentProgram cursem (_,_,term,year,_,_) =
    leqSemester (prevSemester (prevSemester cursem)) (term,year)

  visibleProgram (_,_,_,_,vis,_) = vis


-- Transform the module table by replacing the ModData key with
-- the actual ModData:
getMCodeForInfo :: (a, b, String, c, d) -> DBAction (a, b, ModData, c, d)
getMCodeForInfo (c,b,mk,t,y) = do
  mod <- getModData (fromJust (readModDataKey mk))
  return (c,b,mod,t,y)

--- Shows a MasterProgram entity.
showMasterProgramController :: MasterProgram -> Controller
showMasterProgramController mprog =
  checkAuthorization
   (masterProgramOperationAllowed (ShowEntity mprog)) $ \_ -> do
      runQ (queryInfoOfMasterProgram (masterProgramKey mprog)) >>=
       maybe (displayError "Illegal Master Program")
        (\mpinfo -> do
          let modinfo = progModsOfMasterProgInfo mpinfo
          tmodinfo <- runJustT $ mapM getMCodeForInfo modinfo
          mcarea <- runJustT $ getMasterCoreArea
                     (masterProgramMasterCoreAreaAreaProgramsKey mprog)
          responsibleUser <- runJustT (getAdvisingUser mprog)
          --let semyr = (masterProgramTerm mprog,masterProgramYear mprog)
          return
            (singleMasterProgramView responsibleUser
               mprog mpinfo tmodinfo mcarea (xmlURL mprog))
        )

--- Gets the associated MasterCoreArea entity for a given MasterProgram entity.
getAreaProgramsMasterCoreArea :: MasterProgram -> DBAction MasterCoreArea
getAreaProgramsMasterCoreArea mMasterCoreArea =
  getMasterCoreArea
   (masterProgramMasterCoreAreaAreaProgramsKey mMasterCoreArea)


--- Get module instances of next n semesters from a given one:
queryModInstInSemesters :: (String,Int) -> Int -> DBAction [ModInst]
queryModInstInSemesters semyear n =
  let nextsems = take n (iterate nextSemester semyear)
   in queryCondModInst (\mi -> (modInstTerm mi,modInstYear mi) `elem` nextsems)

--- Get module instances and their categories
--- of next n semesters from a given one:
getModInstCatsInSemesters :: (String,Int) -> Int
                          -> DBAction [(ModInst,[Category])]
getModInstCatsInSemesters semyear n = do
  mis <- queryModInstInSemesters semyear n
  mdkcats <-
    mapM (\mdk -> getModDataKeyCategories mdk >>= \cats -> return (mdk,cats))
         (nub (map modInstModDataModuleInstancesKey mis))
  return $
   map (\mi -> (mi,fromJust
                    (lookup (modInstModDataModuleInstancesKey mi) mdkcats)))
       mis

--- Get module instances (and their categories) belonging to given
--- list of categories (specified by their ShortNames)
--- of next n semesters from a given one:
getCategoryModInstInSemesters :: (String,Int) -> Int -> [String]
                          -> DBAction [(ModInst,[Category])]
getCategoryModInstInSemesters semyear n catkeys =
  getModInstCatsInSemesters semyear n |>>=
  returnT .
    (filter (\ (_,cats) -> any (\c -> categoryShortName c `elem` catkeys) cats))

--- Get master module instances (and their module data and categories)
--- of next n semesters from a given one:
getMasterModInstInSemesters :: (String,Int) -> Int
                          -> DBAction [(ModInst,ModData,[Category])]
getMasterModInstInSemesters semyear n =
  getCategoryModInstInSemesters semyear n ["IG","TG","IS","MV"] |>>=
  mapM (\ (mi,cats) -> getModData (modInstModDataModuleInstancesKey mi)
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
showAllXmlMasterPrograms :: IO HtmlPage
showAllXmlMasterPrograms = do
  allmprogs <- runQ $ liftM (filter masterProgramVisible) queryAllMasterPrograms
  mpxmls <- mapIO getMasterProgramXML allmprogs
  return (HtmlAnswer "text/xml"
                     (showXmlDoc (xml "studyprograms" (catMaybes mpxmls))))

showXmlMasterProgram :: MasterProgramID -> IO HtmlPage
showXmlMasterProgram mpkey = do
  mprog <- runJustT $ getMasterProgram mpkey
  mbxml <- getMasterProgramXML mprog
  maybe (displayUrlError >>= getPage)
        (\xdoc -> return (HtmlAnswer "text/xml" (showXmlDoc xdoc)))
        mbxml

getMasterProgramXML :: MasterProgram -> IO (Maybe XmlExp)
getMasterProgramXML mprog =
  runQ (queryInfoOfMasterProgram (masterProgramKey mprog)) >>=
  maybe (return Nothing)
    (\mpinfo -> do
      let modinfo = progModsOfMasterProgInfo mpinfo
      tmodinfo <- runJustT $ mapM getMCodeForInfo modinfo
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