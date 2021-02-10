module Controller.StudyProgram
  ( mainStudyProgramController, newStudyProgramWuiForm, editStudyProgramForm )
 where

import Directory
import Global
import IO     ( hPutStr, hClose )
import IOExts ( connectToCommand, evalCmd, readCompleteFile )
import List   ( (\\), nub )
import Maybe
import System
import Time

import HTML.Base
import HTML.Session
import HTML.Styles.Bootstrap4 ( ehrefScndBadge )
import HTML.WUI
import ShowDotGraph ( showDotGraph )

import Config.EntityRoutes
import MDB
import SpecialQueries
import System.Helpers
import System.Spicey
import View.StudyProgram
import System.Authorization
import System.AuthorizedActions
import Config.UserProcesses
import System.Authentication
import System.MultiLang
import System.SessionInfo
import View.MDBEntitiesToHtml

--- Choose the controller for a StudyProgram entity according to the URL parameter.
mainStudyProgramController :: Controller
mainStudyProgramController =
  do args <- getControllerParams
     case args of
      [] -> listStudyProgramController
      ["list"] -> listStudyProgramController
      ["new"] -> newStudyProgramController
      ["show" ,s] -> controllerOnKey s showStudyProgramController
      ["edit" ,s] -> controllerOnKey s editStudyProgramController
      ["delete" ,s] -> controllerOnKey s confirmDeleteStudyProgramController
      ["destroy" ,s] -> controllerOnKey s destroyStudyProgramController
      ["prereqs" ,s] -> controllerOnKey s showPrereqsStudyProgramController
      _ -> displayError "Illegal URL"

------------------------------------------------------------------------------
-- Shows a form to create a new StudyProgram entity.
newStudyProgramController :: Controller
newStudyProgramController =
  checkAuthorization (studyProgramOperationAllowed NewEntity) $ \sinfo -> do
    setParWuiStore newStudyProgramWuiStore sinfo ("", "", "", "", 0)
    return [formElem newStudyProgramWuiForm]

type NewStudyProgram = (String,String,String,String,Int)

--- A WUI form to create a new StudyProgram entity.
--- The default values for the fields are stored in the
--- `newStudyProgramWuiStore`.
newStudyProgramWuiForm ::
  HtmlFormDef (UserSessionInfo, WuiStore NewStudyProgram)
newStudyProgramWuiForm =
  pwui2FormDef "Controller.StudyProgram.newStudyProgramWuiForm"
    newStudyProgramWuiStore
    (\_ -> wStudyProgram)
    (\_ entity ->
       checkAuthorization (studyProgramOperationAllowed NewEntity) $ \_ ->
         transactionController (createStudyProgramT entity)
           (nextInProcessOr listStudyProgramController Nothing))
    (\sinfo ->
       renderWUI sinfo "Create new StudyProgram" "Create"
                 "?StudyProgram/list" ())

---- The data stored for executing the WUI form.
newStudyProgramWuiStore ::
  Global (SessionStore (UserSessionInfo, WuiStore NewStudyProgram))
newStudyProgramWuiStore =
  global emptySessionStore (Persistent (inSessionDataDir "newStudyProgramWuiStore"))

--- Transaction to persist a new StudyProgram entity to the database.
createStudyProgramT :: (String,String,String,String,Int) -> DBAction ()
createStudyProgramT (name,nameE,shortName,progKey,position) =
  newStudyProgram name nameE shortName progKey position |>> returnT ()

------------------------------------------------------------------------------
--- Shows a form to edit the given StudyProgram entity.
editStudyProgramController :: StudyProgram -> Controller
editStudyProgramController studyprog =
  checkAuthorization (studyProgramOperationAllowed (UpdateEntity studyprog))
   $ \sinfo -> do
    setParWuiStore wuiEditStudyProgramStore (sinfo,studyprog) studyprog
    return [formElem editStudyProgramForm]

--- A form to edit the given StudyProgram entity.
editStudyProgramForm ::
  HtmlFormDef ((UserSessionInfo,StudyProgram), WuiStore StudyProgram)
editStudyProgramForm = pwui2FormDef "Controller.StudyProgram.editStudyProgramForm"
  wuiEditStudyProgramStore
  (\ (_,studyprog) -> wStudyProgramType studyprog)
  (\ (_,studyprog) entity ->
     checkAuthorization (studyProgramOperationAllowed (UpdateEntity studyprog))
      $ \_ -> updateStudyProgramController entity)
  (\ (sinfo,_) ->
      renderWUI sinfo "Studienprogramm ändern" "Change"
                "?StudyProgram/list" ())

--- The data stored for executing the WUI form.
wuiEditStudyProgramStore ::
  Global (SessionStore ((UserSessionInfo,StudyProgram), WuiStore StudyProgram))
wuiEditStudyProgramStore =
  global emptySessionStore
         (Persistent (inSessionDataDir "wuiEditStudyProgramStore"))

--- Persists modifications of a given StudyProgram entity.
updateStudyProgramController :: StudyProgram -> Controller
updateStudyProgramController studyProgram =
  runT (updateStudyProgram studyProgram) >>=
  either (\ error -> displayError (showTError error))
         (\ _ -> nextInProcessOr listStudyProgramController Nothing)

------------------------------------------------------------------------------
--- Deletes a given StudyProgram entity (after asking for confirmation)
--- and proceeds with the list controller.
confirmDeleteStudyProgramController :: StudyProgram -> Controller
confirmDeleteStudyProgramController studyProgram =
  checkAuthorization
   (studyProgramOperationAllowed (DeleteEntity studyProgram)) $ \si ->
   confirmDeletionPage si $ concat
     ["Really delete entity \"", studyProgramToShortView studyProgram, "\"?"]

--- Deletes a given StudyProgram entity (depending on the Boolean
--- argument) and proceeds with the list controller.
destroyStudyProgramController :: StudyProgram -> Controller
destroyStudyProgramController studyProgram =
  checkAuthorization
   (studyProgramOperationAllowed (DeleteEntity studyProgram)) $ \_ ->
   (runT (deleteStudyProgram studyProgram) >>=
    either (\error -> displayError (showTError error))
           (\ _ -> listStudyProgramController))

--- Lists all StudyProgram entities with buttons to show, delete,
--- or edit an entity.
listStudyProgramController :: Controller
listStudyProgramController =
  checkAuthorization (studyProgramOperationAllowed ListEntities) $ \sinfo ->
   (do studyPrograms <- runQ queryAllStudyPrograms
       return (listStudyProgramView sinfo studyPrograms))

--- Shows a StudyProgram entity.
showStudyProgramController :: StudyProgram -> Controller
showStudyProgramController studyProgram =
  checkAuthorization (studyProgramOperationAllowed (ShowEntity studyProgram))
   $ \_ ->
   (do return (showStudyProgramView studyProgram))

--- Shows the prerequisites of the given StudyProgram entity.
showPrereqsStudyProgramController :: StudyProgram -> Controller
showPrereqsStudyProgramController sprog =
  checkAuthorization (studyProgramOperationAllowed (ShowEntity sprog))
   $ \sinfo -> do
    let t = translate sinfo
    pid <- getPID
    let tmppdf = "tmp_" ++ show pid ++ ".pdf"
    pdfexists <- doesFileExist tmppdf
    if pdfexists then system ("chmod 644 " ++ tmppdf)
                 else return 0
    mcodes  <- getModuleCodesOfStudyProg sprog
    prereqs <- getStudyProgRequirements sprog
    let prereqmods = nub (concatMap (\ (x,y) -> [x,y]) prereqs)
    let basemods  = filter ((`notElem` prereqmods) . snd) mcodes
        dotgraph  = showDotGraph (depsToGraph prereqs)
        dotpdfcmd = "/usr/bin/dot -Tpdf -o" ++ tmppdf
    dotstr <- connectToCommand dotpdfcmd
    hPutStr dotstr dotgraph
    hClose dotstr
    (_,svgtxt,_) <- evalCmd "/usr/bin/dot" ["-Tsvg"] dotgraph
    basemoddatas <- runJustT $ mapM getModData (map fst basemods)
    return [ h1 [ htxt $ t "Module dependencies", htxt ": "
                , studyProgramToHRef sinfo sprog]
           , h2 [ htxt $ t "Module dependencies", htxt ": "
                , ehrefScndBadge tmppdf [htxt "PDF"]]
           , block [htmlText svgtxt]
           , h3 [htxt $ t "Modules without prerequisites", htxt ":"]
           , showModDatasAsLinks sinfo basemoddatas
            ]
