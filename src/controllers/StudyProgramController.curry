module StudyProgramController (
 mainStudyProgramController
 ) where

import Spicey
import Transaction
import HTML.Base
import Time
import MDB
import StudyProgramView
import Maybe
import Authorization
import AuthorizedControllers
import UserProcesses
import Authentication
import SessionInfo
import MDBEntitiesToHtml

--- Choose the controller for a StudyProgram entity according to the URL parameter.
mainStudyProgramController :: Controller
mainStudyProgramController =
  do args <- getControllerParams
     case args of
      [] -> listStudyProgramController
      ["list"] -> listStudyProgramController
      ["new"] -> newStudyProgramController
      ["show" ,s] ->
       applyControllerOn (readStudyProgramKey s) getStudyProgram
        showStudyProgramController
      ["edit" ,s] ->
       applyControllerOn (readStudyProgramKey s) getStudyProgram
        editStudyProgramController
      ["delete" ,s] ->
       applyControllerOn (readStudyProgramKey s) getStudyProgram
        confirmDeleteStudyProgramController
      _ -> displayError "Illegal URL"

--- Shows a form to create a new StudyProgram entity.
newStudyProgramController :: Controller
newStudyProgramController =
  checkAuthorization (studyProgramOperationAllowed NewEntity)
   $ (\sinfo ->
     do return
         (blankStudyProgramView sinfo
           (\entity ->
             transactionController (createStudyProgramT entity)
              (nextInProcessOr listStudyProgramController Nothing))
           listStudyProgramController))

--- Transaction to persist a new StudyProgram entity to the database.
createStudyProgramT :: (String,String,String,String,Int) -> Transaction ()
createStudyProgramT (name,nameE,shortName,progKey,position) =
  newStudyProgram name nameE shortName progKey position |>> returnT ()

--- Shows a form to edit the given StudyProgram entity.
editStudyProgramController :: StudyProgram -> Controller
editStudyProgramController studyProgramToEdit =
  checkAuthorization
   (studyProgramOperationAllowed (UpdateEntity studyProgramToEdit)) $ \_ ->
   (do return
        (editStudyProgramView studyProgramToEdit
          updateStudyProgramController))

--- Persists modifications of a given StudyProgram entity to the
--- database depending on the Boolean argument. If the Boolean argument
--- is False, nothing is changed.
updateStudyProgramController :: Bool -> StudyProgram -> Controller
updateStudyProgramController False _ = listStudyProgramController
updateStudyProgramController True studyProgram =
  do transResult <- runT (updateStudyProgram studyProgram)
     flip either (\ _ -> nextInProcessOr listStudyProgramController Nothing)
      (\ error -> displayError (showTError error)) transResult

--- Deletes a given StudyProgram entity (after asking for confirmation)
--- and proceeds with the list controller.
confirmDeleteStudyProgramController :: StudyProgram -> Controller
confirmDeleteStudyProgramController studyProgram =
  checkAuthorization
   (studyProgramOperationAllowed (DeleteEntity studyProgram)) $ \_ ->
   confirmControllerOLD
    (h3
      [htxt
        (concat
          ["Really delete entity \"",studyProgramToShortView studyProgram
          ,"\"?"])])
    (\ ack -> if ack
               then deleteStudyProgramController studyProgram
               else showStudyProgramController studyProgram)

--- Deletes a given StudyProgram entity (depending on the Boolean
--- argument) and proceeds with the list controller.
deleteStudyProgramController :: StudyProgram -> Controller
deleteStudyProgramController studyProgram =
  checkAuthorization
   (studyProgramOperationAllowed (DeleteEntity studyProgram)) $ \_ ->
   (do transResult <- runT (deleteStudyProgram studyProgram)
       flip either (\ _ -> listStudyProgramController)
        (\ error -> displayError (showTError error)) transResult)

--- Lists all StudyProgram entities with buttons to show, delete,
--- or edit an entity.
listStudyProgramController :: Controller
listStudyProgramController =
  checkAuthorization (studyProgramOperationAllowed ListEntities) $ \_ ->
   (do sinfo <- getUserSessionInfo
       studyPrograms <- runQ queryAllStudyPrograms
       return (listStudyProgramView sinfo studyPrograms))

--- Shows a StudyProgram entity.
showStudyProgramController :: StudyProgram -> Controller
showStudyProgramController studyProgram =
  checkAuthorization (studyProgramOperationAllowed (ShowEntity studyProgram))
   $ \_ ->
   (do return (showStudyProgramView studyProgram))
