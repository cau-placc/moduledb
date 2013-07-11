module StudyProgramController (
 newStudyProgramController, editStudyProgramController,
 deleteStudyProgramController, listStudyProgramController
 ) where

import Spicey
import KeyDatabase
import HTML
import Time
import MDB
import StudyProgramView
import Maybe
import Authorization
import AuthorizedControllers
import UserProcesses
import Authentication
import UserPreferences

--- Shows a form to create a new StudyProgram entity.
newStudyProgramController :: Controller
newStudyProgramController =
  checkAuthorization (studyProgramOperationAllowed NewEntity) $
   (do return (blankStudyProgramView createStudyProgramController))

--- Persists a new StudyProgram entity to the database.
createStudyProgramController
 :: Bool -> (String,String,String,String,Int) -> Controller
createStudyProgramController False _ = listStudyProgramController
createStudyProgramController True (name ,shortName ,progKey ,uRLKey
                                   ,position) =
  do transResult <- runT
                     (newStudyProgram name shortName progKey uRLKey position)
     either (\ _ -> nextInProcessOr listStudyProgramController Nothing)
      (\ error -> displayError (showTError error)) transResult

--- Shows a form to edit the given StudyProgram entity.
editStudyProgramController :: StudyProgram -> Controller
editStudyProgramController studyProgramToEdit =
  checkAuthorization
   (studyProgramOperationAllowed (UpdateEntity studyProgramToEdit)) $
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
     either (\ _ -> nextInProcessOr listStudyProgramController Nothing)
      (\ error -> displayError (showTError error)) transResult

--- Deletes a given StudyProgram entity (depending on the Boolean
--- argument) and proceeds with the list controller.
deleteStudyProgramController :: StudyProgram -> Bool -> Controller
deleteStudyProgramController _ False = listStudyProgramController
deleteStudyProgramController studyProgram True =
  checkAuthorization
   (studyProgramOperationAllowed (DeleteEntity studyProgram)) $
   (do transResult <- runT (deleteStudyProgram studyProgram)
       either (\ _ -> listStudyProgramController)
        (\ error -> displayError (showTError error)) transResult)

--- Lists all StudyProgram entities with buttons to show, delete,
--- or edit an entity.
listStudyProgramController :: Controller
listStudyProgramController =
  checkAuthorization (studyProgramOperationAllowed ListEntities) $
   (do admin <- isAdmin
       userprefs <- getSessionUserPrefs
       studyPrograms <- runQ queryAllStudyPrograms
       return
        (listStudyProgramView admin userprefs studyPrograms
          showStudyProgramController
          editStudyProgramController deleteStudyProgramController))

--- Shows a StudyProgram entity.
showStudyProgramController :: StudyProgram -> Controller
showStudyProgramController studyProgram =
  checkAuthorization (studyProgramOperationAllowed (ShowEntity studyProgram))
   $
   (do return (showStudyProgramView studyProgram listStudyProgramController))
