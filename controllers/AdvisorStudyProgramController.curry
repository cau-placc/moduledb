module AdvisorStudyProgramController ( mainAdvisorStudyProgramController )
where

import Spicey
import KeyDatabase
import HTML
import Time
import MDB
import AdvisorStudyProgramView
import Maybe
import SessionInfo
import Authorization
import AuthorizedControllers
import UserProcesses
import MDBEntitiesToHtml

--- Choose the controller for a AdvisorStudyProgram entity according to the URL parameter.
mainAdvisorStudyProgramController :: Controller
mainAdvisorStudyProgramController =
  do args <- getControllerParams
     case args of
       [] -> listAdvisorStudyProgramController
       ["list"] -> listAdvisorStudyProgramController
       ["new"] -> newAdvisorStudyProgramController
       ["show",s] ->
         applyControllerOn (readAdvisorStudyProgramKey s)
          getAdvisorStudyProgram
          showAdvisorStudyProgramController
       ["edit",s] ->
         applyControllerOn (readAdvisorStudyProgramKey s)
          getAdvisorStudyProgram
          editAdvisorStudyProgramController
       ["delete",s] ->
         applyControllerOn (readAdvisorStudyProgramKey s)
          getAdvisorStudyProgram
          deleteAdvisorStudyProgramController
       _ -> displayError "Illegal URL"

--- Shows a form to create a new AdvisorStudyProgram entity.
newAdvisorStudyProgramController :: Controller
newAdvisorStudyProgramController =
  checkAuthorization (advisorStudyProgramOperationAllowed NewEntity)
   $ (\sinfo ->
     do allStudyPrograms <- runQ queryAllStudyPrograms
        allUsers <- runQ queryAllUsers
        return
         (blankAdvisorStudyProgramView sinfo allStudyPrograms allUsers
           (\entity ->
             transactionController (createAdvisorStudyProgramT entity)
              (nextInProcessOr listAdvisorStudyProgramController Nothing))
           listAdvisorStudyProgramController))

--- Transaction to persist a new AdvisorStudyProgram entity to the database.
createAdvisorStudyProgramT
  :: (String,String,Int,String,String,String,Bool,StudyProgram,User)
  -> Transaction ()
createAdvisorStudyProgramT
    (name,term,year,desc,prereq,comments,visible,studyProgram,user) =
  newAdvisorStudyProgramWithUserStudyAdvisingKeyWithStudyProgramStudyProgramsAdvisedKey
   name
   term
   (Just year)
   desc
   prereq
   comments
   visible
   (userKey user)
   (studyProgramKey studyProgram)
   |>> returnT ()

--- Shows a form to edit the given AdvisorStudyProgram entity.
editAdvisorStudyProgramController :: AdvisorStudyProgram -> Controller
editAdvisorStudyProgramController advisorStudyProgramToEdit =
  checkAuthorization
   (advisorStudyProgramOperationAllowed
     (UpdateEntity advisorStudyProgramToEdit))
   $ (\sinfo ->
     do allStudyPrograms <- runQ queryAllStudyPrograms
        allUsers <- runQ queryAllUsers
        studyProgramsAdvisedStudyProgram <- runJustT
                                             (getStudyProgramsAdvisedStudyProgram
                                               advisorStudyProgramToEdit)
        studyAdvisingUser <- runJustT
                              (getStudyAdvisingUser advisorStudyProgramToEdit)
        return
         (editAdvisorStudyProgramView sinfo advisorStudyProgramToEdit
           studyProgramsAdvisedStudyProgram
           studyAdvisingUser
           allStudyPrograms
           allUsers
           (\entity ->
             transactionController (updateAdvisorStudyProgramT entity)
              (nextInProcessOr listAdvisorStudyProgramController Nothing))
           listAdvisorStudyProgramController))

--- Transaction to persist modifications of a given AdvisorStudyProgram entity
--- to the database.
updateAdvisorStudyProgramT :: AdvisorStudyProgram -> Transaction ()
updateAdvisorStudyProgramT advisorStudyProgram =
  updateAdvisorStudyProgram advisorStudyProgram

--- Deletes a given AdvisorStudyProgram entity (after asking for confirmation)
--- and proceeds with the list controller.
deleteAdvisorStudyProgramController :: AdvisorStudyProgram -> Controller
deleteAdvisorStudyProgramController advisorStudyProgram =
  checkAuthorization
   (advisorStudyProgramOperationAllowed (DeleteEntity advisorStudyProgram))
   $ (\_ ->
     confirmController
      [h3
        [htxt
          (concat
            ["Really delete entity \"",advisorStudyProgramToShortView
                                        advisorStudyProgram,"\"?"])]]
      (transactionController (deleteAdvisorStudyProgramT advisorStudyProgram)
        listAdvisorStudyProgramController)
      (showAdvisorStudyProgramController advisorStudyProgram))

--- Transaction to delete a given AdvisorStudyProgram entity.
deleteAdvisorStudyProgramT :: AdvisorStudyProgram -> Transaction ()
deleteAdvisorStudyProgramT advisorStudyProgram =
  deleteAdvisorStudyProgram advisorStudyProgram

--- Lists all AdvisorStudyProgram entities with buttons to show, delete,
--- or edit an entity.
listAdvisorStudyProgramController :: Controller
listAdvisorStudyProgramController =
  checkAuthorization (advisorStudyProgramOperationAllowed ListEntities)
   $ (\sinfo ->
     do advisorStudyPrograms <- runQ queryAllAdvisorStudyPrograms
        return (listAdvisorStudyProgramView sinfo advisorStudyPrograms))

--- Shows a AdvisorStudyProgram entity.
showAdvisorStudyProgramController :: AdvisorStudyProgram -> Controller
showAdvisorStudyProgramController advisorStudyProgram =
  checkAuthorization
   (advisorStudyProgramOperationAllowed (ShowEntity advisorStudyProgram))
   $ (\sinfo ->
     do studyProgramsAdvisedStudyProgram <- runJustT
                                             (getStudyProgramsAdvisedStudyProgram
                                               advisorStudyProgram)
        studyAdvisingUser <- runJustT
                              (getStudyAdvisingUser advisorStudyProgram)
        return
         (showAdvisorStudyProgramView sinfo advisorStudyProgram
           studyProgramsAdvisedStudyProgram
           studyAdvisingUser))

--- Gets the associated StudyProgram entity for a given AdvisorStudyProgram entity.
getStudyProgramsAdvisedStudyProgram
  :: AdvisorStudyProgram -> Transaction StudyProgram
getStudyProgramsAdvisedStudyProgram aStudyProgram =
  getStudyProgram
   (advisorStudyProgramStudyProgramStudyProgramsAdvisedKey aStudyProgram)

--- Gets the associated User entity for a given AdvisorStudyProgram entity.
getStudyAdvisingUser :: AdvisorStudyProgram -> Transaction User
getStudyAdvisingUser aUser =
  getUser (advisorStudyProgramUserStudyAdvisingKey aUser)