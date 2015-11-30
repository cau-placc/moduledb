module AdvisorCategoryController ( mainAdvisorCategoryController ) where

import Spicey
import KeyDatabase
import HTML
import Time
import MDB
import AdvisorCategoryView
import Maybe
import SessionInfo
import Authorization
import AuthorizedControllers
import UserProcesses
import MDBEntitiesToHtml

--- Choose the controller for a AdvisorCategory entity according to the URL parameter.
mainAdvisorCategoryController :: Controller
mainAdvisorCategoryController =
  do args <- getControllerParams
     case args of
       [] -> listAdvisorCategoryController
       ["list"] -> listAdvisorCategoryController
       ["new"] -> newAdvisorCategoryController
       ["show",s] ->
         applyControllerOn (readAdvisorCategoryKey s) getAdvisorCategory
          showAdvisorCategoryController
       ["edit",s] ->
         applyControllerOn (readAdvisorCategoryKey s) getAdvisorCategory
          editAdvisorCategoryController
       ["delete",s] ->
         applyControllerOn (readAdvisorCategoryKey s) getAdvisorCategory
          deleteAdvisorCategoryController
       _ -> displayError "Illegal URL"

--- Shows a form to create a new AdvisorCategory entity.
newAdvisorCategoryController :: Controller
newAdvisorCategoryController =
  checkAuthorization (advisorCategoryOperationAllowed NewEntity)
   $ (\sinfo ->
     do allAdvisorStudyPrograms <- runQ queryAllAdvisorStudyPrograms
        return
         (blankAdvisorCategoryView sinfo allAdvisorStudyPrograms
           (\entity ->
             transactionController (createAdvisorCategoryT entity)
              (nextInProcessOr listAdvisorCategoryController Nothing))
           listAdvisorCategoryController))

--- Transaction to persist a new AdvisorCategory entity to the database.
createAdvisorCategoryT :: (String,Int,AdvisorStudyProgram) -> Transaction ()
createAdvisorCategoryT (comment,position,advisorStudyProgram) =
  newAdvisorCategoryWithAdvisorStudyProgramAdvisedProgramCategoriesKey comment
   position
   (advisorStudyProgramKey advisorStudyProgram)
   |>> returnT ()

--- Shows a form to edit the given AdvisorCategory entity.
editAdvisorCategoryController :: AdvisorCategory -> Controller
editAdvisorCategoryController advisorCategoryToEdit =
  checkAuthorization
   (advisorCategoryOperationAllowed (UpdateEntity advisorCategoryToEdit))
   $ (\sinfo ->
     do allAdvisorStudyPrograms <- runQ queryAllAdvisorStudyPrograms
        advisedProgramCategoriesAdvisorStudyProgram <- runJustT
                                                        (getAdvisedProgramCategoriesAdvisorStudyProgram
                                                          advisorCategoryToEdit)
        return
         (editAdvisorCategoryView sinfo advisorCategoryToEdit
           advisedProgramCategoriesAdvisorStudyProgram
           allAdvisorStudyPrograms
           (\entity ->
             transactionController (updateAdvisorCategoryT entity)
              (nextInProcessOr listAdvisorCategoryController Nothing))
           listAdvisorCategoryController))

--- Transaction to persist modifications of a given AdvisorCategory entity
--- to the database.
updateAdvisorCategoryT :: AdvisorCategory -> Transaction ()
updateAdvisorCategoryT advisorCategory = updateAdvisorCategory advisorCategory

--- Deletes a given AdvisorCategory entity (after asking for confirmation)
--- and proceeds with the list controller.
deleteAdvisorCategoryController :: AdvisorCategory -> Controller
deleteAdvisorCategoryController advisorCategory =
  checkAuthorization
   (advisorCategoryOperationAllowed (DeleteEntity advisorCategory))
   $ (\_ ->
     confirmController
      [h3
        [htxt
          (concat
            ["Really delete entity \"",advisorCategoryToShortView
                                        advisorCategory,"\"?"])]]
      (transactionController (deleteAdvisorCategoryT advisorCategory)
        listAdvisorCategoryController)
      (showAdvisorCategoryController advisorCategory))

--- Transaction to delete a given AdvisorCategory entity.
deleteAdvisorCategoryT :: AdvisorCategory -> Transaction ()
deleteAdvisorCategoryT advisorCategory = deleteAdvisorCategory advisorCategory

--- Lists all AdvisorCategory entities with buttons to show, delete,
--- or edit an entity.
listAdvisorCategoryController :: Controller
listAdvisorCategoryController =
  checkAuthorization (advisorCategoryOperationAllowed ListEntities)
   $ (\sinfo ->
     do advisorCategorys <- runQ queryAllAdvisorCategorys
        return (listAdvisorCategoryView sinfo advisorCategorys))

--- Shows a AdvisorCategory entity.
showAdvisorCategoryController :: AdvisorCategory -> Controller
showAdvisorCategoryController advisorCategory =
  checkAuthorization
   (advisorCategoryOperationAllowed (ShowEntity advisorCategory))
   $ (\sinfo ->
     do advisedProgramCategoriesAdvisorStudyProgram <- runJustT
                                                        (getAdvisedProgramCategoriesAdvisorStudyProgram
                                                          advisorCategory)
        return
         (showAdvisorCategoryView sinfo advisorCategory
           advisedProgramCategoriesAdvisorStudyProgram))

--- Gets the associated AdvisorStudyProgram entity for a given AdvisorCategory entity.
getAdvisedProgramCategoriesAdvisorStudyProgram
  :: AdvisorCategory -> Transaction AdvisorStudyProgram
getAdvisedProgramCategoriesAdvisorStudyProgram aAdvisorStudyProgram =
  getAdvisorStudyProgram
   (advisorCategoryAdvisorStudyProgramAdvisedProgramCategoriesKey
     aAdvisorStudyProgram)