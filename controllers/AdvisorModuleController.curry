module AdvisorModuleController ( mainAdvisorModuleController ) where

import Spicey
import KeyDatabase
import HTML
import Time
import MDB
import AdvisorModuleView
import Maybe
import SessionInfo
import Authorization
import AuthorizedControllers
import UserProcesses
import MDBEntitiesToHtml

--- Choose the controller for a AdvisorModule entity according to the URL parameter.
mainAdvisorModuleController :: Controller
mainAdvisorModuleController =
  do args <- getControllerParams
     case args of
       [] -> listAdvisorModuleController
       ["list"] -> listAdvisorModuleController
       ["new"] -> newAdvisorModuleController
       ["show",s] ->
         applyControllerOn (readAdvisorModuleKey s) getAdvisorModule
          showAdvisorModuleController
       ["edit",s] ->
         applyControllerOn (readAdvisorModuleKey s) getAdvisorModule
          editAdvisorModuleController
       ["delete",s] ->
         applyControllerOn (readAdvisorModuleKey s) getAdvisorModule
          deleteAdvisorModuleController
       _ -> displayError "Illegal URL"

--- Shows a form to create a new AdvisorModule entity.
newAdvisorModuleController :: Controller
newAdvisorModuleController =
  checkAuthorization (advisorModuleOperationAllowed NewEntity)
   $ (\sinfo ->
     do allModInsts <- runQ queryAllModInsts
        allAdvisorCategorys <- runQ queryAllAdvisorCategorys
        return
         (blankAdvisorModuleView sinfo allModInsts allAdvisorCategorys
           (\entity ->
             transactionController (createAdvisorModuleT entity)
              (nextInProcessOr listAdvisorModuleController Nothing))
           listAdvisorModuleController))

--- Transaction to persist a new AdvisorModule entity to the database.
createAdvisorModuleT :: (Maybe Bool,ModInst,AdvisorCategory) -> Transaction ()
createAdvisorModuleT (compulsory,modInst,advisorCategory) =
  newAdvisorModuleWithAdvisorCategoryAdvisorCategorizingKeyWithModInstAdvisedProgramModuleInstancesKey
   compulsory
   (advisorCategoryKey advisorCategory)
   (modInstKey modInst)
   |>> returnT ()

--- Shows a form to edit the given AdvisorModule entity.
editAdvisorModuleController :: AdvisorModule -> Controller
editAdvisorModuleController advisorModuleToEdit =
  checkAuthorization
   (advisorModuleOperationAllowed (UpdateEntity advisorModuleToEdit))
   $ (\sinfo ->
     do allModInsts <- runQ queryAllModInsts
        allAdvisorCategorys <- runQ queryAllAdvisorCategorys
        advisedProgramModuleInstancesModInst <- runJustT
                                                 (getAdvisedProgramModuleInstancesModInst
                                                   advisorModuleToEdit)
        advisorCategorizingAdvisorCategory <- runJustT
                                               (getAdvisorCategorizingAdvisorCategory
                                                 advisorModuleToEdit)
        return
         (editAdvisorModuleView sinfo advisorModuleToEdit
           advisedProgramModuleInstancesModInst
           advisorCategorizingAdvisorCategory
           allModInsts
           allAdvisorCategorys
           (\entity ->
             transactionController (updateAdvisorModuleT entity)
              (nextInProcessOr listAdvisorModuleController Nothing))
           listAdvisorModuleController))

--- Transaction to persist modifications of a given AdvisorModule entity
--- to the database.
updateAdvisorModuleT :: AdvisorModule -> Transaction ()
updateAdvisorModuleT advisorModule = updateAdvisorModule advisorModule

--- Deletes a given AdvisorModule entity (after asking for confirmation)
--- and proceeds with the list controller.
deleteAdvisorModuleController :: AdvisorModule -> Controller
deleteAdvisorModuleController advisorModule =
  checkAuthorization
   (advisorModuleOperationAllowed (DeleteEntity advisorModule))
   $ (\_ ->
     confirmController
      [h3
        [htxt
          (concat
            ["Really delete entity \"",advisorModuleToShortView advisorModule
            ,"\"?"])]]
      (transactionController (deleteAdvisorModuleT advisorModule)
        listAdvisorModuleController)
      (showAdvisorModuleController advisorModule))

--- Transaction to delete a given AdvisorModule entity.
deleteAdvisorModuleT :: AdvisorModule -> Transaction ()
deleteAdvisorModuleT advisorModule = deleteAdvisorModule advisorModule

--- Lists all AdvisorModule entities with buttons to show, delete,
--- or edit an entity.
listAdvisorModuleController :: Controller
listAdvisorModuleController =
  checkAuthorization (advisorModuleOperationAllowed ListEntities)
   $ (\sinfo ->
     do advisorModules <- runQ queryAllAdvisorModules
        return (listAdvisorModuleView sinfo advisorModules))

--- Shows a AdvisorModule entity.
showAdvisorModuleController :: AdvisorModule -> Controller
showAdvisorModuleController advisorModule =
  checkAuthorization
   (advisorModuleOperationAllowed (ShowEntity advisorModule))
   $ (\sinfo ->
     do advisedProgramModuleInstancesModInst <- runJustT
                                                 (getAdvisedProgramModuleInstancesModInst
                                                   advisorModule)
        advisorCategorizingAdvisorCategory <- runJustT
                                               (getAdvisorCategorizingAdvisorCategory
                                                 advisorModule)
        return
         (showAdvisorModuleView sinfo advisorModule
           advisedProgramModuleInstancesModInst
           advisorCategorizingAdvisorCategory))

--- Gets the associated ModInst entity for a given AdvisorModule entity.
getAdvisedProgramModuleInstancesModInst
  :: AdvisorModule -> Transaction ModInst
getAdvisedProgramModuleInstancesModInst aModInst =
  getModInst (advisorModuleModInstAdvisedProgramModuleInstancesKey aModInst)

--- Gets the associated AdvisorCategory entity for a given AdvisorModule entity.
getAdvisorCategorizingAdvisorCategory
  :: AdvisorModule -> Transaction AdvisorCategory
getAdvisorCategorizingAdvisorCategory aAdvisorCategory =
  getAdvisorCategory
   (advisorModuleAdvisorCategoryAdvisorCategorizingKey aAdvisorCategory)