module AdvisorModuleController
   ( mainAdvisorModuleController, selectAdvisorModuleController
   , deleteAdvisorModuleController ) where

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
          (deleteAdvisorModuleController listAdvisorModuleController)
       _ -> displayError "Illegal URL"

--- Shows a form to create a new AdvisorModule entity.
newAdvisorModuleController :: Controller
newAdvisorModuleController =
  checkAuthorization (advisorModuleOperationAllowed NewEntity)
   $ (\sinfo ->
     do allModInsts <- runQ queryAllModInsts
        allCategorys <- runQ queryAllCategorys
        allAdvisorStudyPrograms <- runQ queryAllAdvisorStudyPrograms
        return
         (blankAdvisorModuleView sinfo allModInsts allCategorys
           allAdvisorStudyPrograms
           (\entity ->
             transactionController (createAdvisorModuleT entity)
              (nextInProcessOr listAdvisorModuleController Nothing))
           listAdvisorModuleController))

--- Shows a form to select and create a new AdvisorModule entity.
selectAdvisorModuleController
  :: AdvisorStudyProgram -> Category -> [(ModInst,ModData)]
  -> Controller -> Controller
selectAdvisorModuleController asprog cat modinstdatas nextctrl = do
  sinfo <- getUserSessionInfo
  return (selectAdvisorModuleView sinfo modinstdatas cat
           (\entity ->
             transactionController (let (c1,(c2,_),c3) = entity
                                     in createAdvisorModuleT (c1,c2,c3,asprog))
                                   nextctrl)
           nextctrl)

--- Transaction to persist a new AdvisorModule entity to the database.
createAdvisorModuleT
  :: (Bool,ModInst,Category,AdvisorStudyProgram) -> Transaction ()
createAdvisorModuleT (mandatory,modInst,category,advisorStudyProgram) =
  newAdvisorModuleWithAdvisorStudyProgramAdvisorProgramModulesKeyWithCategoryAdvisorCategorizingKeyWithModInstAdvisedProgramModuleInstancesKey
   mandatory
   (advisorStudyProgramKey advisorStudyProgram)
   (categoryKey category)
   (modInstKey modInst)
   |>> returnT ()

--- Shows a form to edit the given AdvisorModule entity.
editAdvisorModuleController :: AdvisorModule -> Controller
editAdvisorModuleController advisorModuleToEdit =
  checkAuthorization
   (advisorModuleOperationAllowed (UpdateEntity advisorModuleToEdit))
   $ (\sinfo ->
     do allModInsts <- runQ queryAllModInsts
        allCategorys <- runQ queryAllCategorys
        allAdvisorStudyPrograms <- runQ queryAllAdvisorStudyPrograms
        advisedProgramModuleInstancesModInst <- runJustT
                                                 (getAdvisedProgramModuleInstancesModInst
                                                   advisorModuleToEdit)
        advisorCategorizingCategory <- runJustT
                                        (getAdvisorCategorizingCategory
                                          advisorModuleToEdit)
        advisorProgramModulesAdvisorStudyProgram <- runJustT
                                                     (getAdvisorProgramModulesAdvisorStudyProgram
                                                       advisorModuleToEdit)
        return
         (editAdvisorModuleView sinfo advisorModuleToEdit
           advisedProgramModuleInstancesModInst
           advisorCategorizingCategory
           advisorProgramModulesAdvisorStudyProgram
           allModInsts
           allCategorys
           allAdvisorStudyPrograms
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
deleteAdvisorModuleController :: Controller -> AdvisorModule -> Controller
deleteAdvisorModuleController nextctrl advisorModule =
  checkAuthorization
   (advisorModuleOperationAllowed (DeleteEntity advisorModule))
   $ (\_ ->
     confirmController
      [h3
        [htxt "Modulempfehlung wirklich löschen?"]]
      (transactionController (deleteAdvisorModuleT advisorModule)
                             nextctrl)
      nextctrl)

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
     do advisedProgramModuleInstancesModInst <-
          runJustT (getAdvisedProgramModuleInstancesModInst advisorModule)
        advisorCategorizingCategory <-
          runJustT (getAdvisorCategorizingCategory advisorModule)
        advisorProgramModulesAdvisorStudyProgram <-
          runJustT (getAdvisorProgramModulesAdvisorStudyProgram advisorModule)
        return
         (showAdvisorModuleView sinfo advisorModule
           advisedProgramModuleInstancesModInst
           advisorCategorizingCategory
           advisorProgramModulesAdvisorStudyProgram))

--- Gets the associated ModInst entity for a given AdvisorModule entity.
getAdvisedProgramModuleInstancesModInst
  :: AdvisorModule -> Transaction ModInst
getAdvisedProgramModuleInstancesModInst aModInst =
  getModInst (advisorModuleModInstAdvisedProgramModuleInstancesKey aModInst)

--- Gets the associated Category entity for a given AdvisorModule entity.
getAdvisorCategorizingCategory :: AdvisorModule -> Transaction Category
getAdvisorCategorizingCategory aCategory =
  getCategory (advisorModuleCategoryAdvisorCategorizingKey aCategory)

--- Gets the associated AdvisorStudyProgram entity for a given AdvisorModule entity.
getAdvisorProgramModulesAdvisorStudyProgram
  :: AdvisorModule -> Transaction AdvisorStudyProgram
getAdvisorProgramModulesAdvisorStudyProgram aAdvisorStudyProgram =
  getAdvisorStudyProgram
   (advisorModuleAdvisorStudyProgramAdvisorProgramModulesKey
     aAdvisorStudyProgram)
     