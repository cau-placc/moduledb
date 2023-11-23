module Controller.AdvisorModule
   ( mainAdvisorModuleController --, selectAdvisorModuleController
   , createAdvisorModuleT, deleteAdvisorModuleController
   , deleteAdvisorModuleT ) where

import Data.Maybe
import Data.Time
import System.Helpers
import System.Spicey
import HTML.Base
import Model.MDB
import View.AdvisorModule
import Config.EntityRoutes
import System.SessionInfo
import System.Authorization
import System.AuthorizedActions
import Config.UserProcesses
import View.MDBEntitiesToHtml

--- Choose the controller for a AdvisorModule entity according to the URL parameter.
mainAdvisorModuleController :: Controller
mainAdvisorModuleController =
  do args <- getControllerParams
     case args of
       [] -> listAdvisorModuleController
       ["list"] -> listAdvisorModuleController
       --["new"] -> newAdvisorModuleController
       ["show",s] -> controllerOnKey s showAdvisorModuleController
       --["edit",s] -> controllerOnKey s editAdvisorModuleController
       ["delete",s] -> controllerOnKey s deleteAdvisorModuleController
       ["destroy",s] -> controllerOnKey s destroyAdvisorModuleController
       _ -> displayUrlError

-----------------------------------------------------------------------
--- Shows a form to create a new AdvisorModule entity.
-- newAdvisorModuleController :: Controller
-- newAdvisorModuleController =
--   checkAuthorization (advisorModuleOperationAllowed NewEntity)
--    $ (\sinfo ->
--      do allModInsts <- runQ queryAllModInsts
--         allCategorys <- runQ queryAllCategorys
--         allAdvisorStudyPrograms <- runQ queryAllAdvisorStudyPrograms
--         return
--          (blankAdvisorModuleView sinfo allModInsts allCategorys
--            allAdvisorStudyPrograms
--            (\entity ->
--              transactionController (createAdvisorModuleT entity)
--               (nextInProcessOr listAdvisorModuleController Nothing))
--            listAdvisorModuleController))

-- --- Shows a form to select and create a new AdvisorModule entity.
-- selectAdvisorModuleController
--   :: AdvisorStudyProgram -> Category -> [(ModInst,ModData)]
--   -> Controller -> Controller
-- selectAdvisorModuleController asprog cat modinstdatas nextctrl = do
--   sinfo <- getUserSessionInfo
--   return (selectAdvisorModuleView sinfo modinstdatas cat
--            (\entity ->
--              transactionController (let (c1,(c2,_),c3) = entity
--                                      in createAdvisorModuleT (c1,c2,c3,asprog))
--                                    nextctrl)
--            nextctrl)

--- Transaction to persist a new AdvisorModule entity to the database.
createAdvisorModuleT
  :: (Bool,ModInst,Category,AdvisorStudyProgram) -> DBAction ()
createAdvisorModuleT (mandatory,modInst,category,advisorStudyProgram) =
  newAdvisorModuleWithAdvisorStudyProgramAdvisorProgramModulesKeyWithCategoryAdvisorCategorizingKeyWithModInstAdvisedProgramModuleInstancesKey
   mandatory
   (advisorStudyProgramKey advisorStudyProgram)
   (categoryKey category)
   (modInstKey modInst)
   |>> returnT ()

-- --- Shows a form to edit the given AdvisorModule entity.
-- editAdvisorModuleController :: AdvisorModule -> Controller
-- editAdvisorModuleController advisorModuleToEdit =
--   checkAuthorization
--    (advisorModuleOperationAllowed (UpdateEntity advisorModuleToEdit))
--    $ (\sinfo ->
--      do allModInsts <- runQ queryAllModInsts
--         allCategorys <- runQ queryAllCategorys
--         allAdvisorStudyPrograms <- runQ queryAllAdvisorStudyPrograms
--         advisedProgramModuleInstancesModInst <-
--           runJustT (getAdvisedProgramModuleInstancesModInst advisorModuleToEdit)
--         advisorCategorizingCategory <-
--           runJustT (getAdvisorCategorizingCategory advisorModuleToEdit)
--         advisorProgramModulesAdvisorStudyProgram <-
--           runJustT (getAdvisorProgramModulesAdvisorStudyProgram
--                                                           advisorModuleToEdit)
--         return
--          (editAdvisorModuleView sinfo advisorModuleToEdit
--            advisedProgramModuleInstancesModInst
--            advisorCategorizingCategory
--            advisorProgramModulesAdvisorStudyProgram
--            allModInsts
--            allCategorys
--            allAdvisorStudyPrograms
--            (\entity ->
--              transactionController (updateAdvisorModuleT entity)
--               (nextInProcessOr listAdvisorModuleController Nothing))
--            listAdvisorModuleController))

--- Transaction to persist modifications of a given AdvisorModule entity
--- to the database.
updateAdvisorModuleT :: AdvisorModule -> DBAction ()
updateAdvisorModuleT advisorModule = updateAdvisorModule advisorModule

--- Deletes a given AdvisorModule entity (after asking for confirmation)
--- and proceeds with the list controller.
deleteAdvisorModuleController :: AdvisorModule -> Controller
deleteAdvisorModuleController advisorModule =
  checkAuthorization
   (advisorModuleOperationAllowed (DeleteEntity advisorModule)) $ \si ->
     confirmDeletionPage si "Modulempfehlung wirklich löschen?"

--- Deletes a given AdvisorModule entity
--- and proceeds with the list controller.
destroyAdvisorModuleController :: AdvisorModule -> Controller
destroyAdvisorModuleController advisorModule =
  checkAuthorization
   (advisorModuleOperationAllowed (DeleteEntity advisorModule)) $ \_ ->
     transactionController (deleteAdvisorModuleT advisorModule)
                           listAdvisorModuleController

--- Transaction to delete a given AdvisorModule entity.
deleteAdvisorModuleT :: AdvisorModule -> DBAction ()
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
  :: AdvisorModule -> DBAction ModInst
getAdvisedProgramModuleInstancesModInst aModInst =
  getModInst (advisorModuleModInstAdvisedProgramModuleInstancesKey aModInst)

--- Gets the associated Category entity for a given AdvisorModule entity.
getAdvisorCategorizingCategory :: AdvisorModule -> DBAction Category
getAdvisorCategorizingCategory aCategory =
  getCategory (advisorModuleCategoryAdvisorCategorizingKey aCategory)

--- Gets the associated AdvisorStudyProgram entity for a given AdvisorModule entity.
getAdvisorProgramModulesAdvisorStudyProgram
  :: AdvisorModule -> DBAction AdvisorStudyProgram
getAdvisorProgramModulesAdvisorStudyProgram aAdvisorStudyProgram =
  getAdvisorStudyProgram
   (advisorModuleAdvisorStudyProgramAdvisorProgramModulesKey
     aAdvisorStudyProgram)
     