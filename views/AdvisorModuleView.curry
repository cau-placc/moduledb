module AdvisorModuleView
  ( wAdvisorModule, tuple2AdvisorModule, advisorModule2Tuple
  , wAdvisorModuleType, blankAdvisorModuleView, createAdvisorModuleView
  , editAdvisorModuleView, showAdvisorModuleView, listAdvisorModuleView )
where

import WUI
import HTML
import Time
import Sort
import Spicey
import SessionInfo
import MDB
import MDBEntitiesToHtml

--- The WUI specification for the entity type AdvisorModule.
--- It also includes fields for associated entities.
wAdvisorModule
  :: [ModInst]
  -> [AdvisorCategory] -> WuiSpec (Maybe Bool,ModInst,AdvisorCategory)
wAdvisorModule modInstList advisorCategoryList =
  withRendering
   (wTriple (wUncheckMaybe False wBoolean)
     (wSelect modInstToShortView modInstList)
     (wSelect advisorCategoryToShortView advisorCategoryList))
   (renderLabels advisorModuleLabelList)

--- Transformation from data of a WUI form to entity type AdvisorModule.
tuple2AdvisorModule
  :: AdvisorModule -> (Maybe Bool,ModInst,AdvisorCategory) -> AdvisorModule
tuple2AdvisorModule
    advisorModuleToUpdate (compulsory,modInst,advisorCategory) =
  setAdvisorModuleCompulsory
   (setAdvisorModuleAdvisorCategoryAdvisorCategorizingKey
     (setAdvisorModuleModInstAdvisedProgramModuleInstancesKey
       advisorModuleToUpdate
       (modInstKey modInst))
     (advisorCategoryKey advisorCategory))
   compulsory

--- Transformation from entity type AdvisorModule to a tuple
--- which can be used in WUI specifications.
advisorModule2Tuple
  :: ModInst
  -> AdvisorCategory -> AdvisorModule -> (Maybe Bool,ModInst,AdvisorCategory)
advisorModule2Tuple modInst advisorCategory advisorModule =
  (advisorModuleCompulsory advisorModule,modInst,advisorCategory)

--- WUI Type for editing or creating AdvisorModule entities.
--- Includes fields for associated entities.
wAdvisorModuleType
  :: AdvisorModule
  -> ModInst
  -> AdvisorCategory
  -> [ModInst] -> [AdvisorCategory] -> WuiSpec AdvisorModule
wAdvisorModuleType
    advisorModule modInst advisorCategory modInstList advisorCategoryList =
  transformWSpec
   (tuple2AdvisorModule advisorModule
   ,advisorModule2Tuple modInst advisorCategory)
   (wAdvisorModule modInstList advisorCategoryList)

--- Supplies a WUI form to create a new AdvisorModule entity.
--- The fields of the entity have some default values.
blankAdvisorModuleView
  :: UserSessionInfo
  -> [ModInst]
  -> [AdvisorCategory]
  -> ((Maybe Bool,ModInst,AdvisorCategory) -> Controller)
  -> Controller -> [HtmlExp]
blankAdvisorModuleView
    sinfo
    possibleModInsts
    possibleAdvisorCategorys
    controller
    cancelcontroller =
  createAdvisorModuleView sinfo Nothing (head possibleModInsts)
   (head possibleAdvisorCategorys)
   possibleModInsts
   possibleAdvisorCategorys
   controller
   cancelcontroller

--- Supplies a WUI form to create a new AdvisorModule entity.
--- Takes default values to be prefilled in the form fields.
createAdvisorModuleView
  :: UserSessionInfo
  -> Maybe Bool
  -> ModInst
  -> AdvisorCategory
  -> [ModInst]
  -> [AdvisorCategory]
  -> ((Maybe Bool,ModInst,AdvisorCategory) -> Controller)
  -> Controller -> [HtmlExp]
createAdvisorModuleView
    _
    defaultCompulsory
    defaultModInst
    defaultAdvisorCategory
    possibleModInsts
    possibleAdvisorCategorys
    controller
    cancelcontroller =
  renderWuiForm (wAdvisorModule possibleModInsts possibleAdvisorCategorys)
   (defaultCompulsory,defaultModInst,defaultAdvisorCategory)
   controller
   cancelcontroller
   "Create new AdvisorModule"
   "create"

--- Supplies a WUI form to edit the given AdvisorModule entity.
--- Takes also associated entities and a list of possible associations
--- for every associated entity type.
editAdvisorModuleView
  :: UserSessionInfo
  -> AdvisorModule
  -> ModInst
  -> AdvisorCategory
  -> [ModInst]
  -> [AdvisorCategory]
  -> (AdvisorModule -> Controller) -> Controller -> [HtmlExp]
editAdvisorModuleView
    _
    advisorModule
    relatedModInst
    relatedAdvisorCategory
    possibleModInsts
    possibleAdvisorCategorys
    controller
    cancelcontroller =
  renderWuiForm
   (wAdvisorModuleType advisorModule relatedModInst relatedAdvisorCategory
     possibleModInsts
     possibleAdvisorCategorys)
   advisorModule
   controller
   cancelcontroller
   "Edit AdvisorModule"
   "change"

--- Supplies a view to show the details of a AdvisorModule.
showAdvisorModuleView
  :: UserSessionInfo
  -> AdvisorModule -> ModInst -> AdvisorCategory -> [HtmlExp]
showAdvisorModuleView _ advisorModule relatedModInst relatedAdvisorCategory =
  advisorModuleToDetailsView advisorModule relatedModInst
   relatedAdvisorCategory
   ++ [spHref "?AdvisorModule/list" [htxt "back to AdvisorModule list"]]

--- Compares two AdvisorModule entities. This order is used in the list view.
leqAdvisorModule :: AdvisorModule -> AdvisorModule -> Bool
leqAdvisorModule x1 x2 =
  advisorModuleCompulsory x1 <= advisorModuleCompulsory x2

--- Supplies a list view for a given list of AdvisorModule entities.
--- Shows also show/edit/delete buttons if the user is logged in.
--- The arguments are the session info and the list of AdvisorModule entities.
listAdvisorModuleView :: UserSessionInfo -> [AdvisorModule] -> [HtmlExp]
listAdvisorModuleView sinfo advisorModules =
  [h1 [htxt "AdvisorModule list"],spTable
                                   ([take 1 advisorModuleLabelList]
                                     ++ map listAdvisorModule
                                         (mergeSort leqAdvisorModule
                                           advisorModules))]
  where
    listAdvisorModule advisorModule =
      advisorModuleToListView advisorModule
       ++ (if userLoginOfSession sinfo == Nothing
              then []
              else [[spHref
                      ("?AdvisorModule/show/"
                        ++ showAdvisorModuleKey advisorModule)
                      [htxt "show"]],[spHref
                                       ("?AdvisorModule/edit/"
                                         ++ showAdvisorModuleKey
                                             advisorModule)
                                       [htxt "edit"]],[spHref
                                                        ("?AdvisorModule/delete/"
                                                          ++ showAdvisorModuleKey
                                                              advisorModule)
                                                        [htxt "delete"]]])