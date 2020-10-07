module View.AdvisorModule
  ( wAdvisorModule, tuple2AdvisorModule, advisorModule2Tuple
  , wAdvisorModuleType, wSelectAdvisorModule
  , showAdvisorModuleView, listAdvisorModuleView )
where

import System.Helpers
import HTML.Base
import HTML.Styles.Bootstrap4
import HTML.WUI
import Time
import Sort
import System.Spicey
import System.SessionInfo
import MDB
import View.MDBEntitiesToHtml
import View.ModInst (leqModInst)
import MDBExts (modInstSemester)

--- The WUI specification for the entity type AdvisorModule.
--- It also includes fields for associated entities.
wSelectAdvisorModule :: [(ModInst,ModData)]
                     -> WuiSpec (Bool,(ModInst,ModData),Category)
wSelectAdvisorModule modInstList =
  withRendering
   (wTriple wMandatory (wSelect selectName sortedModInstList)
            (wConstant (stringToHtml . categoryName)))
   (renderLabels advisorModuleLabelList)
 where
  sortedModInstList = sortBy leqModInstData modInstList
   where leqModInstData (mi1,md1) (mi2,md2) =
           modDataNameG md1 < modDataNameG md2 ||
           (modDataNameG md1 == modDataNameG md2 && leqModInst mi1 mi2)

  -- The wRadioBool implementation leads to an internal run-time error
  -- with KiCS2 2.2.0!
  --wMandatory = wRadioBool [htxt "Pflicht", nbsp] [htxt "Empfehlung"]
  wMandatory = wSelectBool "Pflicht" "Empfehlung"

  selectName (modinst,moddata) =
    modDataNameG moddata ++
    " (" ++ showSemester (modInstSemester modinst) ++ ")"

wAdvisorModule
  :: [ModInst]
  -> [Category]
  -> [AdvisorStudyProgram]
  -> WuiSpec (Bool,ModInst,Category,AdvisorStudyProgram)
wAdvisorModule modInstList categoryList advisorStudyProgramList =
  withRendering
   (w4Tuple wBoolean (wSelect modInstToShortView modInstList)
     (wSelect categoryToShortView categoryList)
     (wSelect advisorStudyProgramToShortView advisorStudyProgramList))
   (renderLabels advisorModuleLabelList)

--- Transformation from data of a WUI form to entity type AdvisorModule.
tuple2AdvisorModule
  :: AdvisorModule
  -> (Bool,ModInst,Category,AdvisorStudyProgram) -> AdvisorModule
tuple2AdvisorModule
    advisorModuleToUpdate (mandatory,modInst,category,advisorStudyProgram) =
  setAdvisorModuleMandatory
   (setAdvisorModuleAdvisorStudyProgramAdvisorProgramModulesKey
     (setAdvisorModuleCategoryAdvisorCategorizingKey
       (setAdvisorModuleModInstAdvisedProgramModuleInstancesKey
         advisorModuleToUpdate
         (modInstKey modInst))
       (categoryKey category))
     (advisorStudyProgramKey advisorStudyProgram))
   mandatory

--- Transformation from entity type AdvisorModule to a tuple
--- which can be used in WUI specifications.
advisorModule2Tuple
  :: ModInst
  -> Category
  -> AdvisorStudyProgram
  -> AdvisorModule -> (Bool,ModInst,Category,AdvisorStudyProgram)
advisorModule2Tuple modInst category advisorStudyProgram advisorModule =
  (advisorModuleMandatory advisorModule,modInst,category,advisorStudyProgram)

--- WUI Type for editing or creating AdvisorModule entities.
--- Includes fields for associated entities.
wAdvisorModuleType
  :: AdvisorModule
  -> ModInst
  -> Category
  -> AdvisorStudyProgram
  -> [ModInst] -> [Category] -> [AdvisorStudyProgram] -> WuiSpec AdvisorModule
wAdvisorModuleType
    advisorModule
    modInst
    category
    advisorStudyProgram
    modInstList
    categoryList
    advisorStudyProgramList =
  transformWSpec
   (tuple2AdvisorModule advisorModule
   ,advisorModule2Tuple modInst category advisorStudyProgram)
   (wAdvisorModule modInstList categoryList advisorStudyProgramList)

{-
--- Supplies a WUI form to create a new AdvisorModule entity.
--- The fields of the entity have some default values.
blankAdvisorModuleView
  :: UserSessionInfo
  -> [ModInst]
  -> [Category]
  -> [AdvisorStudyProgram]
  -> ((Bool,ModInst,Category,AdvisorStudyProgram) -> Controller)
  -> Controller -> [HtmlExp]
blankAdvisorModuleView
    sinfo
    possibleModInsts
    possibleCategorys
    possibleAdvisorStudyPrograms
    controller
    cancelcontroller =
  createAdvisorModuleView sinfo False (head possibleModInsts)
   (head possibleCategorys)
   (head possibleAdvisorStudyPrograms)
   possibleModInsts
   possibleCategorys
   possibleAdvisorStudyPrograms
   controller
   cancelcontroller

--- Supplies a WUI form to create a new AdvisorModule entity.
--- The fields of the entity have some default values.
selectAdvisorModuleView
  :: UserSessionInfo
  -> [(ModInst,ModData)]
  -> Category
  -> ((Bool,(ModInst,ModData),Category) -> Controller)
  -> Controller -> [HtmlExp]
selectAdvisorModuleView sinfo possibleModInsts cat controller cancelcontroller =
  createSelectedAdvisorModuleView sinfo False (head possibleModInsts) cat
   possibleModInsts
   controller
   cancelcontroller

--- Supplies a WUI form to create a new AdvisorModule entity.
--- Takes default values to be prefilled in the form fields.
createSelectedAdvisorModuleView
  :: UserSessionInfo
  -> Bool
  -> (ModInst,ModData)
  -> Category
  -> [(ModInst,ModData)]
  -> ((Bool,(ModInst,ModData),Category) -> Controller)
  -> Controller -> [HtmlExp]
createSelectedAdvisorModuleView
    _
    defaultMandatory
    defaultModInst
    category
    possibleModInsts
    controller
    cancelcontroller =
  renderWuiForm (wSelectAdvisorModule possibleModInsts)
   (defaultMandatory,defaultModInst,category)
   controller
   cancelcontroller
   "Neues Modul im Studienprogramm"
   "Hinzufügen"

--- Supplies a WUI form to create a new AdvisorModule entity.
--- Takes default values to be prefilled in the form fields.
createAdvisorModuleView
  :: UserSessionInfo
  -> Bool
  -> ModInst
  -> Category
  -> AdvisorStudyProgram
  -> [ModInst]
  -> [Category]
  -> [AdvisorStudyProgram]
  -> ((Bool,ModInst,Category,AdvisorStudyProgram) -> Controller)
  -> Controller -> [HtmlExp]
createAdvisorModuleView
    _
    defaultMandatory
    defaultModInst
    defaultCategory
    defaultAdvisorStudyProgram
    possibleModInsts
    possibleCategorys
    possibleAdvisorStudyPrograms
    controller
    cancelcontroller =
  renderWuiForm
   (wAdvisorModule possibleModInsts possibleCategorys
     possibleAdvisorStudyPrograms)
   (defaultMandatory
   ,defaultModInst
   ,defaultCategory
   ,defaultAdvisorStudyProgram)
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
  -> Category
  -> AdvisorStudyProgram
  -> [ModInst]
  -> [Category]
  -> [AdvisorStudyProgram]
  -> (AdvisorModule -> Controller) -> Controller -> [HtmlExp]
editAdvisorModuleView
    _
    advisorModule
    relatedModInst
    relatedCategory
    relatedAdvisorStudyProgram
    possibleModInsts
    possibleCategorys
    possibleAdvisorStudyPrograms
    controller
    cancelcontroller =
  renderWuiForm
   (wAdvisorModuleType advisorModule relatedModInst relatedCategory
     relatedAdvisorStudyProgram
     possibleModInsts
     possibleCategorys
     possibleAdvisorStudyPrograms)
   advisorModule
   controller
   cancelcontroller
   "Edit AdvisorModule"
   "change"
-}

--- Supplies a view to show the details of a AdvisorModule.
showAdvisorModuleView
  :: UserSessionInfo
  -> AdvisorModule -> ModInst -> Category -> AdvisorStudyProgram -> [BaseHtml]
showAdvisorModuleView
    _
    advisorModule
    relatedModInst
    relatedCategory
    relatedAdvisorStudyProgram =
  advisorModuleToDetailsView advisorModule relatedModInst relatedCategory
   relatedAdvisorStudyProgram
   ++ [hrefPrimSmButton "?AdvisorModule/list"
                        [htxt "back to AdvisorModule list"]]

--- Compares two AdvisorModule entities. This order is used in the list view.
leqAdvisorModule :: AdvisorModule -> AdvisorModule -> Bool
leqAdvisorModule x1 x2 =
  advisorModuleMandatory x1 <= advisorModuleMandatory x2

--- Supplies a list view for a given list of AdvisorModule entities.
--- Shows also show/edit/delete buttons if the user is logged in.
--- The arguments are the session info and the list of AdvisorModule entities.
listAdvisorModuleView :: UserSessionInfo -> [AdvisorModule] -> [BaseHtml]
listAdvisorModuleView sinfo advisorModules =
  [h1 [htxt "AdvisorModule list"],
   spTable ([take 1 advisorModuleLabelList] ++
            map listAdvisorModule (sortBy leqAdvisorModule advisorModules))]
  where
    listAdvisorModule advisorModule =
      advisorModuleToListView advisorModule
       ++ (if userLoginOfSession sinfo == Nothing
              then []
              else [[hrefPrimBadge
                      ("?AdvisorModule/show/"
                        ++ showAdvisorModuleKey advisorModule)
                      [htxt "show"]],[hrefPrimBadge
                                       ("?AdvisorModule/edit/"
                                         ++ showAdvisorModuleKey
                                             advisorModule)
                                       [htxt "edit"]],[hrefPrimBadge
                                                        ("?AdvisorModule/delete/"
                                                          ++ showAdvisorModuleKey
                                                              advisorModule)
                                                        [htxt "delete"]]])