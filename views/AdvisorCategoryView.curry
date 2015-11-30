module AdvisorCategoryView
  ( wAdvisorCategory, tuple2AdvisorCategory, advisorCategory2Tuple
  , wAdvisorCategoryType, blankAdvisorCategoryView, createAdvisorCategoryView
  , editAdvisorCategoryView, showAdvisorCategoryView
  , listAdvisorCategoryView ) where

import WUI
import HTML
import Time
import Sort
import Spicey
import SessionInfo
import MDB
import MDBEntitiesToHtml

--- The WUI specification for the entity type AdvisorCategory.
--- It also includes fields for associated entities.
wAdvisorCategory
  :: [AdvisorStudyProgram] -> WuiSpec (String,Int,AdvisorStudyProgram)
wAdvisorCategory advisorStudyProgramList =
  withRendering
   (wTriple wString wInt
     (wSelect advisorStudyProgramToShortView advisorStudyProgramList))
   (renderLabels advisorCategoryLabelList)

--- Transformation from data of a WUI form to entity type AdvisorCategory.
tuple2AdvisorCategory
  :: AdvisorCategory -> (String,Int,AdvisorStudyProgram) -> AdvisorCategory
tuple2AdvisorCategory
    advisorCategoryToUpdate (comment,position,advisorStudyProgram) =
  setAdvisorCategoryComment
   (setAdvisorCategoryPosition
     (setAdvisorCategoryAdvisorStudyProgramAdvisedProgramCategoriesKey
       advisorCategoryToUpdate
       (advisorStudyProgramKey advisorStudyProgram))
     position)
   comment

--- Transformation from entity type AdvisorCategory to a tuple
--- which can be used in WUI specifications.
advisorCategory2Tuple
  :: AdvisorStudyProgram
  -> AdvisorCategory -> (String,Int,AdvisorStudyProgram)
advisorCategory2Tuple advisorStudyProgram advisorCategory =
  (advisorCategoryComment advisorCategory
  ,advisorCategoryPosition advisorCategory
  ,advisorStudyProgram)

--- WUI Type for editing or creating AdvisorCategory entities.
--- Includes fields for associated entities.
wAdvisorCategoryType
  :: AdvisorCategory
  -> AdvisorStudyProgram -> [AdvisorStudyProgram] -> WuiSpec AdvisorCategory
wAdvisorCategoryType
    advisorCategory advisorStudyProgram advisorStudyProgramList =
  transformWSpec
   (tuple2AdvisorCategory advisorCategory
   ,advisorCategory2Tuple advisorStudyProgram)
   (wAdvisorCategory advisorStudyProgramList)

--- Supplies a WUI form to create a new AdvisorCategory entity.
--- The fields of the entity have some default values.
blankAdvisorCategoryView
  :: UserSessionInfo
  -> [AdvisorStudyProgram]
  -> ((String,Int,AdvisorStudyProgram) -> Controller)
  -> Controller -> [HtmlExp]
blankAdvisorCategoryView
    sinfo possibleAdvisorStudyPrograms controller cancelcontroller =
  createAdvisorCategoryView sinfo "" 0 (head possibleAdvisorStudyPrograms)
   possibleAdvisorStudyPrograms
   controller
   cancelcontroller

--- Supplies a WUI form to create a new AdvisorCategory entity.
--- Takes default values to be prefilled in the form fields.
createAdvisorCategoryView
  :: UserSessionInfo
  -> String
  -> Int
  -> AdvisorStudyProgram
  -> [AdvisorStudyProgram]
  -> ((String,Int,AdvisorStudyProgram) -> Controller)
  -> Controller -> [HtmlExp]
createAdvisorCategoryView
    _
    defaultComment
    defaultPosition
    defaultAdvisorStudyProgram
    possibleAdvisorStudyPrograms
    controller
    cancelcontroller =
  renderWuiForm (wAdvisorCategory possibleAdvisorStudyPrograms)
   (defaultComment,defaultPosition,defaultAdvisorStudyProgram)
   controller
   cancelcontroller
   "Create new AdvisorCategory"
   "create"

--- Supplies a WUI form to edit the given AdvisorCategory entity.
--- Takes also associated entities and a list of possible associations
--- for every associated entity type.
editAdvisorCategoryView
  :: UserSessionInfo
  -> AdvisorCategory
  -> AdvisorStudyProgram
  -> [AdvisorStudyProgram]
  -> (AdvisorCategory -> Controller) -> Controller -> [HtmlExp]
editAdvisorCategoryView
    _
    advisorCategory
    relatedAdvisorStudyProgram
    possibleAdvisorStudyPrograms
    controller
    cancelcontroller =
  renderWuiForm
   (wAdvisorCategoryType advisorCategory relatedAdvisorStudyProgram
     possibleAdvisorStudyPrograms)
   advisorCategory
   controller
   cancelcontroller
   "Edit AdvisorCategory"
   "change"

--- Supplies a view to show the details of a AdvisorCategory.
showAdvisorCategoryView
  :: UserSessionInfo -> AdvisorCategory -> AdvisorStudyProgram -> [HtmlExp]
showAdvisorCategoryView _ advisorCategory relatedAdvisorStudyProgram =
  advisorCategoryToDetailsView advisorCategory relatedAdvisorStudyProgram
   ++ [spHref "?AdvisorCategory/list" [htxt "back to AdvisorCategory list"]]

--- Compares two AdvisorCategory entities. This order is used in the list view.
leqAdvisorCategory :: AdvisorCategory -> AdvisorCategory -> Bool
leqAdvisorCategory x1 x2 =
  (advisorCategoryComment x1,advisorCategoryPosition x1)
   <= (advisorCategoryComment x2,advisorCategoryPosition x2)

--- Supplies a list view for a given list of AdvisorCategory entities.
--- Shows also show/edit/delete buttons if the user is logged in.
--- The arguments are the session info and the list of AdvisorCategory entities.
listAdvisorCategoryView :: UserSessionInfo -> [AdvisorCategory] -> [HtmlExp]
listAdvisorCategoryView sinfo advisorCategorys =
  [h1 [htxt "AdvisorCategory list"],spTable
                                     ([take 2 advisorCategoryLabelList]
                                       ++ map listAdvisorCategory
                                           (mergeSort leqAdvisorCategory
                                             advisorCategorys))]
  where
    listAdvisorCategory advisorCategory =
      advisorCategoryToListView advisorCategory
       ++ (if userLoginOfSession sinfo == Nothing
              then []
              else [[spHref
                      ("?AdvisorCategory/show/"
                        ++ showAdvisorCategoryKey advisorCategory)
                      [htxt "show"]],[spHref
                                       ("?AdvisorCategory/edit/"
                                         ++ showAdvisorCategoryKey
                                             advisorCategory)
                                       [htxt "edit"]],[spHref
                                                        ("?AdvisorCategory/delete/"
                                                          ++ showAdvisorCategoryKey
                                                              advisorCategory)
                                                        [htxt "delete"]]])