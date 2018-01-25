module View.StudyProgram (
 wStudyProgram, tuple2StudyProgram, studyProgram2Tuple, wStudyProgramType,
 blankStudyProgramView, createStudyProgramView, editStudyProgramView,
 showStudyProgramView, listStudyProgramView, leqStudyProgram
 ) where

import WUI
import HTML.Base
import Time
import Sort
import System.Spicey
import MDB
import View.MDBEntitiesToHtml
import System.SessionInfo
import System.MultiLang

--- The WUI specification for the entity type StudyProgram.
wStudyProgram :: WuiSpec (String,String,String,String,Int)
wStudyProgram =
  withRendering
   (w5Tuple wRequiredString wString wRequiredString wRequiredString wInt)
   (renderLabels studyProgramLabelList)

--- Transformation from data of a WUI form to entity type StudyProgram.
tuple2StudyProgram
  :: StudyProgram -> (String,String,String,String,Int) -> StudyProgram
tuple2StudyProgram
    studyProgramToUpdate (name,nameE,shortName,progKey,position) =
  setStudyProgramName
   (setStudyProgramNameE
     (setStudyProgramShortName
       (setStudyProgramProgKey
         (setStudyProgramPosition studyProgramToUpdate position)
         progKey)
       shortName)
     nameE)
   name

--- Transformation from entity type StudyProgram to a tuple
--- which can be used in WUI specifications.
studyProgram2Tuple :: StudyProgram -> (String,String,String,String,Int)
studyProgram2Tuple studyProgram =
  (studyProgramName studyProgram
  ,studyProgramNameE studyProgram
  ,studyProgramShortName studyProgram
  ,studyProgramProgKey studyProgram
  ,studyProgramPosition studyProgram)

--- WUI Type for editing or creating StudyProgram entities.
--- Includes fields for associated entities.
wStudyProgramType :: StudyProgram -> WuiSpec StudyProgram
wStudyProgramType studyProgram =
  transformWSpec (tuple2StudyProgram studyProgram,studyProgram2Tuple)
   wStudyProgram

--- Supplies a WUI form to create a new StudyProgram entity.
--- The fields of the entity have some default values.
blankStudyProgramView
  :: UserSessionInfo
  -> ((String,String,String,String,Int) -> Controller)
  -> Controller -> [HtmlExp]
blankStudyProgramView sinfo controller cancelcontroller =
  createStudyProgramView sinfo "" "" "" "" 0 controller cancelcontroller

--- Supplies a WUI form to create a new StudyProgram entity.
--- Takes default values to be prefilled in the form fields.
createStudyProgramView
  :: UserSessionInfo
  -> String
  -> String
  -> String
  -> String
  -> Int
  -> ((String,String,String,String,Int) -> Controller)
  -> Controller -> [HtmlExp]
createStudyProgramView
    _
    defaultName
    defaultNameE
    defaultShortName
    defaultProgKey
    defaultPosition
    controller
    cancelcontroller =
  renderWuiForm wStudyProgram
   (defaultName,defaultNameE,defaultShortName,defaultProgKey,defaultPosition)
   controller
   cancelcontroller
   "Create new StudyProgram"
   "create"

--- Supplies a WUI form to edit the given StudyProgram entity.
--- Takes also associated entities and a list of possible associations
--- for every associated entity type.
editStudyProgramView
 :: StudyProgram -> (Bool -> StudyProgram -> Controller) -> [HtmlExp]
editStudyProgramView studyProgram controller =
  let initdata = studyProgram
      
      wuiframe = wuiEditForm "edit StudyProgram" "change"
                  (controller False initdata)
      
      (hexp ,handler) = wuiWithErrorForm (wStudyProgramType studyProgram)
                         initdata (nextControllerForData (controller True))
                         (wuiFrameToForm wuiframe)
   in wuiframe hexp handler

--- Supplies a view to show the details of a StudyProgram.
showStudyProgramView :: StudyProgram -> [HtmlExp]
showStudyProgramView studyProgram =
  studyProgramToDetailsView studyProgram ++
   [spHref "?StudyProgram/list" [htxt "back to StudyProgram list"]]

--- Compares two StudyProgram entities. This order is used in the list view.
leqStudyProgram :: StudyProgram -> StudyProgram -> Bool
leqStudyProgram x1 x2 =
  studyProgramPosition x1 <= studyProgramPosition x2

--- Supplies a list view for a given list of StudyProgram entities.
--- Shows also buttons to show, delete, or edit entries.
--- The arguments are the list of StudyProgram entities
--- and the controller functions to show, delete and edit entities.
listStudyProgramView :: UserSessionInfo -> [StudyProgram] -> [HtmlExp]
listStudyProgramView sinfo studyPrograms =
  if isAdminSession sinfo
  then [h1 [htxt $ t "Degree programs"],
        spTable (map (studyProgramLabelList!!) [0,2,3,4] :
                 map listStudyProgram (sortBy leqStudyProgram studyPrograms))]
  else [h1 [htxt $ t "Degree programs"],
        spTable (map (\sp -> [langSelect sinfo
                                (studyProgramToListView sp !! 1)
                                (head (studyProgramToListView sp))])
                     (sortBy leqStudyProgram studyPrograms))]
 where
  t = translate sinfo

  listStudyProgram :: StudyProgram -> [[HtmlExp]]
  listStudyProgram studyProgram =
     studyProgramToListView studyProgram ++
      [[spHref
         ("?StudyProgram/show/" ++ showStudyProgramKey studyProgram)
         [htxt "show"]]
      ,[spHref
         ("?StudyProgram/edit/" ++ showStudyProgramKey studyProgram)
         [htxt "edit"]]
      ,[spHref
         ("?StudyProgram/delete/" ++ showStudyProgramKey studyProgram)
         [htxt "delete"]]]
