module StudyProgramView (
 wStudyProgram, tuple2StudyProgram, studyProgram2Tuple, wStudyProgramType,
 blankStudyProgramView, createStudyProgramView, editStudyProgramView,
 showStudyProgramView, listStudyProgramView, leqStudyProgram
 ) where

import WUI
import HTML
import Time
import Sort
import Spicey
import MDB
import MDBEntitiesToHtml
import UserPreferences

--- The WUI specification for the entity type StudyProgram.
wStudyProgram :: WuiSpec (String,String,String,String,Int)
wStudyProgram =
  withRendering
   (w5Tuple wRequiredString wRequiredString wRequiredString wRequiredString
     wInt)
   (renderLabels studyProgramLabelList)

--- Transformation from data of a WUI form to entity type StudyProgram.
tuple2StudyProgram
 :: StudyProgram -> (String,String,String,String,Int) -> StudyProgram
tuple2StudyProgram studyProgramToUpdate (name ,shortName ,progKey ,uRLKey
                                         ,position) =
  setStudyProgramName
   (setStudyProgramShortName
     (setStudyProgramProgKey
       (setStudyProgramURLKey
         (setStudyProgramPosition studyProgramToUpdate position) uRLKey)
       progKey)
     shortName)
   name

--- Transformation from entity type StudyProgram to a tuple
--- which can be used in WUI specifications.
studyProgram2Tuple :: StudyProgram -> (String,String,String,String,Int)
studyProgram2Tuple studyProgram =
  (studyProgramName studyProgram,studyProgramShortName studyProgram
  ,studyProgramProgKey studyProgram,studyProgramURLKey studyProgram
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
 :: (Bool -> (String,String,String,String,Int) -> Controller) -> [HtmlExp]
blankStudyProgramView controller =
  createStudyProgramView [] [] [] [] 0 controller

--- Supplies a WUI form to create a new StudyProgram entity.
--- Takes default values to be prefilled in the form fields.
createStudyProgramView
 :: String -> String -> String -> String -> Int
  -> (Bool -> (String,String,String,String,Int) -> Controller) -> [HtmlExp]
createStudyProgramView defaultName defaultShortName defaultProgKey
                       defaultURLKey defaultPosition controller =
  let initdata = (defaultName,defaultShortName,defaultProgKey,defaultURLKey
                 ,defaultPosition)
      
      wuiframe = wuiEditForm "new StudyProgram" "create"
                  (controller False initdata)
      
      (hexp ,handler) = wuiWithErrorForm wStudyProgram initdata
                         (nextControllerForData (controller True))
                         (wuiFrameToForm wuiframe)
   in wuiframe hexp handler

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
showStudyProgramView :: StudyProgram -> Controller -> [HtmlExp]
showStudyProgramView studyProgram controller =
  studyProgramToDetailsView studyProgram ++
   [spButton "back to StudyProgram list" (nextController controller)]

--- Compares two StudyProgram entities. This order is used in the list view.
leqStudyProgram :: StudyProgram -> StudyProgram -> Bool
leqStudyProgram x1 x2 =
  studyProgramPosition x1 <= studyProgramPosition x2

--- Supplies a list view for a given list of StudyProgram entities.
--- Shows also buttons to show, delete, or edit entries.
--- The arguments are the list of StudyProgram entities
--- and the controller functions to show, delete and edit entities.
listStudyProgramView
 :: Bool -> UserPrefs -> [StudyProgram] -> (StudyProgram -> Controller)
  -> (StudyProgram -> Controller) -> (StudyProgram -> Bool -> Controller)
  -> [HtmlExp]
listStudyProgramView admin prefs studyPrograms showStudyProgramController
                     editStudyProgramController deleteStudyProgramController =
  if admin
  then [h1 [htxt $ t "Study programs"],
        spTable ([take 5 studyProgramLabelList] ++
                map listStudyProgram (mergeSort leqStudyProgram studyPrograms))]
  else [h1 [htxt $ t "Study programs"],
        spTable (map (\sp -> [head (studyProgramToListView sp)])
                     (mergeSort leqStudyProgram studyPrograms))]
 where t = translate prefs

       listStudyProgram :: StudyProgram -> [[HtmlExp]]
       listStudyProgram studyProgram =
          studyProgramToListView studyProgram ++
           [[spSmallButton "show"
              (nextController (showStudyProgramController studyProgram)),
             spSmallButton "edit"
              (nextController (editStudyProgramController studyProgram)),
             spSmallButton "delete"
              (confirmNextController
                (h3
                  [htxt
                    (concat
                      ["Really delete entity \""
                      ,studyProgramToShortView studyProgram,"\"?"])])
                (deleteStudyProgramController studyProgram))]]
