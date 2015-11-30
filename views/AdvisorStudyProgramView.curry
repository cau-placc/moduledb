module AdvisorStudyProgramView
  ( wAdvisorStudyProgram, tuple2AdvisorStudyProgram, advisorStudyProgram2Tuple
  , wAdvisorStudyProgramType, blankAdvisorStudyProgramView
  , createAdvisorStudyProgramView, editAdvisorStudyProgramView
  , showAdvisorStudyProgramView, listAdvisorStudyProgramView ) where

import WUI
import HTML
import Time
import Sort
import Spicey
import SessionInfo
import MDB
import MDBEntitiesToHtml

--- The WUI specification for the entity type AdvisorStudyProgram.
--- It also includes fields for associated entities.
wAdvisorStudyProgram
  :: [StudyProgram]
  -> [User]
  -> WuiSpec (String,String,Int,String,String,String,Bool,StudyProgram,User)
wAdvisorStudyProgram studyProgramList userList =
  withRendering
   (w9Tuple wRequiredString wRequiredString wInt wString wString wString
     wBoolean
     (wSelect studyProgramToShortView studyProgramList)
     (wSelect userToShortView userList))
   (renderLabels advisorStudyProgramLabelList)

--- Transformation from data of a WUI form to entity type AdvisorStudyProgram.
tuple2AdvisorStudyProgram
  :: AdvisorStudyProgram
  -> (String,String,Int,String,String,String,Bool,StudyProgram,User)
  -> AdvisorStudyProgram
tuple2AdvisorStudyProgram
    advisorStudyProgramToUpdate
    (name,term,year,desc,prereq,comments,visible,studyProgram,user) =
  setAdvisorStudyProgramName
   (setAdvisorStudyProgramTerm
     (setAdvisorStudyProgramYear
       (setAdvisorStudyProgramDesc
         (setAdvisorStudyProgramPrereq
           (setAdvisorStudyProgramComments
             (setAdvisorStudyProgramVisible
               (setAdvisorStudyProgramUserStudyAdvisingKey
                 (setAdvisorStudyProgramStudyProgramStudyProgramsAdvisedKey
                   advisorStudyProgramToUpdate
                   (studyProgramKey studyProgram))
                 (userKey user))
               visible)
             comments)
           prereq)
         desc)
       year)
     term)
   name

--- Transformation from entity type AdvisorStudyProgram to a tuple
--- which can be used in WUI specifications.
advisorStudyProgram2Tuple
  :: StudyProgram
  -> User
  -> AdvisorStudyProgram
  -> (String,String,Int,String,String,String,Bool,StudyProgram,User)
advisorStudyProgram2Tuple studyProgram user advisorStudyProgram =
  (advisorStudyProgramName advisorStudyProgram
  ,advisorStudyProgramTerm advisorStudyProgram
  ,advisorStudyProgramYear advisorStudyProgram
  ,advisorStudyProgramDesc advisorStudyProgram
  ,advisorStudyProgramPrereq advisorStudyProgram
  ,advisorStudyProgramComments advisorStudyProgram
  ,advisorStudyProgramVisible advisorStudyProgram
  ,studyProgram
  ,user)

--- WUI Type for editing or creating AdvisorStudyProgram entities.
--- Includes fields for associated entities.
wAdvisorStudyProgramType
  :: AdvisorStudyProgram
  -> StudyProgram
  -> User -> [StudyProgram] -> [User] -> WuiSpec AdvisorStudyProgram
wAdvisorStudyProgramType
    advisorStudyProgram studyProgram user studyProgramList userList =
  transformWSpec
   (tuple2AdvisorStudyProgram advisorStudyProgram
   ,advisorStudyProgram2Tuple studyProgram user)
   (wAdvisorStudyProgram studyProgramList userList)

--- Supplies a WUI form to create a new AdvisorStudyProgram entity.
--- The fields of the entity have some default values.
blankAdvisorStudyProgramView
  :: UserSessionInfo
  -> [StudyProgram]
  -> [User]
  -> ((String,String,Int,String,String,String,Bool,StudyProgram,User)
  -> Controller)
  -> Controller -> [HtmlExp]
blankAdvisorStudyProgramView
    sinfo possibleStudyPrograms possibleUsers controller cancelcontroller =
  createAdvisorStudyProgramView sinfo "" "" 2015 "" "" "" False
   (head possibleStudyPrograms)
   (head possibleUsers)
   possibleStudyPrograms
   possibleUsers
   controller
   cancelcontroller

--- Supplies a WUI form to create a new AdvisorStudyProgram entity.
--- Takes default values to be prefilled in the form fields.
createAdvisorStudyProgramView
  :: UserSessionInfo
  -> String
  -> String
  -> Int
  -> String
  -> String
  -> String
  -> Bool
  -> StudyProgram
  -> User
  -> [StudyProgram]
  -> [User]
  -> ((String,String,Int,String,String,String,Bool,StudyProgram,User)
  -> Controller)
  -> Controller -> [HtmlExp]
createAdvisorStudyProgramView
    _
    defaultName
    defaultTerm
    defaultYear
    defaultDesc
    defaultPrereq
    defaultComments
    defaultVisible
    defaultStudyProgram
    defaultUser
    possibleStudyPrograms
    possibleUsers
    controller
    cancelcontroller =
  renderWuiForm (wAdvisorStudyProgram possibleStudyPrograms possibleUsers)
   (defaultName
   ,defaultTerm
   ,defaultYear
   ,defaultDesc
   ,defaultPrereq
   ,defaultComments
   ,defaultVisible
   ,defaultStudyProgram
   ,defaultUser)
   controller
   cancelcontroller
   "Create new AdvisorStudyProgram"
   "create"

--- Supplies a WUI form to edit the given AdvisorStudyProgram entity.
--- Takes also associated entities and a list of possible associations
--- for every associated entity type.
editAdvisorStudyProgramView
  :: UserSessionInfo
  -> AdvisorStudyProgram
  -> StudyProgram
  -> User
  -> [StudyProgram]
  -> [User] -> (AdvisorStudyProgram -> Controller) -> Controller -> [HtmlExp]
editAdvisorStudyProgramView
    _
    advisorStudyProgram
    relatedStudyProgram
    relatedUser
    possibleStudyPrograms
    possibleUsers
    controller
    cancelcontroller =
  renderWuiForm
   (wAdvisorStudyProgramType advisorStudyProgram relatedStudyProgram
     relatedUser
     possibleStudyPrograms
     possibleUsers)
   advisorStudyProgram
   controller
   cancelcontroller
   "Edit AdvisorStudyProgram"
   "change"

--- Supplies a view to show the details of a AdvisorStudyProgram.
showAdvisorStudyProgramView
  :: UserSessionInfo
  -> AdvisorStudyProgram -> StudyProgram -> User -> [HtmlExp]
showAdvisorStudyProgramView
    _ advisorStudyProgram relatedStudyProgram relatedUser =
  advisorStudyProgramToDetailsView advisorStudyProgram relatedStudyProgram
   relatedUser
   ++ [spHref "?AdvisorStudyProgram/list"
        [htxt "back to AdvisorStudyProgram list"]]

--- Compares two AdvisorStudyProgram entities. This order is used in the list view.
leqAdvisorStudyProgram :: AdvisorStudyProgram -> AdvisorStudyProgram -> Bool
leqAdvisorStudyProgram x1 x2 =
  (advisorStudyProgramName x1
  ,advisorStudyProgramTerm x1
  ,advisorStudyProgramYear x1
  ,advisorStudyProgramDesc x1
  ,advisorStudyProgramPrereq x1
  ,advisorStudyProgramComments x1
  ,advisorStudyProgramVisible x1)
   <= (advisorStudyProgramName x2
      ,advisorStudyProgramTerm x2
      ,advisorStudyProgramYear x2
      ,advisorStudyProgramDesc x2
      ,advisorStudyProgramPrereq x2
      ,advisorStudyProgramComments x2
      ,advisorStudyProgramVisible x2)

--- Supplies a list view for a given list of AdvisorStudyProgram entities.
--- Shows also show/edit/delete buttons if the user is logged in.
--- The arguments are the session info and the list of AdvisorStudyProgram entities.
listAdvisorStudyProgramView
  :: UserSessionInfo -> [AdvisorStudyProgram] -> [HtmlExp]
listAdvisorStudyProgramView sinfo advisorStudyPrograms =
  [h1 [htxt "AdvisorStudyProgram list"],spTable
                                         ([take 7
                                            advisorStudyProgramLabelList]
                                           ++ map listAdvisorStudyProgram
                                               (mergeSort
                                                 leqAdvisorStudyProgram
                                                 advisorStudyPrograms))]
  where
    listAdvisorStudyProgram advisorStudyProgram =
      advisorStudyProgramToListView advisorStudyProgram
       ++ (if userLoginOfSession sinfo == Nothing
              then []
              else [[spHref
                      ("?AdvisorStudyProgram/show/"
                        ++ showAdvisorStudyProgramKey advisorStudyProgram)
                      [htxt "show"]],[spHref
                                       ("?AdvisorStudyProgram/edit/"
                                         ++ showAdvisorStudyProgramKey
                                             advisorStudyProgram)
                                       [htxt "edit"]],[spHref
                                                        ("?AdvisorStudyProgram/delete/"
                                                          ++ showAdvisorStudyProgramKey
                                                              advisorStudyProgram)
                                                        [htxt "delete"]]])