module View.StudentCourse
  ( wStudentCourse, tuple2StudentCourse, studentCourse2Tuple
  , wStudentCourseType, blankStudentCourseView, createStudentCourseView
  , editStudentCourseView, showStudentCourseView, listStudentCourseView )
where

import WUI
import HTML.Base
import Time
import Sort
import HTML.Styles.Bootstrap3
import System.Spicey
import System.SessionInfo
import MDB
import View.MDBEntitiesToHtml

--- The WUI specification for the entity type StudentCourse.
--- It also includes fields for associated entities.
wStudentCourse
  :: [ModInst] -> [Student] -> WuiSpec (ClockTime,ModInst,Student)
wStudentCourse modInstList studentList =
  withRendering
   (wTriple wDateType (wSelect modInstToShortView modInstList)
     (wSelect studentToShortView studentList))
   (renderLabels studentCourseLabelList)

--- Transformation from data of a WUI form to entity type StudentCourse.
tuple2StudentCourse
  :: StudentCourse -> (ClockTime,ModInst,Student) -> StudentCourse
tuple2StudentCourse studentCourseToUpdate (selectDate,modInst,student) =
  setStudentCourseSelectDate
   (setStudentCourseStudentStudentCoursesKey
     (setStudentCourseModInstStudentCourseInstancesKey studentCourseToUpdate
       (modInstKey modInst))
     (studentKey student))
   selectDate

--- Transformation from entity type StudentCourse to a tuple
--- which can be used in WUI specifications.
studentCourse2Tuple
  :: ModInst -> Student -> StudentCourse -> (ClockTime,ModInst,Student)
studentCourse2Tuple modInst student studentCourse =
  (studentCourseSelectDate studentCourse,modInst,student)

--- WUI Type for editing or creating StudentCourse entities.
--- Includes fields for associated entities.
wStudentCourseType
  :: StudentCourse
  -> ModInst -> Student -> [ModInst] -> [Student] -> WuiSpec StudentCourse
wStudentCourseType studentCourse modInst student modInstList studentList =
  transformWSpec
   (tuple2StudentCourse studentCourse,studentCourse2Tuple modInst student)
   (wStudentCourse modInstList studentList)

--- Supplies a WUI form to create a new StudentCourse entity.
--- The fields of the entity have some default values.
blankStudentCourseView
  :: UserSessionInfo
  -> ClockTime
  -> [ModInst]
  -> [Student]
  -> ((ClockTime,ModInst,Student) -> Controller) -> Controller -> [HtmlExp]
blankStudentCourseView
    sinfo ctime possibleModInsts possibleStudents controller cancelcontroller =
  createStudentCourseView sinfo ctime (head possibleModInsts)
   (head possibleStudents)
   possibleModInsts
   possibleStudents
   controller
   cancelcontroller

--- Supplies a WUI form to create a new StudentCourse entity.
--- Takes default values to be prefilled in the form fields.
createStudentCourseView
  :: UserSessionInfo
  -> ClockTime
  -> ModInst
  -> Student
  -> [ModInst]
  -> [Student]
  -> ((ClockTime,ModInst,Student) -> Controller) -> Controller -> [HtmlExp]
createStudentCourseView
    _
    defaultSelectDate
    defaultModInst
    defaultStudent
    possibleModInsts
    possibleStudents
    controller
    cancelcontroller =
  renderWuiForm (wStudentCourse possibleModInsts possibleStudents)
   (defaultSelectDate,defaultModInst,defaultStudent)
   controller
   cancelcontroller
   "Create new StudentCourse"
   "create"

--- Supplies a WUI form to edit the given StudentCourse entity.
--- Takes also associated entities and a list of possible associations
--- for every associated entity type.
editStudentCourseView
  :: UserSessionInfo
  -> StudentCourse
  -> ModInst
  -> Student
  -> [ModInst]
  -> [Student] -> (StudentCourse -> Controller) -> Controller -> [HtmlExp]
editStudentCourseView
    _
    studentCourse
    relatedModInst
    relatedStudent
    possibleModInsts
    possibleStudents
    controller
    cancelcontroller =
  renderWuiForm
   (wStudentCourseType studentCourse relatedModInst relatedStudent
     possibleModInsts
     possibleStudents)
   studentCourse
   controller
   cancelcontroller
   "Edit StudentCourse"
   "change"

--- Supplies a view to show the details of a StudentCourse.
showStudentCourseView
  :: UserSessionInfo -> StudentCourse -> ModInst -> Student -> [HtmlExp]
showStudentCourseView _ studentCourse relatedModInst relatedStudent =
  studentCourseToDetailsView studentCourse relatedModInst relatedStudent
   ++ [hrefButton "?StudentCourse/list" [htxt "back to StudentCourse list"]]

--- Compares two StudentCourse entities. This order is used in the list view.
leqStudentCourse :: StudentCourse -> StudentCourse -> Bool
leqStudentCourse x1 x2 =
  studentCourseSelectDate x1 <= studentCourseSelectDate x2

--- Supplies a list view for a given list of StudentCourse entities.
--- Shows also show/edit/delete buttons if the user is logged in.
--- The arguments are the session info and the list of StudentCourse entities.
listStudentCourseView :: UserSessionInfo -> [StudentCourse] -> [HtmlExp]
listStudentCourseView sinfo studentCourses =
  [h1 [htxt "StudentCourse list"]
  ,spTable
    ([take 1 studentCourseLabelList]
      ++ map listStudentCourse (mergeSortBy leqStudentCourse studentCourses))]
  where
    listStudentCourse studentCourse =
      studentCourseToListView studentCourse
       ++ (if userLoginOfSession sinfo == Nothing
              then []
              else [[hrefButton
                      ("?StudentCourse/show/"
                        ++ showStudentCourseKey studentCourse)
                      [htxt "show"]]
                   ,[hrefButton
                      ("?StudentCourse/edit/"
                        ++ showStudentCourseKey studentCourse)
                      [htxt "edit"]]
                   ,[hrefButton
                      ("?StudentCourse/delete/"
                        ++ showStudentCourseKey studentCourse)
                      [htxt "delete"]]])
