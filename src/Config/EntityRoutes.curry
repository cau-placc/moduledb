module Config.EntityRoutes
 where

import System.Spicey

import Model.MDB

------------------------------------------------------------------------------

instance EntityController AdvisorModule where
  controllerOnKey s controller =
    applyControllerOn (readAdvisorModuleKey s) getAdvisorModule controller


instance EntityController AdvisorStudyProgram where
  controllerOnKey s controller =
    applyControllerOn (readAdvisorStudyProgramKey s) getAdvisorStudyProgram
                      controller

  entityRoute r ent =
    "?AdvisorStudyProgram/" ++ r ++ "/" ++ showAdvisorStudyProgramKey ent


instance EntityController Category where
  controllerOnKey s controller =
    applyControllerOn (readCategoryKey s) getCategory controller


instance EntityController ModData where
  controllerOnKey s controller =
    applyControllerOn (readModDataKey s) getModData controller

  entityRoute r ent = "?ModData/" ++ r ++ "/" ++ showModDataKey ent


instance EntityController Student where
  controllerOnKey s controller =
    applyControllerOn (readStudentKey s) getStudent controller

  entityRoute r ent = "?Student/" ++ r ++ "/" ++ showStudentKey ent


instance EntityController StudentCourse where
  controllerOnKey s controller =
    applyControllerOn (readStudentCourseKey s) getStudentCourse controller


instance EntityController StudyProgram where
  controllerOnKey s controller =
    applyControllerOn (readStudyProgramKey s) getStudyProgram controller


instance EntityController User where
  controllerOnKey s controller =
    applyControllerOn (readUserKey s) getUser controller

  entityRoute r ent = "?User/" ++ r ++ "/" ++ showUserKey ent

-------------------------------------------------------------------------
