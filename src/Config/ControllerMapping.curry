module Config.ControllerMapping where

import Config.RoutesData
import System.Spicey
import System.Routes
import Controller.SpiceySystem
import Controller.StudyProgram
import Controller.Category
import Controller.MasterCoreArea
import Controller.User
import Controller.ModData
import Controller.ModDescr
import Controller.ModInst
import Controller.AdvisorStudyProgram
import Controller.AdvisorModule
import Controller.MasterProgram
import Controller.MasterProgInfo
import Controller.UnivisInfo
import Controller.Student
import Controller.StudentCourse
import Controller.Main
import Controller.Search

--- Maps the controllers associated to URLs in module RoutesData
--- into the actual controller operations.
getController :: ControllerReference -> Controller
getController fktref =
  case fktref of
    MainPageController -> mainPageController
    SearchController -> searchController
    ProcessListController -> processListController
    LoginController -> loginController
    StudyProgramController -> mainStudyProgramController
    CategoryController -> categoryController
    MasterCoreAreaController -> masterCoreAreaController
    UserController -> userController
    ModDataController -> mainModDataController
    ModInstController -> mainModInstController
    AdvisorStudyProgramController -> mainAdvisorStudyProgramController
    AdvisorModuleController -> mainAdvisorModuleController
    MasterProgramController -> mainMasterProgramController
    UnivisInfoController -> mainUnivisInfoController
    StudentController -> mainStudentController
    StudentCourseController -> mainStudentCourseController
    _ -> displayError "getController: no mapping found"
