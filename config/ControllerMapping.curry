module ControllerMapping where

import Spicey
import Routes
import SpiceySystemController
import RoutesData
import StudyProgramController
import CategoryController
import MasterCoreAreaController
import UserController
import ModDataController
import ModDescrController
import ModInstController
import AdvisorStudyProgramController
import AdvisorModuleController
import MasterProgramController
import MasterProgInfoController
import UnivisInfoController
import MainController
import SearchController

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
    _ -> displayError "getController: no mapping found"
