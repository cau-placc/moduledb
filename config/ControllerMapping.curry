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
   ListStudyProgramController -> listStudyProgramController
   NewStudyProgramController -> newStudyProgramController
   ListCategoryController -> listCategoryController
   NewCategoryController -> newCategoryController
   ListMasterCoreAreaController -> listMasterCoreAreaController
   NewMasterCoreAreaController -> newMasterCoreAreaController
   ListUserController -> listUserController
   NewUserController -> newUserController
   ListModDataController -> listModDataController
   NewModDataController -> newModDataController
   NewImpModDataController -> newImportModDataController
   ListModInstController -> listModInstController
   ListMasterProgramController -> listMasterProgramController
   NewMasterProgramController -> newMasterProgramController
   ListMasterProgInfoController -> listMasterProgInfoController
   ListUnivisInfoController -> listUnivisInfoController
   NewUnivisInfoController -> newUnivisInfoController
   _ -> displayError "getController: no mapping found"
