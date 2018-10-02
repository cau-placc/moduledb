module Config.RoutesData where

import System.Authentication

data ControllerReference = MainPageController
                         | SearchController
                         | ProcessListController
                         | LoginController
                         | StudyProgramController
                         | CategoryController
                         | MasterCoreAreaController
                         | UserController
                         | ModDataController
                         | ModDescrController
                         | ModInstController
                         | AdvisorStudyProgramController
                         | AdvisorModuleController
                         | MasterProgramController
                         | MasterProgInfoController
                         | UnivisInfoController

data UrlMatch = Exact String
              | Prefix String String
              | Matcher (String -> Bool)
              | Always

type Route = (String,UrlMatch,ControllerReference)

--- This constant specifies the association of URLs to controllers.
--- Controllers are identified here by constants of type
--- ControllerReference. The actual mapping of these constants
--- into the controller operations is specified in the module
--- ControllerMapping.
getRoutes :: IO [Route]
getRoutes =
  do login <- getSessionLogin
     admin <- isAdmin
     return $
      [("Hauptseite",Exact "main",MainPageController)
      ,("Modulsuche",Prefix "search" "main",SearchController)
      ,("Studiengänge",Prefix "StudyProgram" "list",StudyProgramController)] ++
      addIf admin
        [("Neues Modul",Prefix "ModData" "new",ModDataController)
        ,("Neues Importmodul",Prefix "ModData" "newimp",ModDataController)
        ,("Neuer Benutzer",Prefix "User" "new",UserController)
        ,("Neuer Studiengang",Prefix "StudyProgram" "new",
                              StudyProgramController)
        ,("Neue Kategorie",Prefix "Category" "new",CategoryController)
        ,("Neuer Masterbereich",Prefix "MCA" "new",MasterCoreAreaController)] ++
      [("Alle Kategorien",Prefix "Category" "list",CategoryController)
      ,("Alle Benutzer",Prefix "User" "list",UserController)
      ,("Masterbereiche",Prefix "MCA" "list",MasterCoreAreaController)
      ,("List ModData",Prefix "ModData" "list",ModDataController)
      ,("List ModInst",Prefix "ModInst" "list",ModInstController)
      ,("List MasterProgram",Prefix "MasterProgram" "list",
                             MasterProgramController)
      ,("Daten aus UnivIS übernehmen",Prefix "UnivisInfo" "load",
                                      UnivisInfoController)
      ,("Zeige UnivisInfo-Daten (lang!)",Prefix "UnivisInfo" "list",
                                 UnivisInfoController)
      ,("List AdvisorStudyProgram",Prefix "AdvisorStudyProgram" "list"
                                  ,AdvisorStudyProgramController)
      ,(maybe "An" (const "Ab") login ++ "melden",Exact "login",LoginController)
      ,("default",Always,MainPageController)]
 where
  addIf cnd l = if cnd then l else []
