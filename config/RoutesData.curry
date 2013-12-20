module RoutesData where

import Authentication

data ControllerReference
 = MainPageController | SearchController
 | ProcessListController | LoginController | ListStudyProgramController 
 | NewStudyProgramController
 | CategoryController | ListCategoryController
 | MasterCoreAreaController
 | UserController
 | ModDataController
 | ListModInstController | ListMasterProgramController 
 | NewMasterProgramController | ListMasterProgInfoController 
 | ListUnivisInfoController | NewUnivisInfoController 
 | LoadUnivisController

data UrlMatch = Exact String | Prefix String String
              | Matcher (String -> Bool) | Always 
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
      ,("Modulsuche",Exact "search",SearchController)
      ,("Studiengänge",Exact "listStudyProgram",ListStudyProgramController)] ++
      addIf admin
        [("Neues Modul",Prefix "ModData" "new",ModDataController),
         ("Neues Importmodul",Prefix "ModData" "newimp",ModDataController),
         ("Alle Benutzer",Prefix "User" "list",UserController),
         ("Neuer Benutzer",Prefix "User" "new",UserController),
         ("Neuer Studiengang",Exact "newStudyProgram",
          NewStudyProgramController),
         ("Neue Kategorie",Prefix "Category" "new",CategoryController),
         ("Neuer Masterbereich",Prefix "MCA" "new"
          ,MasterCoreAreaController)] ++
      [("Alle Kategorien",Exact "listCategory",ListCategoryController)
      ,("Masterbereiche",Prefix "MCA" "list"
       ,MasterCoreAreaController)
      ,("List ModData",Prefix "ModData" "list",ModDataController)
      ,("List ModInst",Exact "listModInst",ListModInstController)
      --,("New ModInst",Exact "newModInst",NewModInstController)
      ,("List MasterProgram",Exact "listMasterProgram"
       ,ListMasterProgramController)
      ,("New MasterProgram",Exact "newMasterProgram",NewMasterProgramController)
      --,("List MasterProgInfo",Exact "listMasterProgInfo"
      -- ,ListMasterProgInfoController)
      --,("New MasterProgInfo",Exact "newMasterProgInfo"
      -- ,NewMasterProgInfoController)
      ,("Daten aus UnivIS übernehmen",Exact "loadUnivisInfo",
        LoadUnivisController)
      ,("Zeige UnivisInfo-Daten",Exact "listUnivisInfo",
        ListUnivisInfoController)
      ,(maybe "An" (const "Ab") login ++ "melden",Exact "login",LoginController)
      ,("default",Always,MainPageController)]
 where
  addIf cnd l = if cnd then l else []
