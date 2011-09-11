module RoutesData where

import Authentication

data ControllerReference
 = MainPageController | SearchController
 | ProcessListController | LoginController | ListStudyProgramController 
 | NewStudyProgramController | ListCategoryController | NewCategoryController 
 | ListMasterCoreAreaController | NewMasterCoreAreaController 
 | ListUserController | NewUserController | ListModDataController 
 | NewModDataController | NewImpModDataController
 | ListModInstController | ListMasterProgramController 
 | NewMasterProgramController | ListMasterProgInfoController 
 | ListUnivisInfoController | NewUnivisInfoController 
 | LoadUnivisController

data UrlMatch = Exact String | Matcher (String -> Bool) | Always 
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
      [("Haupseite",Exact "main",MainPageController)
      ,("Modulsuche",Exact "search",SearchController)
      ,("Studiengänge",Exact "listStudyProgram",ListStudyProgramController)] ++
      addIf admin [("Neuer Studiengang",Exact "newStudyProgram",
                   NewStudyProgramController)] ++
      [("Alle Kategorien",Exact "listCategory",ListCategoryController)] ++
      addIf admin
            [("Neue Kategorie",Exact "newCategory",NewCategoryController),
             ("Neues Modul",Exact "newModData",NewModDataController),
             ("Neues Importmodul",Exact "newImpModData",
              NewImpModDataController)] ++
      [("Masterbereiche",Exact "listMasterCoreArea"
       ,ListMasterCoreAreaController)] ++
      addIf admin [("Neuer Masterbereich",Exact "newMasterCoreArea"
                   ,NewMasterCoreAreaController)] ++
      addIf admin [("Alle Benutzer",Exact "listUser",ListUserController)
                  ,("Neuer Benutzer",Exact "newUser",NewUserController)] ++
      [("List ModData",Exact "listModData",ListModDataController)
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
