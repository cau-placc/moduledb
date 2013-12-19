module CategoryController (
 categoryController, editCategoryController, deleteCategoryController,
 listCategoryController, showCategoryController, showCategoryPlanController,
 showEmailCorrectionController
 ) where

import Spicey
import KeyDatabase
import HTML
import Time
import MDB
import CategoryView
import ModDataView
import Maybe
import Authorization
import AuthorizedControllers
import UserProcesses
import Authentication
import DefaultController
import ModDataController
import Helpers
import List
import Sort
import UserPreferences
import MDBEntitiesToHtml

--- Choose the controller for a Category entity according to the URL parameter.
categoryController :: Controller
categoryController = do
  args <- getControllerParams
  case args of
    ["list"] -> newListCategoryController
    ["new"]  -> newCategoryController
    ["show",s] -> applyControllerOn (readCategoryKey s)
                    getCategory showCategoryController
    ["edit",s] -> applyControllerOn (readCategoryKey s)
                    getCategory editCategoryController
    ["delete",s] -> applyControllerOn (readCategoryKey s)
                      getCategory askAndDeleteCategoryController


--- Shows a form to create a new Category entity.
newCategoryController :: Controller
newCategoryController =
  checkAuthorization (categoryOperationAllowed NewEntity) $
   (do allStudyPrograms <- runQ queryAllStudyPrograms
       return (blankCategoryView allStudyPrograms createCategoryController))

--- Persists a new Category entity to the database.
createCategoryController
 :: Bool -> (String,String,String,Int,StudyProgram) -> Controller
createCategoryController False _ = defaultController
createCategoryController True (name ,shortName ,catKey ,position
                               ,studyProgram) =
  do transResult <- runT
                     (newCategoryWithStudyProgramProgramCategoriesKey name
                       shortName catKey position
                       (studyProgramKey studyProgram))
     either (\ _ -> nextInProcessOr listCategoryController Nothing)
      (\ error -> displayError (showTError error)) transResult

--- Shows a form to edit the given Category entity.
editCategoryController :: Category -> Controller
editCategoryController categoryToEdit =
  checkAuthorization (categoryOperationAllowed (UpdateEntity categoryToEdit))
   $
   (do allStudyPrograms <- runQ queryAllStudyPrograms
       programCategoriesStudyProgram <- runJustT
                                         (getProgramCategoriesStudyProgram
                                           categoryToEdit)
       return
        (editCategoryView categoryToEdit programCategoriesStudyProgram
          allStudyPrograms updateCategoryController))

--- Persists modifications of a given Category entity to the
--- database depending on the Boolean argument. If the Boolean argument
--- is False, nothing is changed.
updateCategoryController :: Bool -> Category -> Controller
updateCategoryController False category = showCategoryController category
updateCategoryController True category =
  do transResult <- runT (updateCategory category)
     either (\ _ -> nextInProcessOr (showCategoryController category) Nothing)
      (\ error -> displayError (showTError error)) transResult

--- Deletes a given Category entity (after asking for acknowledgment)
--- and proceeds with the show controller.
askAndDeleteCategoryController :: Category -> Controller
askAndDeleteCategoryController cat =
  confirmController
    (h3 [htxt (concat ["Kategorie \"",categoryToShortView cat
                      ,"\" wirklich löschen?"])])
    (\ack -> if ack
             then deleteCategoryController cat
             else showCategoryController cat)

--- Deletes a given Category entity and proceeds with the list controller.
deleteCategoryController :: Category -> Controller
deleteCategoryController category =
  checkAuthorization (categoryOperationAllowed (DeleteEntity category)) $
   (do transResult <- runT (deleteCategory category)
       either (\ _ -> listCategoryController)
        (\ error -> displayError (showTError error)) transResult)

--- Lists all Category entities with buttons to show, delete,
--- or edit an entity.
newListCategoryController :: Controller
newListCategoryController =
  checkAuthorization (categoryOperationAllowed ListEntities) $
    do admin <- isAdmin
       login <- getSessionLogin
       runListCategoryController admin login []

--- Lists all Category entities with buttons to show, delete,
--- or edit an entity.
listCategoryController :: Controller
listCategoryController =
  checkAuthorization (categoryOperationAllowed ListEntities) $
    do admin <- isAdmin
       login <- getSessionLogin
       args  <- getControllerParams
       runListCategoryController admin login args

runListCategoryController admin login args
 | null args
  = do categorys <- runQ queryAllCategorys
       prefs <- getSessionUserPrefs
       let t = translate prefs
       return (listCategoryView admin login prefs (Right $ t "All categories")
                 (map (\c -> (Left c,[])) (mergeSort leqCategory categorys))
                 [] []
                 showCategoryPlanController formatCatModulesForm
                 showEmailCorrectionController)
 | take 5 (head args) == "user="
  = do let lname = drop 5 (head args)
       -- get user entries with a given login name
       users <- runQ $ queryCondUser (\u -> userLogin u == lname)
       if null users then return [h1 [htxt "Illegal URL"]] else
        do mods <- runQ $ queryModDataOfUser (userKey (head users))
           prefs <- getSessionUserPrefs
           let t = translate prefs
           return (listCategoryView admin login prefs
                        (Right $ t "My modules")
                        [(Right "",map (\m->(m,[],[])) mods)]
                        [] []
                        showCategoryPlanController
                        formatCatModulesForm showEmailCorrectionController)
 | head (head args) == 'C'
  = maybe (displayError "Illegal URL")
            (\catkey -> do
              cat <- runJustT $ getCategory catkey
              sprogs <- runQ queryAllStudyPrograms
              let spk = categoryStudyProgramProgramCategoriesKey cat
                  mbsprog = find (\p -> studyProgramKey p == spk) sprogs
              mods <- runJustT $ getModDataOfCategory catkey
              prefs <- getSessionUserPrefs
              return (listCategoryView admin login prefs
                        (maybe (Right "???") Left mbsprog)
                        [(Left cat,map (\m->(m,[],[]))
                                       (maybe (filter modDataVisible mods)
                                              (const mods)
                                              login))]
                        [] []
                        showCategoryPlanController
                        formatCatModulesForm showEmailCorrectionController))
            (readCategoryKey (head args))
 | otherwise
  = maybe (displayError "Illegal URL")
            (\spkey -> do
              studyprog <- runJustT (getStudyProgram spkey)
              categorys <- runQ $ queryCategorysOfStudyProgram spkey
              catmods <- runJustT $
               if length args > 1 && args!!1 == "all"
               then mapT (\c ->
                            getModDataOfCategory (categoryKey c) |>>= \mods ->
                            returnT (Left c,map (\m->(m,[],[]))
                                             (maybe (filter modDataVisible mods)
                                                    (const mods)
                                                    login)))
                         categorys
               else mapT (\c -> returnT (Left c,[])) categorys
              prefs <- getSessionUserPrefs
              return (listCategoryView admin login prefs (Left studyprog)
                         catmods [] []
                         showCategoryPlanController
                         formatCatModulesForm showEmailCorrectionController))
            (readStudyProgramKey (head args))
    
--- Lists all Categories and their modules together with their instances
--- in the given period.
showCategoryPlanController
  :: Either StudyProgram String -> [(Either Category String,[ModData])]
  -> (String,Int) -> (String,Int) -> Bool -> Bool -> Controller
showCategoryPlanController mbstudyprog catmods startsem stopsem
                           withunivis withmprogs = do
  admin <- isAdmin
  login <- getSessionLogin
  prefs <- getSessionUserPrefs
  users <- runQ queryAllUsers
  catmodinsts <- runJustT $
                   mapT (\ (c,mods) ->
                               mapT getModInsts
                                    (maybe (filter modDataVisible mods)
                                           (const mods)
                                           login              ) |>>= \mmis ->
                               returnT (c,mmis))
                        catmods
  return (listCategoryView admin login prefs mbstudyprog
             catmodinsts semPeriod users
             showCategoryPlanController
             formatCatModulesForm showEmailCorrectionController)
 where
   getModInsts md =
     getDB (queryInstancesOfMod (modDataKey md)) |>>= \mis ->
     (if withmprogs
      then getDB (transformQ (map length)
              $ getMasterProgramKeysOfModInst mis)
      else returnT (repeat 0)) |>>= \nummps ->
     (if withunivis
      then mapT (\s -> getDB $ queryHasUnivisEntry (modDataCode md) s) semPeriod
      else returnT []) |>>= \univs ->
     returnT (md,map (instOfSem (zip mis nummps)) semPeriod,univs)

   instOfSem misnums sem =
     find (\ (mi,_) -> (modInstTerm mi,modInstYear mi) == sem) misnums

   semPeriod = takeWhile (\s -> leqSemester s stopsem)
                         (iterate nextSemester startsem)

--- Lists all Categories and their modules together with their instances
--- in the given period.
showEmailCorrectionController
  :: Either StudyProgram String -> [(Either Category String,[ModData])]
  -> (String,Int) -> (String,Int) -> Controller
showEmailCorrectionController mbstudyprog catmods startsem stopsem = do
  login <- getSessionLogin
  users <- runQ queryAllUsers
  modinsts <- runJustT $
                   mapT (\ (_,mods) ->
                               mapT getModInsts
                                    (maybe (filter modDataVisible mods)
                                           (const mods)
                                           login              ) |>>= \mmis ->
                               returnT mmis)
                        catmods
  return (listEmailCorrectionView mbstudyprog
                                  (concat modinsts) semPeriod users)
 where
   getModInsts md =
     getDB (queryInstancesOfMod (modDataKey md)) |>>= \mis ->
     mapT (\s -> getDB $ queryHasUnivisEntry (modDataCode md) s) semPeriod
      |>>= \univs ->
     returnT (md,map (instOfSem mis) semPeriod,univs)

   instOfSem mis sem =
     find (\mi -> (modInstTerm mi,modInstYear mi) == sem) mis

   semPeriod = takeWhile (\s -> leqSemester s stopsem)
                         (iterate nextSemester startsem)

--- Shows a Category entity.
showCategoryController :: Category -> Controller
showCategoryController category =
  checkAuthorization (categoryOperationAllowed (ShowEntity category)) $
   (do programCategoriesStudyProgram <- runJustT
                                         (getProgramCategoriesStudyProgram
                                           category)
       return
        (showCategoryView category programCategoriesStudyProgram))

--- Gets the associated StudyProgram entity for a given Category entity.
getProgramCategoriesStudyProgram :: Category -> Transaction StudyProgram
getProgramCategoriesStudyProgram cStudyProgram =
  getStudyProgram (categoryStudyProgramProgramCategoriesKey cStudyProgram)

--- Query the categories of a given StudyProgram.
queryCategorysOfStudyProgram :: StudyProgramKey -> Query [Category]
queryCategorysOfStudyProgram sp =
  transformQ (mergeSort leqCategory) $
   queryCondCategory (\c -> categoryStudyProgramProgramCategoriesKey c == sp)

