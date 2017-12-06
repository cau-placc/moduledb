module CategoryController (
 categoryController, showCategoryPlanController,
 showEmailCorrectionController
 ) where

import Spicey
import KeyDatabase
import HTML.Base
import Time
import MDB
import MDBExts
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
import SessionInfo
import MultiLang
import MDBEntitiesToHtml
import ModInstView (leqModInst)
import StudyPlanner

--- Choose the controller for a Category entity according to the URL parameter.
categoryController :: Controller
categoryController = do
  args <- getControllerParams
  case args of
    ["list"]    -> listAllCategoryController
    ["user"]    -> listCurrentUserCategoryController False
    ["userall"] -> listCurrentUserCategoryController True
    ["new"]     -> newCategoryController
    ["studyprogram",s] ->
       applyControllerOn (readStudyProgramKey s)
         getStudyProgram (listStudyProgramCategoryController False)
    ["studyprogramall",s] ->
       applyControllerOn (readStudyProgramKey s)
         getStudyProgram (listStudyProgramCategoryController True)
    ["show",s] -> applyControllerOn (readCategoryKey s)
                    getCategory showCategoryController
    ["edit",s] -> applyControllerOn (readCategoryKey s)
                    getCategory editCategoryController
    ["delete",s] -> applyControllerOn (readCategoryKey s)
                      getCategory askAndDeleteCategoryController
    _ -> displayError "Illegal URL"


--- Shows a form to create a new Category entity.
newCategoryController :: Controller
newCategoryController =
  checkAuthorization (categoryOperationAllowed NewEntity)
   $ (\sinfo ->
     do allStudyPrograms <- runQ queryAllStudyPrograms
        return
         (blankCategoryView sinfo allStudyPrograms
           (\entity ->
             transactionController (createCategoryT entity)
              (nextInProcessOr listAllCategoryController Nothing))
           listAllCategoryController))

--- Transaction to persist a new Category entity to the database.
createCategoryT
  :: (String,String,String,String,Int,Int,Int,StudyProgram) -> Transaction ()
createCategoryT
    (name,nameE,shortName,comment,minECTS,maxECTS,position,studyProgram) =
  newCategoryWithStudyProgramProgramCategoriesKey name nameE shortName comment
   (Just minECTS)
   (Just maxECTS)
   position
   (studyProgramKey studyProgram)
   |>> returnT ()

--- Shows a form to edit the given Category entity.
editCategoryController :: Category -> Controller
editCategoryController categoryToEdit =
  checkAuthorization (categoryOperationAllowed (UpdateEntity categoryToEdit))
   $ \_ ->
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
  confirmControllerOLD
    (h3 [htxt (concat ["Kategorie \"",categoryToShortView cat
                      ,"\" wirklich löschen?"])])
    (\ack -> if ack
             then deleteCategoryController cat
             else showCategoryController cat)

--- Deletes a given Category entity and proceeds with the list controller.
deleteCategoryController :: Category -> Controller
deleteCategoryController category =
  checkAuthorization (categoryOperationAllowed (DeleteEntity category)) $ \_ ->
   (do transResult <- runT (deleteCategory category)
       either (\ _ -> listAllCategoryController)
        (\ error -> displayError (showTError error)) transResult)

--- Lists all Category entities with buttons to show, delete,
--- or edit an entity.
listAllCategoryController :: Controller
listAllCategoryController =
  checkAuthorization (categoryOperationAllowed ListEntities) $ \sinfo ->
    do let t = translate sinfo
       csem  <- getCurrentSemester
       categorys <- runQ queryAllCategorys
       return (listCategoryView sinfo csem (Right [htxt $ t "All categories"])
                 (map (\c -> (Left c,[])) (mergeSortBy leqCategory categorys))
                 [] []
                 showCategoryPlanController formatCatModulesForm
                 showEmailCorrectionController)

--- Controller to list all modules of the current user.
--- The argument indicates whether all modules should be shown.
--- If it is false, only the modules without or with most recent instances
--- are shown.
listCurrentUserCategoryController :: Bool -> Controller
listCurrentUserCategoryController listall =
  checkAuthorization (categoryOperationAllowed ListEntities) $ \sinfo ->
    do let lname = maybe "" id (userLoginOfSession sinfo)
       -- get user entries with a given login name
       users <- runQ $ queryCondUser (\u -> userLogin u == lname)
       if null users then (displayError "Illegal URL") else
        do usermods <- runQ $ queryModDataOfUser (userKey (head users))
           csem  <- getCurrentSemester
           showmods <- if listall
                         then return usermods
                         else mapIO addModInsts usermods >>=
                              return . map fst . filter (isCurrentModInst csem)
           let t = translate sinfo
           return $ listCategoryView sinfo csem
                        (Right $ listCatHeader t)
                        [(Right "",map (\m->(m,[],[])) showmods)]
                        [] []
                        showCategoryPlanController
                        formatCatModulesForm showEmailCorrectionController
 where
   listCatHeader t =
     if listall
       then [htxt $ t "All my modules"]
       else [htxt $ t "My modules", nbsp, htxt "(",
             href "?Category/userall" [htxt $ t "All my modules"], htxt ")"]
             
   addModInsts md = runJustT $
     getDB (queryInstancesOfMod (modDataKey md)) |>>= \mis ->
     returnT (md, mis)

   -- Has a module empty instances or instances in the current semester frame?
   isCurrentModInst cursem (_,mis) =
     null mis || not (null (intersect (map modInstSemester mis)
                                      (drop 2 (semesterSelection cursem))))

--- Lists a study program with all its Category entities.
listStudyProgramCategoryController :: Bool -> StudyProgram -> Controller
listStudyProgramCategoryController listall studyprog =
  checkAuthorization (categoryOperationAllowed ListEntities) $ \sinfo ->
    do categorys <- runQ $ transformQ (mergeSortBy leqCategory) $
                             queryCategorysOfStudyProgram
                                           (studyProgramKey studyprog)
       catmods <- runJustT $
        if listall
        then mapT (\c ->
                     getModDataOfCategory (categoryKey c) |>>= \mods ->
                     returnT (Left c,map (\m->(m,[],[]))
                                      (maybe (filter modDataVisible mods)
                                             (const mods)
                                             (userLoginOfSession sinfo))))
                  categorys
        else mapT (\c -> returnT (Left c,[])) categorys
       csem  <- getCurrentSemester
       return (listCategoryView sinfo csem (Left studyprog)
                  catmods [] []
                  showCategoryPlanController
                  formatCatModulesForm showEmailCorrectionController)

--- Lists all Categories and their modules together with their instances
--- in the given period.
showCategoryPlanController
  :: Either StudyProgram [HtmlExp] -> [(Either Category String,[ModData])]
  -> (String,Int) -> (String,Int) -> Bool -> Bool -> Bool -> Controller
showCategoryPlanController mbstudyprog catmods startsem stopsem
                           withunivis withmprogs withstudyplan = do
  sinfo <- getUserSessionInfo
  csem  <- getCurrentSemester
  let filterMods ms = maybe (filter modDataVisible ms)
                            (const ms)
                            (userLoginOfSession sinfo)
  users <- runQ queryAllUsers
  catmodinsts <- mapIO (\ (c,mods) ->
                               mapIO getModInsts (filterMods mods) >>= \mmis ->
                               return (c,mmis))
                       catmods
  return (listCategoryView sinfo csem mbstudyprog
             catmodinsts semPeriod users
             showCategoryPlanController
             formatCatModulesForm showEmailCorrectionController)
 where
   getModInsts md =
    if withstudyplan
    then do
     mis <- runJustT (getDB (queryInstancesOfMod (modDataKey md)))
     studnums <- getModuleInstancesStudents md mis
     return (md,map (instOfSem (zip mis studnums)) semPeriod, [])
    else runJustT $
     getDB (queryInstancesOfMod (modDataKey md)) |>>= \mis ->
     (if withmprogs
      then getDB (transformQ (map length)
                             (getMasterProgramKeysOfModInst mis))
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
  :: Either StudyProgram [HtmlExp] -> [(Either Category String,[ModData])]
  -> (String,Int) -> (String,Int) -> Controller
showEmailCorrectionController mbstudyprog catmods startsem stopsem = do
  sinfo <- getUserSessionInfo
  users <- runQ queryAllUsers
  modinsts <- runJustT $
                   mapT (\ (_,mods) ->
                               mapT getModInsts
                                    (maybe (filter modDataVisible mods)
                                           (const mods)
                                           (userLoginOfSession sinfo)) |>>= \mmis ->
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
showCategoryController cat =
  checkAuthorization (categoryOperationAllowed (ShowEntity cat)) $ \sinfo ->
   (do sprogs <- runQ queryAllStudyPrograms
       let spk     = categoryStudyProgramProgramCategoriesKey cat
           mbsprog = find (\p -> studyProgramKey p == spk) sprogs
       mods <- runJustT $ getModDataOfCategory (categoryKey cat)
       csem  <- getCurrentSemester
       return (listCategoryView sinfo csem
                 (maybe (Right [htxt "???"]) Left mbsprog)
                 [(Left cat,map (\m->(m,[],[]))
                                (maybe (filter modDataVisible mods)
                                       (const mods)
                                       (userLoginOfSession sinfo)))]
                 [] []
                 showCategoryPlanController
                 formatCatModulesForm showEmailCorrectionController))

--- Gets the associated StudyProgram entity for a given Category entity.
getProgramCategoriesStudyProgram :: Category -> Transaction StudyProgram
getProgramCategoriesStudyProgram cStudyProgram =
  getStudyProgram (categoryStudyProgramProgramCategoriesKey cStudyProgram)
