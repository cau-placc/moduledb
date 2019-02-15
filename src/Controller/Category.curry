module Controller.Category
 ( categoryController, showCategoryPlanController,
   showEmailCorrectionController
 ) where

import System.Spicey
import HTML.Base
import Time
import MDB
import MDB.Queries
import MDBExts
import View.Category
import View.ModData
import Maybe
import System.Authorization
import System.AuthorizedActions
import Config.UserProcesses
import System.Authentication
import Controller.Default
import Controller.ModData
import System.Helpers
import List
import Sort
import System.SessionInfo
import System.MultiLang
import View.MDBEntitiesToHtml
import View.ModInst (leqModInst)
import System.StudyPlanner

--- Choose the controller for a Category entity according to the URL parameter.
categoryController :: Controller
categoryController = do
  args <- getControllerParams
  case args of
    ["list"]    -> listAllCategoryController
    ["user"]    -> listUserModulesController False False
    ["userall"] -> listUserModulesController False True
    ["lecturer"]    -> listUserModulesController True False
    ["lecturerall"] -> listUserModulesController True True
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
  :: (String,String,String,String,Int,Int,Int,StudyProgram) -> DBAction ()
createCategoryT
    (name,nameE,shortName,comment,minECTS,maxECTS,position,studyProgram) = do
  newCategoryWithStudyProgramProgramCategoriesKey name nameE shortName comment
   (Just minECTS)
   (Just maxECTS)
   position
   (studyProgramKey studyProgram)
  return ()

--- Shows a form to edit the given Category entity.
editCategoryController :: Category -> Controller
editCategoryController categoryToEdit =
  checkAuthorization (categoryOperationAllowed (UpdateEntity categoryToEdit))
   $ \_ ->
   (do allStudyPrograms <- runQ queryAllStudyPrograms
       programCategoriesStudyProgram <-
         runJustT (getProgramCategoriesStudyProgram categoryToEdit)
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
     continueOrError
        (\ _ -> nextInProcessOr (showCategoryController category) Nothing)
        transResult

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
       continueOrError (\ _ -> listAllCategoryController) transResult)

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
                 (showCategoryPlanController Nothing) formatCatModulesForm
                 showEmailCorrectionController)

--- Controller to list all modules of the current user.
--- If the first argument is true, the modules taught by the user are shown.
--- The second argument indicates whether all modules should be shown.
--- If it is false, only the modules without or with most recent instances
--- are shown.
listUserModulesController :: Bool -> Bool -> Controller
listUserModulesController aslecturer listall =
  checkAuthorization (categoryOperationAllowed ListEntities) $ \sinfo ->
    do let lname = maybe "" id (userLoginOfSession sinfo)
       -- get user entry with a given login name
       mbuser <- runQ $ queryUserWithLogin lname
       case mbuser of
         Nothing   -> displayError "Illegal URL"
         Just user -> do
           usermods <- runQ $ (if aslecturer then queryModDataOfLecturer
                                             else queryModDataOfUser    ) (userKey user)
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
             (showCategoryPlanController (if aslecturer then Just user else Nothing))
             formatCatModulesForm showEmailCorrectionController
 where
   listCatHeader t =
     if listall
       then [htxt $ t alltitle]
       else [htxt $ t $ (if aslecturer then "Taught" else "My") ++ " modules", nbsp,
             htxt "(", href ("?Category/" ++ allhref) [htxt $ t alltitle], htxt ")"]
    where alltitle = "All " ++ (if aslecturer then "taught" else "my") ++ " modules"
          allhref  = if aslecturer then "lecturerall" else "userall"
   
   addModInsts md = runJustT $ do
     mis <- queryInstancesOfMod (modDataKey md)
     return (md, mis)

   -- Has a module empty instances or instances in the current semester frame?
   isCurrentModInst cursem (_,mis) =
     null mis || not (null (intersect (map modInstSemester mis)
                                      (drop 2 (semesterSelection cursem))))

--- Lists a study program with all its Category entities.
listStudyProgramCategoryController :: Bool -> StudyProgram -> Controller
listStudyProgramCategoryController listall studyprog =
  checkAuthorization (categoryOperationAllowed ListEntities) $ \sinfo ->
    do categorys <- runQ $ liftM (mergeSortBy leqCategory) $
                             queryCategorysOfStudyProgram
                                           (studyProgramKey studyprog)
       catmods <- runJustT $
        if listall
        then mapM (\c -> do
                     mods <- getModDataOfCategory c
                     returnT (Left c,map (\m->(m,[],[]))
                                      (maybe (filter modDataVisible mods)
                                             (const mods)
                                             (userLoginOfSession sinfo))))
                  categorys
        else mapM (\c -> return (Left c,[])) categorys
       csem  <- getCurrentSemester
       return (listCategoryView sinfo csem (Left studyprog)
                  catmods [] []
                  (showCategoryPlanController Nothing)
                  formatCatModulesForm showEmailCorrectionController)

--- Lists all Categories and their modules together with their instances
--- in the given period.
showCategoryPlanController
  :: Maybe User -> Either StudyProgram [HtmlExp] -> [(Either Category String,[ModData])]
  -> (String,Int) -> (String,Int) -> Bool -> Bool -> Bool -> Controller
showCategoryPlanController mblecturer mbstudyprog catmods startsem stopsem
                           withunivis withmprogs withstudyplan = do
  appendFile "TESTLOG" (show mblecturer)
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
             (showCategoryPlanController mblecturer)
             formatCatModulesForm showEmailCorrectionController)
 where
   getModInsts md =
    let queryinsts = maybe (queryInstancesOfMod (modDataKey md))
                           (\u -> queryLecturedInstancesOfMod (modDataKey md) (userKey u))
                           mblecturer
    in if withstudyplan
         then do
           mis <- runJustT queryinsts
           studnums <- getModuleInstancesStudents md mis
           return (md,map (instOfSem (zip mis studnums)) semPeriod, [])
         else runJustT $ do
           mis <- queryinsts
           -- compute usages in old master programs (unnecessary in the future):
           nummps <- if withmprogs
                     then liftM (map length) (getMasterProgramKeysOfModInst mis)
                     else return (repeat 0)
           -- compute usages in AdvisorStudyPrograms:
           numaps <- if withmprogs
                       then liftM (map length)
                                  (mapM getAdvisorStudyProgramKeysOfModInst mis)
                       else return (repeat 0)
           let numbermprogs = map (uncurry (+)) (zip nummps numaps)
           univs <- if withunivis
                      then mapM (queryHasUnivisEntry (modDataCode md)) semPeriod
                      else return []
           return (md,map (instOfSem (zip mis numbermprogs)) semPeriod,univs)

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
                   mapM (\ (_,mods) ->
                               mapM getModInsts
                                    (maybe (filter modDataVisible mods)
                                           (const mods)
                                           (userLoginOfSession sinfo)))
                        catmods
  return (listEmailCorrectionView mbstudyprog
                                  (concat modinsts) semPeriod users)
 where
   getModInsts md = do
     mis <- queryInstancesOfMod (modDataKey md)
     univs <- mapM (\s -> queryHasUnivisEntry (modDataCode md) s) semPeriod
     return (md, map (instOfSem mis) semPeriod, univs)

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
       mods <- runJustT $ getModDataOfCategory cat
       csem  <- getCurrentSemester
       return (listCategoryView sinfo csem
                 (maybe (Right [htxt "???"]) Left mbsprog)
                 [(Left cat,map (\m->(m,[],[]))
                                (maybe (filter modDataVisible mods)
                                       (const mods)
                                       (userLoginOfSession sinfo)))]
                 [] []
                 (showCategoryPlanController Nothing)
                 formatCatModulesForm showEmailCorrectionController))

--- Gets the associated StudyProgram entity for a given Category entity.
getProgramCategoriesStudyProgram :: Category -> DBAction StudyProgram
getProgramCategoriesStudyProgram cStudyProgram =
  getStudyProgram (categoryStudyProgramProgramCategoriesKey cStudyProgram)
