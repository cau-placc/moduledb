module Controller.Category
 ( categoryController, newCategoryWuiForm, editCategoryForm, semSelectForm
 , listCategoryController, emailCorrectionForm
 ) where

import Global
import Maybe
import List
import Sort
import Time

import HTML.Base
import HTML.Session
import HTML.Styles.Bootstrap4
import HTML.WUI

import Config.UserProcesses
import Config.EntityRoutes
import MDB
import MDB.Queries
import MDBExts
import View.Category
import View.ModData
import System.Authorization
import System.AuthorizedActions
import System.Authentication
import Controller.ModData
import Controller.StudyProgram
import System.Helpers
import System.SessionInfo
import System.MultiLang
import System.Spicey
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
       controllerOnKey s (listStudyProgramCategoryController False)
    ["studyprogramall",s] ->
       controllerOnKey s (listStudyProgramCategoryController True)
    ["show",s] -> controllerOnKey s showCategoryController
    ["edit",s] -> controllerOnKey s editCategoryController
    ["delete",s] -> controllerOnKey s askAndDeleteCategoryController
    ["destroy",s] -> controllerOnKey s destroyCategoryController
    _ -> displayUrlError

------------------------------------------------------------------------------
--- Shows a form to create a new Category entity.
newCategoryController :: Controller
newCategoryController =
  checkAuthorization (categoryOperationAllowed NewEntity) $ \sinfo -> do
    allStudyPrograms <- runQ queryAllStudyPrograms
    setParWuiStore newCategoryWuiStore (sinfo, allStudyPrograms)
                   ("", "", "", "", 0, 180, 0, head allStudyPrograms)
    return [formElem newCategoryWuiForm]

type NewCategory = (String,String,String,String,Int,Int,Int,StudyProgram)

--- A WUI form to create a new Category entity.
--- The default values for the fields are stored in the
--- `newCategoryWuiStore`.
newCategoryWuiForm ::
  HtmlFormDef ((UserSessionInfo,[StudyProgram]), WuiStore NewCategory)
newCategoryWuiForm =
  pwui2FormDef "Controller.Category.newCategoryWuiForm"
    newCategoryWuiStore
    (\ (_,allstudyprogs) -> wCategory allstudyprogs)
    (\_ entity ->
       checkAuthorization (categoryOperationAllowed NewEntity) $ \_ ->
         transactionController (createCategoryT entity)
           (nextInProcessOr redirectToDefaultController Nothing))
    (\ (sinfo,_) ->
       renderWUI sinfo "Neuen Kategorie" "Anlegen" "?" ())

---- The data stored for executing the WUI form.
newCategoryWuiStore ::
  Global (SessionStore ((UserSessionInfo,[StudyProgram]), WuiStore NewCategory))
newCategoryWuiStore =
  global emptySessionStore (Persistent (inSessionDataDir "newCategoryWuiStore"))


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

------------------------------------------------------------------------------
--- Shows a form to edit the given Category entity.
editCategoryController :: Category -> Controller
editCategoryController category =
  checkAuthorization (categoryOperationAllowed (UpdateEntity category))
   $ \sinfo -> do
    allStudyPrograms <- runQ queryAllStudyPrograms
    programCategoriesStudyProgram <-
      runJustT (getProgramCategoriesStudyProgram category)
    setParWuiStore wuiEditCategoryStore
      (sinfo, category, programCategoriesStudyProgram, allStudyPrograms)
      category
    return [formElem editCategoryForm]

--- A form to edit the given Category entity.
editCategoryForm ::
  HtmlFormDef ((UserSessionInfo,Category,StudyProgram,[StudyProgram]),
               WuiStore Category)
editCategoryForm = pwui2FormDef "Controller.Category.editCategoryForm"
  wuiEditCategoryStore
  (\ (_,cat,studyprog,allprogs) -> wCategoryType cat studyprog allprogs)
  (\ (_,cat,_,_) entity ->
     checkAuthorization (categoryOperationAllowed (UpdateEntity cat)) $ \_ ->
     updateCategoryController entity)
  (\ (sinfo,cat,_,_) ->
       renderWUI sinfo "Kategorie ändern" "Change"
         ("?Category/show/" ++ showCategoryKey cat) ())

--- The data stored for executing the WUI form.
wuiEditCategoryStore ::
  Global (SessionStore ((UserSessionInfo,Category,StudyProgram,[StudyProgram]),
                        WuiStore Category))
wuiEditCategoryStore =
  global emptySessionStore (Persistent (inSessionDataDir "wuiEditCategoryStore"))


--- Persists modifications of a given Category entity to the
--- database depending on the Boolean argument. If the Boolean argument
--- is False, nothing is changed.
updateCategoryController :: Category -> Controller
updateCategoryController category =
  do transResult <- runT (updateCategory category)
     continueOrError
        (\ _ -> nextInProcessOr (showCategoryController category) Nothing)
        transResult

--------------------------------------------------------------------------
--- Deletes a given Category entity (after asking for acknowledgment)
--- and proceeds with the show controller.
askAndDeleteCategoryController :: Category -> Controller
askAndDeleteCategoryController cat =
  checkAuthorization (categoryOperationAllowed (DeleteEntity cat)) $ \si ->
  confirmDeletionPage si $ concat
    ["Kategorie \"", categoryToShortView cat, "\" wirklich löschen?"]

--- Deletes a given Category entity and proceeds with the list controller.
destroyCategoryController :: Category -> Controller
destroyCategoryController category =
  checkAuthorization (categoryOperationAllowed (DeleteEntity category)) $ \_ ->
   (do transResult <- runT (deleteCategory category)
       continueOrError (\ _ -> listAllCategoryController) transResult)

--------------------------------------------------------------------------
--- A form to select a semester period to show the module instances
--- in this period.

semSelectStore ::
  Global (SessionStore (Maybe User, Either StudyProgram [BaseHtml],
                        [(Either Category String, [ModData])],[(String,Int)]))
semSelectStore =
  global emptySessionStore (Persistent (inSessionDataDir "semSelectStore"))

--- An operation to store the data required for this view.
storeCategoryListData :: Maybe User
  -> Either StudyProgram [BaseHtml] -> [(Either Category String, [ModData])]
  -> [(String,Int)]
  -> IO ()
storeCategoryListData mbuser sproghtml catmods semperiod =
  writeSessionData semSelectStore (mbuser,sproghtml,catmods,semperiod)

--- An operation to read the data required for this view.
readCategoryListData ::
  IO ( Maybe User, Either StudyProgram [BaseHtml]
     , [(Either Category String, [ModData])], [(String,Int)])
readCategoryListData = fromFormReader $
   getSessionData semSelectStore (Nothing, Right [], [], [])

--- The actual form to select a semester period.
semSelectForm ::
  HtmlFormDef ( UserSessionInfo, Maybe User, Either StudyProgram [BaseHtml]
              , [(Either Category String, [ModData])]
              , (String,Int), [(String,Int)])
semSelectForm =
  formDefWithID "Controller.Category.semSelectForm" readData
    (selSemesterPlanningView showCategoryPlanController
                             showEmailCorrectionController
                             formatCatModulesForm)
 where
  readData = toFormReader $ do
    sinfo  <- getUserSessionInfo
    cursem <- getCurrentSemester
    (mbuser,sproghtml,catmods,semperiod) <- readCategoryListData
    return (sinfo,mbuser,sproghtml,catmods,cursem,semperiod)

--------------------------------------------------------------------------
--- Lists a given list of categoy/module entities.
listCategoryController :: UserSessionInfo -> Either StudyProgram [BaseHtml]
                       -> [(Either Category String, [ModData])]
                       -> [(String,Int)] -> Controller
listCategoryController sinfo sproghtml catmods semperiod = do
  storeCategoryListData Nothing sproghtml catmods semperiod
  return $ listCategoryView sinfo sproghtml
             (map (\ (cat,mods) -> (cat, map (\m -> (m,[],[])) mods)) catmods)
             [] [] (formElem semSelectForm)

--------------------------------------------------------------------------
--- Lists all Category entities with buttons to show, delete,
--- or edit an entity.
listAllCategoryController :: Controller
listAllCategoryController =
  checkAuthorization (categoryOperationAllowed ListEntities) $ \sinfo ->
    do let t = translate sinfo
       categorys <- runQ queryAllCategorys
       listCategoryController sinfo (Right [htxt $ t "All categories"])
         (map (\c -> (Left c,[])) (mergeSortBy leqCategory categorys)) []

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
         Nothing   -> displayUrlError
         Just user -> do
           usermods <- runQ $ (if aslecturer then queryModDataOfLecturer
                                             else queryModDataOfUser    ) (userKey user)
           csem  <- getCurrentSemester
           showmods <- if listall
                         then return usermods
                         else mapIO addModInsts usermods >>=
                              return . map fst . filter (isCurrentModInst csem)
           let t = translate sinfo
           storeCategoryListData (if aslecturer then Just user else Nothing)
                                 (Right $ listCatHeader t)
                                 [(Right "", showmods)] []
           return $ listCategoryView sinfo
             (Right $ listCatHeader t)
             [(Right "",map (\m->(m,[],[])) showmods)]
             [] [] (formElem semSelectForm)
 where
   listCatHeader t =
     if listall
       then [htxt $ t alltitle]
       else [htxt $ t $ (if aslecturer then "Taught" else "My") ++ " modules",
             nbsp,
             htxt "(",
             hrefScndBadge ("?Category/" ++ allhref) [htxt $ t alltitle],
             htxt ")"]
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
                       returnT (Left c, maybe (filter modDataVisible mods)
                                              (const mods)
                                              (userLoginOfSession sinfo)))
                    categorys
          else mapM (\c -> return (Left c,[])) categorys
       listCategoryController sinfo (Left studyprog) catmods []

--- Lists all Categories and their modules together with their instances
--- in the given period.
showCategoryPlanController
  :: Maybe User -> Either StudyProgram [BaseHtml]
  -> [(Either Category String, [ModData])]
  -> (String,Int) -> (String,Int) -> Bool -> Bool -> Bool -> Controller
showCategoryPlanController mblecturer mbstudyprog catmods startsem stopsem
                           withunivis withmprogs withstudyplan = do
  sinfo <- getUserSessionInfo
  let filterMods ms = maybe (filter modDataVisible ms)
                            (const ms)
                            (userLoginOfSession sinfo)
  users <- runQ queryAllUsers
  catmodinsts <- mapIO (\ (c,mods) ->
                               mapIO getModInsts (filterMods mods) >>= \mmis ->
                               return (c,mmis))
                       catmods
  storeCategoryListData mblecturer mbstudyprog
    (map (\ (cat,modinsts) -> (cat, map (\ (m,_,_) -> m) modinsts)) catmodinsts)
    semPeriod
  return (listCategoryView sinfo mbstudyprog
             catmodinsts semPeriod users (formElem semSelectForm))
 where
   getModInsts md =
    let queryinsts = maybe
          (queryInstancesOfMod (modDataKey md))
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

------------------------------------------------------------------------------
--- A controller to list categories, modules, instances, combined
--- with their UnivIS instances, in order to send emails to the
--- responsible person to correct their entries.

emailCorrectionStore ::
  Global (SessionStore ([(ModData,[Maybe ModInst],[Bool])], [(String,Int)]))
emailCorrectionStore =
 global emptySessionStore (Persistent (inSessionDataDir "emailCorrectionStore"))

--- The actual form to send email corrections.
emailCorrectionForm ::
  HtmlFormDef ( [(ModData,[Maybe ModInst],[Bool])], [(String,Int)], [User] )
emailCorrectionForm =
  formDefWithID "Controller.Category.emailCorrectionForm" readData
    (\ (modinsts,semperiod,users) ->
      listEmailCorrectionView modinsts semperiod users)
 where
  readData = do
    (modinsts,semperiod) <- getSessionData emailCorrectionStore ([], [])
    users <- toFormReader $ runQ queryAllUsers
    return (modinsts,semperiod,users)

--- Lists all Categories and their modules together with their instances
--- in the given period.
showEmailCorrectionController
  :: Either StudyProgram [BaseHtml] -> [(Either Category String,[ModData])]
  -> (String,Int) -> (String,Int) -> Controller
showEmailCorrectionController mbsprog catmods startsem stopsem = do
  sinfo <- getUserSessionInfo
  modinsts <- runJustT $
                   mapM (\ (_,mods) ->
                               mapM getModInsts
                                    (maybe (filter modDataVisible mods)
                                           (const mods)
                                           (userLoginOfSession sinfo)))
                        catmods
  writeSessionData emailCorrectionStore (concat modinsts, semPeriod)
  return [h1 $ either (\sp -> [htxt $ studyProgramName sp]) id mbsprog,
          formElem emailCorrectionForm]
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
  checkAuthorization (categoryOperationAllowed (ShowEntity cat)) $ \sinfo -> do
    sprogs <- runQ queryAllStudyPrograms
    let spk     = categoryStudyProgramProgramCategoriesKey cat
        mbsprog = find (\p -> studyProgramKey p == spk) sprogs
    mods <- runJustT $ getModDataOfCategory cat
    listCategoryController sinfo
      (maybe (Right [htxt "???"]) Left mbsprog)
      [(Left cat, maybe (filter modDataVisible mods)
                        (const mods)
                        (userLoginOfSession sinfo))] []

--- Gets the associated StudyProgram entity for a given Category entity.
getProgramCategoriesStudyProgram :: Category -> DBAction StudyProgram
getProgramCategoriesStudyProgram cStudyProgram =
  getStudyProgram (categoryStudyProgramProgramCategoriesKey cStudyProgram)
