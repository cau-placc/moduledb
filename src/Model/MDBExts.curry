{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=foreigncode #-}

--- Some extensions to the generated MDB module.

module MDBExts where

import ReadShowTerm ( readQTerm )

import Database.CDBI.ER

import System.Helpers
import MDB
import ConfigMDB ( storageDir )

-- store DBs in term files:
storeTermDB :: IO ()
storeTermDB = saveDBTo storageDir

-- initialize DBs from term files:
readTermDB :: IO ()
readTermDB = restoreDBFrom storageDir

--- Transforms an SQL list result into a maybe result (of first element).
sqlToMaybe :: DBAction [a] -> DBAction (Maybe a)
sqlToMaybe = liftM (\xs -> if null xs then Nothing else Just (head xs))

-----------------------------------------------------------------------
--- Shows the key of a MasterProgram entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
masterProgramKeyToString :: MasterProgramID -> String
masterProgramKeyToString key =
  Database.CDBI.ER.showDatabaseKey "MasterProgram" masterProgramKeyToInt key

-----------------------------------------------------------------------
--- Gets all MasterProgram entities belonging to a user.
queryMasterProgramMainInfos
  :: DBAction [(MasterProgramID,String,String,Int,Bool,MasterCoreAreaID)]
queryMasterProgramMainInfos =
  ``sql* Select mp.Key, mp.Name, mp.Term, mp.Year, mp.Visible,
                mp.MasterCoreAreaAreaProgramsKey
         From MasterProgram As mp;''

--- Gets the MasterProgInfo entity associated to a MasterProgram key.
queryInfoOfMasterProgram :: MasterProgramID -> DBAction (Maybe MasterProgInfo)
queryInfoOfMasterProgram mpk = sqlToMaybe
  ``sql* Select * From MasterProgInfo As mpi
         Where mpi.MasterProgramProgramInfoKey = {mpk};''

--- Gets the key/code/name of all visible ModData entities.
queryAllModDataShortInfo :: DBAction [(ModDataID,String,String)]
queryAllModDataShortInfo =
  ``sql* Select md.Key, md.Code, md.NameG
         From ModData As md
         Where md.Visible = true;''

--- Gets all ModData entities with a given module code.
queryModDataWithCode :: String -> DBAction [ModData]
queryModDataWithCode mcode =
  ``sql* Select *
         From ModData As md
         Where md.Code = {mcode};''

--- Gets all ModData entities of a user.
queryModDataOfUser :: UserID -> DBAction [ModData]
queryModDataOfUser ukey =
  ``sql* Select * From ModData As mda
         Where mda.UserResponsibleKey = {ukey};''

--- Gets the ModDescr entity associated to a given ModData key.
queryDescriptionOfMod :: ModDataID -> DBAction (Maybe ModDescr)
queryDescriptionOfMod mdk = sqlToMaybe
  ``sql* Select * From ModDescr As mde
         Where Exists
          ( Select * From ModData As mda
            Where mda.Key = {mdk} And Satisfies mda withDesc mde);''

--- Gets the Exam attribute associated to a given ModData key.
queryExamOfMod :: ModDataID -> DBAction (Maybe String)
queryExamOfMod mdk = sqlToMaybe
  ``sql* Select mde.Exam
         From ModData As mda, ModDescr As mde
         Where mda.Key = {mdk} And Satisfies mda withDesc mde;''

--- Gets all module instances for a given module (key).
queryInstancesOfMod :: ModDataID -> DBAction [ModInst]
queryInstancesOfMod mdk =
  ``sql* Select * From ModInst As mi
         Where mi.ModDataModuleInstancesKey = {mdk};''

--- Gets all MasterProgram entities belonging to a user.
queryMasterProgramOfUser :: UserID -> DBAction [MasterProgram]
queryMasterProgramOfUser ukey =
  ``sql* Select * From MasterProgram As mp
         Where mp.UserAdvisingKey = {ukey};''

--- Gets all MasterProgram (keys) for each ModInst of a given ModInst list.
getMasterProgramKeysOfModInst :: [ModInst] -> DBAction [[MasterProgramID]]
getMasterProgramKeysOfModInst mis =
 liftM
   (\mpis ->
     map (\mi -> let mdk = modInstModDataModuleInstancesKey mi in
           map snd
               (filter (\mpi ->
                         any (\ (_,_,smpk,trm,yr) ->
                               readModDataKey smpk == Just mdk &&
                               modInstTerm mi == trm && modInstYear mi == yr)
                             (readProgModules (fst mpi)))
                       mpis))
         mis)
   ``sql* Select mpi.ProgModules, mpi.MasterProgramProgramInfoKey
          From MasterProgInfo As mpi;''

-- to avoid typing problem with kics2
readProgModules :: String -> [(String,Bool,String,String,Int)]
readProgModules s = readQTerm s


--- Query to get the AdvisorModules where a module instance is used.
queryAdvisorStudyProgramOfModInst :: ModInst -> DBAction [AdvisorModule]
queryAdvisorStudyProgramOfModInst mi =
  ``sql* Select * From AdvisorModule As am
         Where am.ModInstAdvisedProgramModuleInstancesKey = {modInstKey mi};''

--- Gets all AdvisorStudyProgram (keys) for a given ModInst.
getAdvisorStudyProgramKeysOfModInst :: ModInst
                                    -> DBAction [AdvisorStudyProgramID]
getAdvisorStudyProgramKeysOfModInst mi =
  ``sql* Select am.AdvisorStudyProgramAdvisorProgramModulesKey
         From AdvisorModule As am
         Where am.ModInstAdvisedProgramModuleInstancesKey = {modInstKey mi};''

--- Destroy an existing Categorizing relation between a ModData entity
--- and a Category entity without ensuring the minimal constraint.
--- This can be used instead of `deleteCategorizing` if the corresponding
--- module is also deleted.
destroyCategorizing :: ModDataID -> CategoryID -> DBAction ()
destroyCategorizing mdkey catkey = deleteCategorizing mdkey catkey

--- Gets all modules contained in a given category.
getModDataOfCategory :: Category -> DBAction [ModData]
getModDataOfCategory c =
  ``sql* Select * From ModData As md
         Where Exists
          ( Select * From Category As cat
            Where cat.Key = {categoryKey c} And Satisfies md belongsTo cat);''

--- Gets the associated Category entities for a given ModData key
getModDataIDCategories :: ModDataID -> DBAction [Category]
getModDataIDCategories mdid =
  ``sql* Select * From Category As cat
         Where Exists
          ( Select * From ModData As md
            Where md.Key = {mdid} And Satisfies md belongsTo cat);''

--- Gets the associated Category entities for a given ModData entity
getModDataCategories :: ModData -> DBAction [Category]
getModDataCategories md = getModDataKeyCategories (modDataKey md)

--- Gets the associated Category entities for a given ModDataKey
getModDataKeyCategories :: ModDataID -> DBAction [Category]
getModDataKeyCategories mdk =
  ``sql* Select * From Category As cat
         Where Exists
          ( Select * From ModData As md
            Where md.Key = {mdk} And Satisfies md belongsTo cat);''

--- query whether a module has a UnivIS instance in a semester:
queryHasUnivisEntry :: String -> (String,Int) -> DBAction Bool
queryHasUnivisEntry mcode (term,year) = liftM (not . null) $
  ``sql* Select * From UnivisInfo As uv
         Where uv.Code = {mcode} And uv.Term = {term} And uv.Year = {year};''

--- query the univis URLs for a module in a semester:
queryUnivisURL :: String -> (String,Int) -> DBAction [String]
queryUnivisURL mcode (term,year) =
  ``sql* Select uv.URL From UnivisInfo As uv
         Where uv.Code = {mcode} And uv.Term = {term} And uv.Year = {year};''

-----------------------------------------------------------------------
--- Gets the pair of term and year of a ModInst entity.
modInstSemester :: ModInst -> (String,Int)
modInstSemester (ModInst _ t y _ _) = (t,y)

-----------------------------------------------------------------------
--- Gets the associated User entity for a given ModData entity.
getResponsibleUser :: ModData -> DBAction User
getResponsibleUser mUser = getUser (modDataUserResponsibleKey mUser)

--- Gets the associated User entity for a given MasterProgram entity.
getAdvisingUser :: MasterProgram -> DBAction User
getAdvisingUser mprog = getUser (masterProgramUserAdvisingKey mprog)

--- Gets the associated User entity for a given AdvisorStudyProgram entity.
getStudyProgAdvisorUser :: AdvisorStudyProgram -> DBAction User
getStudyProgAdvisorUser = getUser . advisorStudyProgramUserStudyAdvisingKey

--- Query the categories of a given StudyProgram.
queryCategorysOfStudyProgram :: StudyProgramID -> DBAction [Category]
queryCategorysOfStudyProgram sp =
  queryCondCategory (\c -> categoryStudyProgramProgramCategoriesKey c == sp)


-----------------------------------------------------------------------
--- Get module instances of next n semesters from a given one:
queryModInstInSemesters :: (String,Int) -> Int -> DBAction [ModInst]
queryModInstInSemesters semyear n =
  let nextsems = take n (iterate nextSemester semyear)
   in queryCondModInst (\mi -> (modInstTerm mi,modInstYear mi) `elem` nextsems)

--- Get module instances and their module data of next n semesters
--  from a given one that belong to a given category:
getCatModInstsInSemesters :: Category -> (String,Int) -> Int
                          -> DBAction [(ModInst,ModData)]
getCatModInstsInSemesters cat semyear n = do
  mis <- queryModInstInSemesters semyear n
  miscats <-
    mapM (\mi -> do
           cats <- getModDataKeyCategories (modInstModDataModuleInstancesKey mi)
           return (mi,cats))
         mis
  mapM (\ mi -> do md <- getModData (modInstModDataModuleInstancesKey mi)
                   return (mi,md))
       (map fst (filter (\ (_,cats) -> elem cat cats) miscats))

--- Get module data for a list of AdvisorModules:
getAdvisorModuleData :: [AdvisorModule]
                     -> DBAction [(AdvisorModule,ModInst,ModData)]
getAdvisorModuleData = mapM $ \amod -> do
    mi <- getModInst (advisorModuleModInstAdvisedProgramModuleInstancesKey amod)
    moddata <- getModData (modInstModDataModuleInstancesKey mi)
    return (amod,mi,moddata)

-----------------------------------------------------------------------
