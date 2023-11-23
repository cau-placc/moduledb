{-# OPTIONS_FRONTEND -F --pgmF=currypp --optF=foreigncode --optF=-o #-}

--- A collection of SQL queries for the module database.

module Model.MDB.Queries where

import Data.List  ( intersect, nub )
import Data.Maybe ( listToMaybe )

import Database.CDBI.ER
import ShowDotGraph

import Model.MDB
import System.Helpers ( moduleCodeURL, nextSemester )

------------------------------------------------------------------------------
--- Gets the term/year pair of a ModInst entity.
modInstSemester :: ModInst -> (String,Int)
modInstSemester (ModInst _ t y _ _) = (t,y)

------------------------------------------------------------------------------
--- Transforms an SQL list result into a maybe result (of first element).
sqlToMaybe :: DBAction [a] -> DBAction (Maybe a)
sqlToMaybe = fmap (\xs -> if null xs then Nothing else Just (head xs))

-----------------------------------------------------------------------
--- Gets a user entity with a given login name.
queryUserWithLogin :: String -> DBAction (Maybe User)
queryUserWithLogin login = fmap listToMaybe
  ``sql* Select * From User As u
         Where u.Login = {login};''

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

--- Gets all ModData entities taught by some user.
queryModDataOfLecturer :: UserID -> DBAction [ModData]
queryModDataOfLecturer ukey = do
  mdkeys <- ``sql* Select Distinct md.Key
                   From ModData As md, ModInst as mi, User as u
                   Where u.Key = {ukey} And
                         Satisfies mi withLecturer u And
                         Satisfies mi withModule md;''
  mapM getModData mdkeys

--- Gets all ModData entities having a given pattern in its code or name.
queryModDataSearch :: String -> DBAction [ModData]
queryModDataSearch pattern =
  ``sql* Select *
         From ModData as md
         Where md.Code like {pattern} Or md.NameG like {pattern}
                                      Or md.NameE like {pattern};''

--- Gets all module instances for a given module (key) taught by the
--- given lecturer (key).
queryLecturedInstancesOfMod :: ModDataID -> UserID -> DBAction [ModInst]
queryLecturedInstancesOfMod mdk uid =
  ``sql* Select * From ModInst As mi
         Where mi.ModDataModuleInstancesKey = {mdk} And
               mi.UserLecturerModsKey = {uid};''

------------------------------------------------------------------------------
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
 fmap
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
readProgModules s = read s


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
queryHasUnivisEntry mcode (term,year) = fmap (not . null) $
  ``sql* Select * From UnivisInfo As uv
         Where uv.Code = {mcode} And uv.Term = {term} And uv.Year = {year};''

--- query the univis URLs for a module in a semester:
queryUnivisURL :: String -> (String,Int) -> DBAction [String]
queryUnivisURL mcode (term,year) =
  ``sql* Select uv.URL From UnivisInfo As uv
         Where uv.Code = {mcode} And uv.Term = {term} And uv.Year = {year};''

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
-- Gets the module codes together with the text of all non-empty
-- prequesite descriptions.
printRequirementDescriptions :: IO ()
printRequirementDescriptions = do
  crs <- runQ ``sql* Select md.Code, mdesc.Prereq
                     From ModData as md, ModDescr as mdesc
                     Where Satisfies md withDesc mdesc;''
  putStr $ concatMap (\ (c,r) -> c ++ ":\n" ++ r ++ "\n\n")
                     (filter (\ (_,r) -> not (null r)) crs)

-----------------------------------------------------------------------
--- Gets the module keys and codes of all modules in a given study program.
getModuleCodesOfStudyProg :: StudyProgram -> IO [(ModDataID,String)]
getModuleCodesOfStudyProg sprog = runQ
  ``sql* Select Distinct md.Key, md.Code
         From ModData as md, Category as cat, StudyProgram as sp
         Where sp.Key = { studyProgramKey sprog } And
               Satisfies sp withCategory cat And
               Satisfies md belongsTo cat;''

--- Gets the module codes of all prequesites in a given study program,
--- i.e., compute (m1,m2) if m1 requires m2.
--- (A bit more complicated since the Satisfies clause seems buggy.)
getStudyProgRequirements :: StudyProgram -> IO [(String,String)]
getStudyProgRequirements sprog = runQ
  ``sql* Select Distinct md1.Code, md2.Code
         From Prerequisites as preq, ModData as md1, ModData as md2,
              Category as cat, StudyProgram as sp
         Where sp.Key = { studyProgramKey sprog } And
               Satisfies sp withCategory cat And
               Satisfies md1 belongsTo cat And
               preq.ModDataPrerequisitesKey1 = md1.Key And
               preq.ModDataPrerequisitesKey = md2.Key;''
               
-----------------------------------------------------------------------
-- Gets the module codes of all prequesites, i.e., compute (m1,m2)
-- if m1 requires m2.
-- (A bit more complicated since the Satisfies clause seems buggy.)
getAllRequirements :: IO [(String,String)]
getAllRequirements = runQ
  ``sql* Select md1.Code, md2.Code
         From Prerequisites as preq, ModData as md1, ModData as md2
         Where preq.ModDataPrerequisitesKey1 = md1.Key And
               preq.ModDataPrerequisitesKey = md2.Key;''

-- Show package prequesites as a dot graph.
showAllModulePrerequisites :: IO ()
showAllModulePrerequisites = getAllRequirements >>= showPrerequisites

-- Show module prequesites as a dot graph.
showPrerequisites :: [(String,String)] -> IO ()
showPrerequisites prereqs = do
  let dotgraph = depsToGraph prereqs
  putStrLn $ "Show dot graph..."
  viewDotGraph dotgraph

--- Transform a list of dependencies into a dot graph.
depsToGraph :: [(String,String)] -> DotGraph
depsToGraph cpmdeps =
  dgraph "Module prerequisites"
         (map (\s -> Node s [("URL", moduleCodeURL s)])
              (nub (map fst cpmdeps ++ map snd cpmdeps)))
         (map (\ (s,t) -> Edge s t []) cpmdeps)

-----------------------------------------------------------------------
--- Queries all students with a given email.
queryStudentByEmail :: String -> DBAction [Student]
queryStudentByEmail email = 
  ``sql* Select * From Student as s Where s.Email = {email};''

-----------------------------------------------------------------------
--- Gets the ModData keys of all module instances in a given semester.
queryModKeysOfSem :: (String,Int) -> DBAction [ModDataID]
queryModKeysOfSem (term,year) =
  ``sql* Select Distinct mi.ModDataModuleInstancesKey
         From   ModInst As mi
         Where  mi.Term = {term} And mi.Year = {year};''

--- Queries all module instances of a given semester.
queryModInstsOfSemester :: (String,Int)
                        -> DBAction [(ModInstID,ModDataID,String,String,String)]
queryModInstsOfSemester (term,year) =
  ``sql* Select mi.Key, md.Key, md.Code, md.NameG, md.NameE
         From ModInst as mi, ModData as md
         Where mi.Term = {term} And mi.Year = {year} And
               Satisfies mi withModule md;''

--- Queries all module instances taken by a student (identified by email)
--- in a given semester.
queryModInstsOfStudentInSem :: String -> (String,Int) -> DBAction [ModInstID]
queryModInstsOfStudentInSem email (term,year) =
  ``sql* Select mi.Key
         From ModInst as mi, Student as s, StudentCourse as sc
         Where s.Email = {email} And mi.Term = {term} And mi.Year = {year} And
               Satisfies sc withStudent s And
               Satisfies sc withModInst mi;''

--- Queries all module instances taken by a student (email).
queryStudentCoursesOfStudent :: String -> DBAction [StudentCourseID]
queryStudentCoursesOfStudent email =
  ``sql* Select sc.Key
         From Student as s, StudentCourse as sc
         Where s.Email = {email} And
               Satisfies sc withStudent s;''

--- Queries information about all courses selected by a student (email).
queryCoursesOfStudent :: String
                      -> DBAction [(ModDataID,String,String,String,String,Int)]
queryCoursesOfStudent email =
  ``sql* Select md.Key, md.Code, md.NameG, md.NameE, mi.Term, mi.Year
         From ModInst as mi, Student as s, StudentCourse as sc, ModData as md
         Where s.Email = {email} And
               Satisfies sc withStudent s And
               Satisfies sc withModInst mi And
               Satisfies mi withModule md;''

--- Queries all students registered for a module in a given given semester.
queryStudentsOfModSemester :: ModData -> (String,Int)
                           -> DBAction [(String,String,String)]
queryStudentsOfModSemester md (term,year) =
  ``sql* Select s.Email, s.Name, s.First
         From ModData as md, ModInst as mi, Student as s, StudentCourse as sc
         Where md.Code = {modDataCode md} And
               mi.Term = {term} And mi.Year = {year} And
               Satisfies sc withStudent s And
               Satisfies sc withModInst mi And
               Satisfies mi withModule md;''

--- Queries the number of all students registered for a module
--- in a given given semester.
queryStudentNumberOfModSemester :: ModData -> (String,Int)
                                -> DBAction Int
queryStudentNumberOfModSemester md (term,year) =
  fmap (\xs -> if null xs then 0 else head xs)
  ``sql* Select Count(s.Email)
         From ModData as md, ModInst as mi, Student as s, StudentCourse as sc
         Where md.Code = {modDataCode md} And
               mi.Term = {term} And mi.Year = {year} And
               Satisfies sc withStudent s And
               Satisfies sc withModInst mi And
               Satisfies mi withModule md;''

-----------------------------------------------------------------------
-- Gets a list of all modules (code/title) for a given semester.
moduleInstancesInSemester :: (String,Int) -> IO [(String,String)]
moduleInstancesInSemester (term,year) = runJustT
  ``sql* Select md.Code, md.NameG
         From ModInst as mi, ModData as md
         Where mi.Term = {term} And mi.Year = {year} And
               Satisfies mi withModule md;''

-- Gets a list of all modules (code/title) and the registered students
-- for a given semester.
moduleInstanceNumber :: (String,Int) -> IO [(String,String)]
moduleInstanceNumber (term,year) = runJustT
  ``sql* Select md.Code, s.Email
         From ModInst as mi, Student as s, StudentCourse as sc, ModData as md
         Where mi.Term = {term} And mi.Year = {year} And
               Satisfies sc withModInst mi And
               Satisfies mi withModule md And
               Satisfies sc withStudent s;''

-- Gets a list of all pairs of modules and the number of students
-- who want to attend both modules:
getModuleConflictList :: (String,Int) -> IO [(String,String,Int)]
getModuleConflictList sem = do
  mods    <- moduleInstancesInSemester sem >>= return . map fst
  modnums <- moduleInstanceNumber sem
  let m = [ (m1, m2, length (attendBoth m1 m2 modnums))
          | m1 <- mods, m2 <- mods, m1 < m2 ]
  return $ filter (\ (_,_,n) -> n>0) m
 where
  attendBoth m1 m2 xs =
    intersect (map snd (filter ((==m1) . fst) xs))
              (map snd (filter ((==m2) . fst) xs))

-----------------------------------------------------------------------
-- More queries:

-- Prints a list of all category short names and their study program
-- short names.
allCategoriesPrograms :: IO ()
allCategoriesPrograms = runJustT
  ``sql* Select cat.ShortName, sp.ShortName
         From Category As cat, StudyProgram As sp
         Where cat.StudyProgramProgramCategoriesKey = sp.Key;'' >>=
  putStrLn . unlines . map (\ (c,p) -> c ++ " | " ++ p )

-----------------------------------------------------------------------
