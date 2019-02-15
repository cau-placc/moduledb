{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=foreigncode #-}

--- Some extensions to the generated MDB module.

module SpecialQueries where

import List ( intersect, nub )

import Database.CDBI.ER
import ShowDotGraph

import MDB

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
         (map (\s -> Node s []) (nub (map fst cpmdeps ++ map snd cpmdeps)))
         (map (\ (s,t) -> Edge s t []) cpmdeps)

-----------------------------------------------------------------------
--- Queries all students with a given email.
queryStudentByEmail :: String -> DBAction [Student]
queryStudentByEmail email = 
  ``sql* Select * From Student as s Where s.Email = {email};''

-----------------------------------------------------------------------
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
  liftM (\xs -> if null xs then 0 else head xs)
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
