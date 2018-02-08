{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=foreigncode #-}

--- Some extensions to the generated MDB module.

module SpecialQueries where

import List ( nub )

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
  Graph "Module prerequisites"
        (map (\s -> Node s []) (nub (map fst cpmdeps ++ map snd cpmdeps)))
        (map (\ (s,t) -> Edge s t []) cpmdeps)

-----------------------------------------------------------------------
