------------------------------------------------------------------------------
--- This module supports simple queries based on the KeyDatabase module.
--- It is intended to be used with the erd2curry-generated database
--- operations, where typed table and attribute definitions are
--- used in the queries provided by this module.
---
--- @author Michael Hanus
--- @version September 2011
------------------------------------------------------------------------------

module KeyDatabaseQuery
        (DBTable(..),DBAttr(..),QueryCond(..),(@==),(@&&),dbWhere,
         select1,select2,select6,select7,
         select1where,select2where)
 where

import KeyDatabase
import ReadShowTerm

infix  4 @==
infixr 3 @&&
infix  1 `dbWhere`

------------------------------------------------------------------------
-- Auxiliaries for better querying

-- Abstract type for conditions used in database queries.
-- The type variable is used to distinguish conditions for different entities.
data QueryCond a = AttrEq ColVal -- attribute equality
                 | QueryCondConj (QueryCond a) (QueryCond a) -- conjunction

-- Translation of query conditions to KeyDatabase conditions
queryCond2ColVals :: QueryCond _ -> [ColVal]
queryCond2ColVals (AttrEq cv) = [cv]
queryCond2ColVals (QueryCondConj c1 c2) =
  queryCond2ColVals c1 ++ queryCond2ColVals c2

--- Conjunction of query conditions.
(@&&) :: QueryCond a -> QueryCond a -> QueryCond a
(@&&) = QueryCondConj

--- Abstract type to represent a table corresponding to an entity.
--- The table contains an operation implementing a conditional query
--- for this entity.
data DBTable e = DBTable ([ColVal] -> Query [e])
                         ([Int] -> [ColVal] -> Query [String])

--- Abstract type to represent an attribute of an entity.
--- Currently, it contains the column number,
--- the equality condition for this attribute, and
--- a possible transformation from database keys into attribute types
--- (used to transform foreign keys in attributes).
data DBAttr etype atype =
  DBAttr Int (atype -> QueryCond etype) (Maybe (KeyDatabase.Key -> atype))

--- Equality condition between a database attribute and a value.
(@==) :: DBAttr etype atype -> atype -> QueryCond etype
(DBAttr _ eqfun _) @== val  =  eqfun val

--- Query an entity with a condition on this entity.
dbWhere :: DBTable e -> QueryCond e -> Query [e]
dbWhere (DBTable condquery _) qcond = condquery (queryCond2ColVals qcond)

-- Read results of sqlite query as tuple.
readInfo :: String -> a
readInfo str = readQTerm $ "(" ++ str ++ ")"

-- Projection on an attribute.
select1 :: DBAttr e atype -> DBTable e -> Query [atype]
select1 (DBAttr i _ t) (DBTable _ condqueryproject) =
  transformQ (map (\s -> trans (readInfo s)))
             (condqueryproject [i] [])
 where
  trans x = maybe x (\f -> f (readQTerm (showQTerm x))) t

-- Projection on two attributes.
select2 :: DBAttr e atype1 -> DBAttr e atype2 -> DBTable e
        -> Query [(atype1,atype2)]
select2 (DBAttr i1 _ t1) (DBAttr i2 _ t2) (DBTable _ condqueryproject) =
  transformQ (map (\s -> trans (readInfo s)))
             (condqueryproject [i1,i2] [])
 where
   trans (x1,x2) = (maybe x1 (\f -> f (readQTerm (showQTerm x1))) t1,
                    maybe x2 (\f -> f (readQTerm (showQTerm x2))) t2)


-- Projection on six attributes.
select6 :: DBAttr e atype1 -> DBAttr e atype2
        -> DBAttr e atype3 -> DBAttr e atype4
        -> DBAttr e atype5 -> DBAttr e atype6
        -> DBTable e
        -> Query [(atype1,atype2,atype3,atype4,atype5,atype6)]
select6 (DBAttr i1 _ t1) (DBAttr i2 _ t2)
        (DBAttr i3 _ t3) (DBAttr i4 _ t4)
        (DBAttr i5 _ t5) (DBAttr i6 _ t6)
        (DBTable _ condqueryproject) =
  transformQ (map (\s -> trans (readInfo s)))
             (condqueryproject [i1,i2,i3,i4,i5,i6] [])
 where
   trans (x1,x2,x3,x4,x5,x6) =
     (maybe x1 (\f -> f (readQTerm (showQTerm x1))) t1,
      maybe x2 (\f -> f (readQTerm (showQTerm x2))) t2,
      maybe x3 (\f -> f (readQTerm (showQTerm x3))) t3,
      maybe x4 (\f -> f (readQTerm (showQTerm x4))) t4,
      maybe x5 (\f -> f (readQTerm (showQTerm x5))) t5,
      maybe x6 (\f -> f (readQTerm (showQTerm x6))) t6)

-- Projection on seven attributes.
select7 :: DBAttr e atype1 -> DBAttr e atype2
        -> DBAttr e atype3 -> DBAttr e atype4
        -> DBAttr e atype5 -> DBAttr e atype6
        -> DBAttr e atype7
        -> DBTable e
        -> Query [(atype1,atype2,atype3,atype4,atype5,atype6,atype7)]
select7 (DBAttr i1 _ t1) (DBAttr i2 _ t2)
        (DBAttr i3 _ t3) (DBAttr i4 _ t4)
        (DBAttr i5 _ t5) (DBAttr i6 _ t6)
        (DBAttr i7 _ t7)
        (DBTable _ condqueryproject) =
  transformQ (map (\s -> trans (readInfo s)))
             (condqueryproject [i1,i2,i3,i4,i5,i6,i7] [])
 where
   trans (x1,x2,x3,x4,x5,x6,x7) =
     (maybe x1 (\f -> f (readQTerm (showQTerm x1))) t1,
      maybe x2 (\f -> f (readQTerm (showQTerm x2))) t2,
      maybe x3 (\f -> f (readQTerm (showQTerm x3))) t3,
      maybe x4 (\f -> f (readQTerm (showQTerm x4))) t4,
      maybe x5 (\f -> f (readQTerm (showQTerm x5))) t5,
      maybe x6 (\f -> f (readQTerm (showQTerm x6))) t6,
      maybe x7 (\f -> f (readQTerm (showQTerm x7))) t7)

-- Projection on an attributes with condition.
select1where :: DBAttr e atype -> DBTable e -> QueryCond e
             -> Query [atype]
select1where (DBAttr i _ t) (DBTable _ condqueryproject) qcond =
  transformQ (map (trans . readInfo))
             (condqueryproject [i] (queryCond2ColVals qcond))
 where
  trans x = maybe x (\f -> f (readQTerm (showQTerm x))) t

-- Projection on two attributes with condition.
select2where :: DBAttr e atype1 -> DBAttr e atype2 -> DBTable e -> QueryCond e
             -> Query [(atype1,atype2)]
select2where (DBAttr i1 _ t1) (DBAttr i2 _ t2) (DBTable _ condqueryproject)
             qcond =
  transformQ (map (\s -> trans (readInfo s)))
             (condqueryproject [i1,i2] (queryCond2ColVals qcond))
 where
   trans (x1,x2) = (maybe x1 (\f -> f (readQTerm (showQTerm x1))) t1,
                    maybe x2 (\f -> f (readQTerm (showQTerm x2))) t2)

{-
ex1 = runQ (select1 modData'Code modDataTable)
        >>= putStrLn . unlines . map show

ex2 = runQ (select2 modData'Code modData'NameG modDataTable)
        >>= putStrLn . unlines . map show

ex3 = runQ (select2 modData'Code modData'UserResponsibleKey modDataTable)
        >>= putStrLn . unlines . map show

ex4 = runQ (select2 modData'Key modData'Code modDataTable)
        >>= putStrLn . unlines . map show

ex5 = runQ (select1where modData'NameG modDataTable (modData'Code @=="Inf-Prog"))

ex6 = runQ (select2where modData'NameG modData'UserResponsibleKey modDataTable
                         (modData'Code @=="Inf-Prog"))

ex7 = runQ queryAllMasterPrograms
ex8 = runQ (select1 masterProgram'UserAdvisingKey masterProgramTable)
ex9 = runQ (select7 masterProgram'Key masterProgram'Name masterProgram'Term
                    masterProgram'Year masterProgram'Visible
                    masterProgram'UserAdvisingKey
                    masterProgram'MasterCoreAreaAreaProgramsKey
              masterProgramTable)
-}

------------------------------------------------------------------------
