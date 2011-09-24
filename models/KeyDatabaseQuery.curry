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
        (DBTable(..),DBAttr(..),readKeyInfo,QueryCond(..),(@==),(@&&),whereQ,
         selectAll,selectAll1,selectAll2,selectAll3,selectAll6,
         select,select1,select2)
 where

import KeyDatabase
import ReadShowTerm

infix  4 @==
infixr 3 @&&
infix  1 `whereQ`

readTuple1 :: (String -> [(a,String)]) -> String -> a
readTuple1 r1 s = case s of
  '(':s1 -> case r1 s1 of
              [(x1,")")] -> x1
              _ -> readError
  _ -> readError
 where readError = error ("Illegal tuple string: "++s)

readTuple2 :: (String -> [(a,String)]) -> (String -> [(b,String)])
           -> String -> (a,b)
readTuple2 r1 r2 s = case s of
  '(':s1 -> case r1 s1 of
              [(x1,',':s2)] -> case r2 s2 of
                                 [(x2,")")] -> (x1,x2)
                                 _ -> readError
              _ -> readError
  _ -> readError
 where readError = error ("Illegal tuple string: "++s)

readTuple3 :: (String -> [(a,String)])
           -> (String -> [(b,String)])
           -> (String -> [(c,String)])
           -> String -> (a,b,c)
readTuple3 r1 r2 r3 s = case s of
  '(':s1 -> case r1 s1 of
       [(x1,',':s2)] -> case r2 s2 of
            [(x2,',':s3)] -> case r3 s3 of
                [(x3,")")] -> (x1,x2,x3)
                _ -> readError
            _ -> readError
       _ -> readError
  _ -> readError
 where readError = error ("Illegal tuple string: "++s)

readTuple4 :: (String -> [(a,String)])
           -> (String -> [(b,String)])
           -> (String -> [(c,String)])
           -> (String -> [(d,String)])
           -> String -> (a,b,c,d)
readTuple4 r1 r2 r3 r4 s = case s of
  '(':s1 -> case r1 s1 of
       [(x1,',':s2)] -> case r2 s2 of
            [(x2,',':s3)] -> case r3 s3 of
                [(x3,',':s4)] -> case r4 s4 of
                    [(x4,")")] -> (x1,x2,x3,x4)
                    _ -> readError
                _ -> readError
            _ -> readError
       _ -> readError
  _ -> readError
 where readError = error ("Illegal tuple string: "++s)

readTuple5 :: (String -> [(a,String)])
           -> (String -> [(b,String)])
           -> (String -> [(c,String)])
           -> (String -> [(d,String)])
           -> (String -> [(e,String)])
           -> String -> (a,b,c,d,e)
readTuple5 r1 r2 r3 r4 r5 s = case s of
  '(':s1 -> case r1 s1 of
       [(x1,',':s2)] -> case r2 s2 of
            [(x2,',':s3)] -> case r3 s3 of
                [(x3,',':s4)] -> case r4 s4 of
                    [(x4,',':s5)] -> case r5 s5 of
                        [(x5,")")] -> (x1,x2,x3,x4,x5)
                        _ -> readError
                    _ -> readError
                _ -> readError
            _ -> readError
       _ -> readError
  _ -> readError
 where readError = error ("Illegal tuple string: "++s)

readTuple6 :: (String -> [(a,String)])
           -> (String -> [(b,String)])
           -> (String -> [(c,String)])
           -> (String -> [(d,String)])
           -> (String -> [(e,String)])
           -> (String -> [(f,String)])
           -> String -> (a,b,c,d,e,f)
readTuple6 r1 r2 r3 r4 r5 r6 s = case s of
  '(':s1 -> case r1 s1 of
       [(x1,',':s2)] -> case r2 s2 of
            [(x2,',':s3)] -> case r3 s3 of
                [(x3,',':s4)] -> case r4 s4 of
                    [(x4,',':s5)] -> case r5 s5 of
                        [(x5,',':s6)] -> case r6 s6 of
                            [(x6,")")] -> (x1,x2,x3,x4,x5,x6)
                            _ -> readError
                        _ -> readError
                    _ -> readError
                _ -> readError
            _ -> readError
       _ -> readError
  _ -> readError
 where readError = error ("Illegal tuple string: "++s)

readKeyInfo :: (Key -> a) -> String -> [(a,String)]
readKeyInfo keyfun s = map (\ (v,r) -> (keyfun v,r)) (readKey s)

readKey :: String -> [(Int,String)]
readKey s = readsQTerm s

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
  DBAttr (DBTable etype)
          Int (atype -> QueryCond etype) (String -> [(atype,String)])

--- Equality condition between a database attribute and a value.
(@==) :: DBAttr etype atype -> atype -> QueryCond etype
(DBAttr _ _ eqfun _) @== val  =  eqfun val

-- Read results of sqlite query as tuple.
readInfo :: String -> a
readInfo str = readQTerm $ "(" ++ str ++ ")"

-- Read results of sqlite query as tuple.
readTInfo :: (String->a) -> String -> a
readTInfo readtuple str = readtuple $ "(" ++ str ++ ")"

--- Query an entity with a condition on this entity.
selectAll :: DBTable e -> Query [e]
selectAll (DBTable condquery _) = condquery []

-- Projection on an attribute.
selectAll1 :: DBAttr e atype -> Query [atype]
selectAll1 (DBAttr (DBTable _ condqueryproject) i _ f) =
  transformQ (map (readTInfo (readTuple1 f)))
             (condqueryproject [i] [])

-- Projection on two attributes.
selectAll2 :: DBAttr e atype1 -> DBAttr e atype2
           -> Query [(atype1,atype2)]
selectAll2 (DBAttr (DBTable _ condqueryproject) i1 _ f1) (DBAttr _ i2 _ f2) =
  transformQ (map (readTInfo (readTuple2 f1 f2)))
             (condqueryproject [i1,i2] [])


-- Projection on three attributes.
selectAll3 :: DBAttr e atype1 -> DBAttr e atype2
           -> DBAttr e atype3
           -> Query [(atype1,atype2,atype3)]
selectAll3 (DBAttr (DBTable _ condqueryproject) i1 _ f1) (DBAttr _ i2 _ f2)
           (DBAttr _ i3 _ f3) =
  transformQ (map (readTInfo (readTuple3 f1 f2 f3)))
             (condqueryproject [i1,i2,i3] [])

-- Projection on six attributes.
selectAll6 :: DBAttr e atype1 -> DBAttr e atype2
           -> DBAttr e atype3 -> DBAttr e atype4
           -> DBAttr e atype5 -> DBAttr e atype6
           -> Query [(atype1,atype2,atype3,atype4,atype5,atype6)]
selectAll6 (DBAttr (DBTable _ condqueryproject) i1 _ f1) (DBAttr _ i2 _ f2)
           (DBAttr _ i3 _ f3) (DBAttr _ i4 _ f4)
           (DBAttr _ i5 _ f5) (DBAttr _ i6 _ f6) =
  transformQ (map (readTInfo (readTuple6 f1 f2 f3 f4 f5 f6)))
             (condqueryproject [i1,i2,i3,i4,i5,i6] [])

--- Query an entity with a condition on this entity.
select :: DBTable e -> QueryCond e -> Query [e]
select (DBTable condquery _) qcond = condquery (queryCond2ColVals qcond)

--- Query an entity with a condition on this entity.
--- This is basically the apply operator but used to write queries
--- more nicely.
whereQ :: (QueryCond e -> Query [a]) -> QueryCond e -> Query [a]
condquery `whereQ` qcond = condquery qcond

-- Projection on an attributes with condition.
select1 :: DBAttr e atype -> QueryCond e -> Query [atype]
select1 (DBAttr (DBTable _ condqueryproject) i _ f) qcond =
  transformQ (map (readTInfo (readTuple1 f)))
             (condqueryproject [i] (queryCond2ColVals qcond))

-- Projection on two attributes with condition.
select2 :: DBAttr e atype1 -> DBAttr e atype2 -> QueryCond e
        -> Query [(atype1,atype2)]
select2 (DBAttr (DBTable _ condqueryproject) i1 _ f1) (DBAttr _ i2 _ f2)
             qcond =
  transformQ (map (readTInfo (readTuple2 f1 f2)))
             (condqueryproject [i1,i2] (queryCond2ColVals qcond))

------------------------------------------------------------------------
