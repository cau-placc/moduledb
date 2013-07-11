------------------------------------------------------------------------------
--- This module supports simple queries based on the KeyDatabase module.
--- It is intended to be used with the erd2curry-generated database
--- operations, where typed table and attribute definitions are
--- used in the queries provided by this module.
---
--- A database table for entities of type `etype` is defined as follows:
--- 
---     dbTable entrypred tuple2entry
--- 
--- where `entrypred` is the dynamic predicate for entities of type `etype`
--- and `tuple2entry` is the transformation from the tuple representation
--- of entities to the type `etype`.
--- 
--- An attribute of some database table `dbtable` is defined as follows:
--- 
---     dbAttr dbtable colno
--- 
--- where `colno` is the the column number of this attribute in the table.
--- If the attribute is a database key, one has to use the operation
--- 'dbKeyAttr' instead (providing some key transformations).
--- 
--- Now one can use various combinators to construct queries:
--- 
---  * Select all entries from a table `t`:
--- 
---        selectAllFrom t
--- 
---  * Select attributes `a1`, `a2`, `a3`:
--- 
---        selectAll3 a1 a2 a3
--- 
---  * Select all entries from a table `t` satisfying a condition on their
---    attributes (`a1` must be "John" and `a3` has the value 40):
--- 
---        selectFrom t `whereQ` a1 @== "John" @&& a3 @== 40
--- 
---  * Select attributes `a1`, `a2`, `a3` of all entries satisfying
---    a condition on their attributes (`a1` must be "John"):
--- 
---        select3 a1 a2 a3 `whereQ` a1 @== "John" @&& a3 @== 40
--- 
--- @author Michael Hanus
--- @version May 2012
------------------------------------------------------------------------------

module KeyDatabaseQuery
        (DBTable,dbTable,DBAttr,dbAttr,dbKeyAttr,
         (@==),(@&&),whereQ,
         selectAllFrom,selectAll1,selectAll2,selectAll3,selectAll4,
         selectAll5,selectAll6,
         selectFrom,select1,select2,select3,select4,select5,select6)
 where

import KeyDatabase
import ReadShowTerm
import List(findIndex)

infix  4 @==
infixr 3 @&&
infix  1 `whereQ`

------------------------------------------------------------------------
-- Basic definitions for database tables, attributes and queries.

--- Abstract type to represent a table corresponding to an entity.
--- It contains the following information:
--- * a list of column names
--- * an operation implementing a conditional query for this entity,
---   (which maps a list of value restrictions into a query)
--- * an operation implementing a conditional query with attribute projection
---   for this entity (i.e., the operation takes a list of projected
---   column numbers and a list of value restrictions and returns a list
---   of strings where each string is the string representation of a
---   selected/projected row)
data DBTable e = DBTable ([ColVal] -> Query [e])
                         ([Int] -> [ColVal] -> Query [String])

--- Auxiliary operation to define a database table 'DBTable'.
dbTable :: (Key -> et -> Dynamic) -> (Key -> et -> etype) -> DBTable etype
dbTable dbentry tuple2entry =
  DBTable (\colvals -> transformQ (map (uncurry tuple2entry))
                                  (someDBKeyInfos dbentry colvals))
          (someDBKeyProjectionRaw dbentry)


--- Abstract type to represent an attribute of an entity.
--- It contains the following information:
--- * the database table containing this attribute
--- * the column number of this attribute in the database table
--- * the equality condition for this attribute (an operation which maps
---   a value into a "where" condition so that the attribute must be equal
---   to the value)
--- * a "reads" operation to transform the string representation of this
---   attribute into its value
data DBAttr etype atype = DBAttr (DBTable etype)
                                 Int
                                 (atype -> WhereCond etype)
                                 (String -> [(atype,String)])

--- Auxiliary operation to define a database attribute 'DBAttr'
--- for a given database table and column number of this attribute.
dbAttr :: DBTable etype -> Int -> DBAttr etype atype
dbAttr dbtable colno = DBAttr dbtable colno (AttrEq . (colno@=)) readsQTerm

--- Auxiliary operation to define a database attribute 'DBAttr'
--- containing a database key. The parameters are
--- the database table, column number of this attribute, and
--- transformation operations for the database key.
dbKeyAttr :: DBTable etype -> Int -> (ekey -> Key) -> (Key -> ekey)
          -> DBAttr etype ekey
dbKeyAttr dbtable colno ekey2key key2ekey =
  DBAttr dbtable colno (AttrEq . (colno@=) . ekey2key) (readKeyInfo key2ekey)


-- Abstract type for conditions used in database queries.
-- The type variable is used to distinguish conditions for different entities.
data WhereCond a = AttrEq ColVal -- attribute equality
                 | WhereCondConj (WhereCond a) (WhereCond a) -- conjunction
                 | WhereCondTrue -- always satisfied query

--- Equality condition between a database attribute and a value.
(@==) :: DBAttr etype atype -> atype -> WhereCond etype
(DBAttr _ _ eqfun _) @== val  =  eqfun val

--- Conjunction of query conditions.
(@&&) :: WhereCond a -> WhereCond a -> WhereCond a
(@&&) = WhereCondConj

--- Query an entity with a condition on this entity.
--- This is basically the apply operator but used to write queries
--- more nicely.
whereQ :: (WhereCond e -> Query [a]) -> WhereCond e -> Query [a]
condquery `whereQ` wcond = condquery wcond

-- Translation of query conditions to KeyDatabase conditions
whereCond2ColVals :: WhereCond _ -> [ColVal]
whereCond2ColVals (AttrEq cv) = [cv]
whereCond2ColVals (WhereCondConj c1 c2) =
  whereCond2ColVals c1 ++ whereCond2ColVals c2
whereCond2ColVals WhereCondTrue = []

------------------------------------------------------------------------
-- Queries without where clauses:

--- Query all entity instances from a database table.
selectAllFrom :: DBTable e -> Query [e]
selectAllFrom dbtable = selectFrom dbtable `whereQ` WhereCondTrue

-- Projection on an attribute without a condition.
selectAll1 :: DBAttr e atype -> Query [atype]
selectAll1 dbattr1 = select1 dbattr1 `whereQ` WhereCondTrue

-- Projection on two attributes without a condition.
selectAll2 :: DBAttr e atype1 -> DBAttr e atype2
           -> Query [(atype1,atype2)]
selectAll2 dbattr1 dbattr2 = select2 dbattr1 dbattr2 `whereQ` WhereCondTrue


-- Projection on three attributes without a condition.
selectAll3 :: DBAttr e atype1 -> DBAttr e atype2
           -> DBAttr e atype3
           -> Query [(atype1,atype2,atype3)]
selectAll3 dbattr1 dbattr2 dbattr3 =
  select3 dbattr1 dbattr2 dbattr3 `whereQ` WhereCondTrue

-- Projection on four attributes without a condition.
selectAll4 :: DBAttr e atype1 -> DBAttr e atype2
           -> DBAttr e atype3 -> DBAttr e atype4
           -> Query [(atype1,atype2,atype3,atype4)]
selectAll4 dbattr1 dbattr2 dbattr3 dbattr4 =
  select4 dbattr1 dbattr2 dbattr3 dbattr4 `whereQ` WhereCondTrue

-- Projection on five attributes without a condition.
selectAll5 :: DBAttr e atype1 -> DBAttr e atype2
           -> DBAttr e atype3 -> DBAttr e atype4
           -> DBAttr e atype5
           -> Query [(atype1,atype2,atype3,atype4,atype5)]
selectAll5 dbattr1 dbattr2 dbattr3 dbattr4 dbattr5 =
  select5 dbattr1 dbattr2 dbattr3 dbattr4 dbattr5 `whereQ` WhereCondTrue

-- Projection on six attributes without a condition.
selectAll6 :: DBAttr e atype1 -> DBAttr e atype2
           -> DBAttr e atype3 -> DBAttr e atype4
           -> DBAttr e atype5 -> DBAttr e atype6
           -> Query [(atype1,atype2,atype3,atype4,atype5,atype6)]
selectAll6 dbattr1 dbattr2 dbattr3 dbattr4 dbattr5 dbattr6 =
  select6 dbattr1 dbattr2 dbattr3 dbattr4 dbattr5 dbattr6 `whereQ` WhereCondTrue

------------------------------------------------------------------------
-- Queries with where clauses:

--- Query all entity instances from a database table
--- satisfying a given condition.
selectFrom :: DBTable e -> WhereCond e -> Query [e]
selectFrom (DBTable condquery _) wcond = condquery (whereCond2ColVals wcond)

-- Projection on an attributes with condition.
select1 :: DBAttr e atype -> WhereCond e -> Query [atype]
select1 (DBAttr (DBTable _ condqueryproject) i _ f) wcond =
  transformQ (map (readTInfo (readTuple1 f)))
             (condqueryproject [i] (whereCond2ColVals wcond))

-- Projection on two attributes with condition.
select2 :: DBAttr e atype1 -> DBAttr e atype2 -> WhereCond e
        -> Query [(atype1,atype2)]
select2 (DBAttr (DBTable _ condqueryproject) i1 _ f1) (DBAttr _ i2 _ f2)
        wcond =
  transformQ (map (readTInfo (readTuple2 f1 f2)))
             (condqueryproject [i1,i2] (whereCond2ColVals wcond))

-- Projection on three attributes.
select3 :: DBAttr e atype1 -> DBAttr e atype2 -> DBAttr e atype3
        -> WhereCond e
        -> Query [(atype1,atype2,atype3)]
select3 (DBAttr (DBTable _ condqueryproject) i1 _ f1) (DBAttr _ i2 _ f2)
        (DBAttr _ i3 _ f3) wcond =
  transformQ (map (readTInfo (readTuple3 f1 f2 f3)))
             (condqueryproject [i1,i2,i3] (whereCond2ColVals wcond))

-- Projection on four attributes.
select4 :: DBAttr e atype1 -> DBAttr e atype2
        -> DBAttr e atype3 -> DBAttr e atype4
        -> WhereCond e
        -> Query [(atype1,atype2,atype3,atype4)]
select4 (DBAttr (DBTable _ condqueryproject) i1 _ f1) (DBAttr _ i2 _ f2)
        (DBAttr _ i3 _ f3) (DBAttr _ i4 _ f4) wcond =
  transformQ (map (readTInfo (readTuple4 f1 f2 f3 f4)))
             (condqueryproject [i1,i2,i3,i4]  (whereCond2ColVals wcond))

-- Projection on five attributes.
select5 :: DBAttr e atype1 -> DBAttr e atype2
        -> DBAttr e atype3 -> DBAttr e atype4
        -> DBAttr e atype5
        -> WhereCond e
        -> Query [(atype1,atype2,atype3,atype4,atype5)]
select5 (DBAttr (DBTable _ condqueryproject) i1 _ f1) (DBAttr _ i2 _ f2)
        (DBAttr _ i3 _ f3) (DBAttr _ i4 _ f4)
        (DBAttr _ i5 _ f5) wcond =
  transformQ (map (readTInfo (readTuple5 f1 f2 f3 f4 f5)))
             (condqueryproject [i1,i2,i3,i4,i5]  (whereCond2ColVals wcond))

-- Projection on six attributes.
select6 :: DBAttr e atype1 -> DBAttr e atype2
        -> DBAttr e atype3 -> DBAttr e atype4
        -> DBAttr e atype5 -> DBAttr e atype6
        -> WhereCond e
        -> Query [(atype1,atype2,atype3,atype4,atype5,atype6)]
select6 (DBAttr (DBTable _ condqueryproject) i1 _ f1) (DBAttr _ i2 _ f2)
        (DBAttr _ i3 _ f3) (DBAttr _ i4 _ f4)
        (DBAttr _ i5 _ f5) (DBAttr _ i6 _ f6) wcond =
  transformQ (map (readTInfo (readTuple6 f1 f2 f3 f4 f5 f6)))
             (condqueryproject [i1,i2,i3,i4,i5,i6]  (whereCond2ColVals wcond))

---------------------------------------------------------------------
-- Auxiliaries for reading tuples with given component readers.

-- Read results of sqlite query as tuple.
-- The argument is a reader for the tuple string.
readTInfo :: (String->a) -> String -> a
readTInfo readtuple str = readtuple $ "(" ++ str ++ ")"

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
