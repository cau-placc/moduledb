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
        (DBTable(..),DBAttr(..),QueryCond(..),(@==),(@&&),whereQ,
         selectAll,selectAll1,selectAll2,selectAll3,selectAll6,selectAll7,
         select,select1,select2)
 where

import KeyDatabase
import ReadShowTerm

infix  4 @==
infixr 3 @&&
infix  1 `whereQ`

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
         Int (atype -> QueryCond etype) (Maybe (KeyDatabase.Key -> atype))

--- Equality condition between a database attribute and a value.
(@==) :: DBAttr etype atype -> atype -> QueryCond etype
(DBAttr _ _ eqfun _) @== val  =  eqfun val

-- Read results of sqlite query as tuple.
readInfo :: String -> a
readInfo str = readQTerm $ "(" ++ str ++ ")"

--- Query an entity with a condition on this entity.
selectAll :: DBTable e -> Query [e]
selectAll (DBTable condquery _) = condquery []

-- Projection on an attribute.
selectAll1 :: DBAttr e atype -> Query [atype]
selectAll1 (DBAttr (DBTable _ condqueryproject) i _ t) =
  transformQ (map (trans . readInfo))
             (condqueryproject [i] [])
 where
  trans x = maybe x (\f -> f (unsafeCoerce x)) t

-- Projection on two attributes.
selectAll2 :: DBAttr e atype1 -> DBAttr e atype2
           -> Query [(atype1,atype2)]
selectAll2 (DBAttr (DBTable _ condqueryproject) i1 _ t1) (DBAttr _ i2 _ t2) =
  transformQ (map (trans . readInfo))
             (condqueryproject [i1,i2] [])
 where
   trans (x1,x2) = (maybe x1 (\f -> f (unsafeCoerce x1)) t1,
                    maybe x2 (\f -> f (unsafeCoerce x2)) t2)


-- Projection on six attributes.
selectAll6 :: DBAttr e atype1 -> DBAttr e atype2
           -> DBAttr e atype3 -> DBAttr e atype4
           -> DBAttr e atype5 -> DBAttr e atype6
           -> Query [(atype1,atype2,atype3,atype4,atype5,atype6)]
selectAll6 (DBAttr (DBTable _ condqueryproject) i1 _ t1) (DBAttr _ i2 _ t2)
           (DBAttr _ i3 _ t3) (DBAttr _ i4 _ t4)
           (DBAttr _ i5 _ t5) (DBAttr _ i6 _ t6) =
  transformQ (map (trans . readInfo))
             (condqueryproject [i1,i2,i3,i4,i5,i6] [])
 where
   trans (x1,x2,x3,x4,x5,x6) =
     (maybe x1 (\f -> f (unsafeCoerce x1)) t1,
      maybe x2 (\f -> f (unsafeCoerce x2)) t2,
      maybe x3 (\f -> f (unsafeCoerce x3)) t3,
      maybe x4 (\f -> f (unsafeCoerce x4)) t4,
      maybe x5 (\f -> f (unsafeCoerce x5)) t5,
      maybe x6 (\f -> f (unsafeCoerce x6)) t6)

-- Projection on three attributes.
selectAll3 :: DBAttr e atype1 -> DBAttr e atype2
           -> DBAttr e atype3
           -> Query [(atype1,atype2,atype3)]
selectAll3 (DBAttr (DBTable _ condqueryproject) i1 _ t1) (DBAttr _ i2 _ t2)
           (DBAttr _ i3 _ t3) =
  transformQ (map (trans . readInfo))
             (condqueryproject [i1,i2,i3] [])
 where
   trans (x1,x2,x3) =
     (maybe x1 (\f -> f (unsafeCoerce x1)) t1,
      maybe x2 (\f -> f (unsafeCoerce x2)) t2,
      maybe x3 (\f -> f (unsafeCoerce x3)) t3)

-- Projection on seven attributes.
selectAll7 :: DBAttr e atype1 -> DBAttr e atype2
           -> DBAttr e atype3 -> DBAttr e atype4
           -> DBAttr e atype5 -> DBAttr e atype6
           -> DBAttr e atype7
           -> Query [(atype1,atype2,atype3,atype4,atype5,atype6,atype7)]
selectAll7 (DBAttr (DBTable _ condqueryproject) i1 _ t1) (DBAttr _ i2 _ t2)
           (DBAttr _ i3 _ t3) (DBAttr _ i4 _ t4)
           (DBAttr _ i5 _ t5) (DBAttr _ i6 _ t6)
           (DBAttr _ i7 _ t7) =
  transformQ (map (trans . readInfo))
             (condqueryproject [i1,i2,i3,i4,i5,i6,i7] [])
 where
   trans (x1,x2,x3,x4,x5,x6,x7) =
     (maybe x1 (\f -> f (unsafeCoerce x1)) t1,
      maybe x2 (\f -> f (unsafeCoerce x2)) t2,
      maybe x3 (\f -> f (unsafeCoerce x3)) t3,
      maybe x4 (\f -> f (unsafeCoerce x4)) t4,
      maybe x5 (\f -> f (unsafeCoerce x5)) t5,
      maybe x6 (\f -> f (unsafeCoerce x6)) t6,
      maybe x7 (\f -> f (unsafeCoerce x7)) t7)

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
select1 (DBAttr (DBTable _ condqueryproject) i _ t) qcond =
  transformQ (map (trans . readInfo))
             (condqueryproject [i] (queryCond2ColVals qcond))
 where
  trans x = maybe x (\f -> f (unsafeCoerce x)) t

-- Projection on two attributes with condition.
select2 :: DBAttr e atype1 -> DBAttr e atype2 -> QueryCond e
        -> Query [(atype1,atype2)]
select2 (DBAttr (DBTable _ condqueryproject) i1 _ t1) (DBAttr _ i2 _ t2)
             qcond =
  transformQ (map (\s -> trans (readInfo s)))
             (condqueryproject [i1,i2] (queryCond2ColVals qcond))
 where
   trans (x1,x2) = (maybe x1 (\f -> f (unsafeCoerce x1)) t1,
                    maybe x2 (\f -> f (unsafeCoerce x2)) t2)

--- A strict definition of arbitrary type conversion.
unsafeCoerce :: a -> b
unsafeCoerce x = readQTerm (showQTerm x)

------------------------------------------------------------------------
