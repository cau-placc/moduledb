--- Compatibility module providing some operations from old
--- Database.KeyDatabaseSQLite API

module System.Transaction
  ( Query, Transaction, returnT, (|>>), (|>>=)
  , mapT, mapT_, showTError
  -- CDBI.Connection:
  , Database.CDBI.Connection.SQLResult
  )
 where

import Database.CDBI.Connection

infixl 1 |>>, |>>=

-----------------------------------------------------------------------------

type Query a = DBAction a

type Transaction a = DBAction a

showTError :: DBError -> String
showTError dberr = show dberr

returnT :: a -> Transaction a
returnT = return

(|>>) :: Transaction a -> Transaction b -> Transaction b
ta |>> tb = ta >+ tb

(|>>=) :: Transaction a -> (a -> Transaction b) -> Transaction b
ta |>>= tb = ta >+= tb

--- Applies a function that yields transactions to all elements of a
--- list, executes the transactions sequentially, and ignores their
--- results.
mapT :: (a -> Transaction b) -> [a] -> Transaction [b]
mapT = mapM

--- Applies a function that yields transactions to all elements of a
--- list, executes the transactions sequentially, and ignores their
--- results.
mapT_ :: (a -> Transaction _) -> [a] -> Transaction ()
mapT_ = mapM_


-----------------------------------------------------------------------------
