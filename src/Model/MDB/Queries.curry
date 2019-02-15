{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=foreigncode #-}

--- A collection of SQL queries for the module database.

module MDB.Queries where

import Maybe ( listToMaybe )

import Database.CDBI.ER

import MDB

-----------------------------------------------------------------------
--- Gets a user entity with a given login name.
queryUserWithLogin :: String -> DBAction (Maybe User)
queryUserWithLogin login = liftM listToMaybe
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

--- Gets all module instances for a given module (key) taught by the
--- given lecturer (key).
queryLecturedInstancesOfMod :: ModDataID -> UserID -> DBAction [ModInst]
queryLecturedInstancesOfMod mdk uid =
  ``sql* Select * From ModInst As mi
         Where mi.ModDataModuleInstancesKey = {mdk} And
               mi.UserLecturerModsKey = {uid};''

-----------------------------------------------------------------------
