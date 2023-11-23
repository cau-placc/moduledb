--- Some extensions to the generated MDB module.

module Model.MDB.Exts where

import Database.CDBI.ER

import System.Helpers
import Model.MDB
import Model.ConfigMDB ( storageDir )

-- store DBs in term files:
storeTermDB :: IO ()
storeTermDB = saveDBTo storageDir

-- initialize DBs from term files:
readTermDB :: IO ()
readTermDB = restoreDBFrom storageDir

------------------------------------------------------------------------------
--- Shows the key of a MasterProgram entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
masterProgramKeyToString :: MasterProgramID -> String
masterProgramKeyToString key =
  Database.CDBI.ER.showDatabaseKey "MasterProgram" masterProgramKeyToInt key

------------------------------------------------------------------------------
--- Shows the ID/key of a `ModData` entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showModDataID :: ModDataID -> String
showModDataID mdkey =
  Database.CDBI.ER.showDatabaseKey "ModData" modDataKeyToInt mdkey

------------------------------------------------------------------------------
