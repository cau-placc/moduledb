--- Some extensions to the generated MDB module.

module MDBHelpers where

import Database.CDBI.ER

import MDB

-----------------------------------------------------------------------
--- Shows the ID/key of a `ModData` entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showModDataID :: ModDataID -> String
showModDataID mdkey =
  Database.CDBI.ER.showDatabaseKey "ModData" modDataKeyToInt mdkey

-----------------------------------------------------------------------
