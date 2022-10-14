--- This file has been generated from
--- 
---     /net/medoc/home/mh/home/curry/applications/MDB/WithCDBI/MDB.erdterm
--- 
--- and contains definitions for all entities and relations
--- specified in this model.

module MDB where

import qualified Data.Time
import qualified Database.CDBI.ER
import qualified Database.CDBI.Criteria
import qualified Database.CDBI.Connection
import qualified Database.CDBI.Description

import ConfigMDB

data Prerequisites = Prerequisites ModDataID ModDataID
 deriving (Eq,Show,Read)

data PrerequisitesID = PrerequisitesID Int
 deriving (Eq,Show,Read)

data Categorizing = Categorizing ModDataID CategoryID
 deriving (Eq,Show,Read)

data CategorizingID = CategorizingID Int
 deriving (Eq,Show,Read)

data StudyProgram = StudyProgram StudyProgramID String String String String Int
 deriving (Eq,Show,Read)

data StudyProgramID = StudyProgramID Int
 deriving (Eq,Show,Read)

data Category = Category CategoryID String String String String Int Int Int StudyProgramID
 deriving (Eq,Show,Read)

data CategoryID = CategoryID Int
 deriving (Eq,Show,Read)

data MasterCoreArea = MasterCoreArea MasterCoreAreaID String String String String Int
 deriving (Eq,Show,Read)

data MasterCoreAreaID = MasterCoreAreaID Int
 deriving (Eq,Show,Read)

data User = User UserID String String String String String String String Data.Time.ClockTime
 deriving (Eq,Show,Read)

data UserID = UserID Int
 deriving (Eq,Show,Read)

data ModData = ModData ModDataID String String String String String Int String Int String Bool UserID
 deriving (Eq,Show,Read)

data ModDataID = ModDataID Int
 deriving (Eq,Show,Read)

data ModDescr = ModDescr ModDescrID String String String String String String String String String String String ModDataID
 deriving (Eq,Show,Read)

data ModDescrID = ModDescrID Int
 deriving (Eq,Show,Read)

data ModInst = ModInst ModInstID String Int UserID ModDataID
 deriving (Eq,Show,Read)

data ModInstID = ModInstID Int
 deriving (Eq,Show,Read)

data AdvisorStudyProgram = AdvisorStudyProgram AdvisorStudyProgramID String String Int String String String Bool UserID StudyProgramID
 deriving (Eq,Show,Read)

data AdvisorStudyProgramID = AdvisorStudyProgramID Int
 deriving (Eq,Show,Read)

data AdvisorModule = AdvisorModule AdvisorModuleID Bool AdvisorStudyProgramID CategoryID ModInstID
 deriving (Eq,Show,Read)

data AdvisorModuleID = AdvisorModuleID Int
 deriving (Eq,Show,Read)

data MasterProgram = MasterProgram MasterProgramID String String Int String String String Bool UserID MasterCoreAreaID
 deriving (Eq,Show,Read)

data MasterProgramID = MasterProgramID Int
 deriving (Eq,Show,Read)

data MasterProgInfo = MasterProgInfo MasterProgInfoID String String String String String String MasterProgramID
 deriving (Eq,Show,Read)

data MasterProgInfoID = MasterProgInfoID Int
 deriving (Eq,Show,Read)

data UnivisInfo = UnivisInfo UnivisInfoID String String Int String
 deriving (Eq,Show,Read)

data UnivisInfoID = UnivisInfoID Int
 deriving (Eq,Show,Read)

data Student = Student StudentID String String String String Data.Time.ClockTime
 deriving (Eq,Show,Read)

data StudentID = StudentID Int
 deriving (Eq,Show,Read)

data StudentCourse = StudentCourse StudentCourseID Data.Time.ClockTime StudentID ModInstID
 deriving (Eq,Show,Read)

data StudentCourseID = StudentCourseID Int
 deriving (Eq,Show,Read)

--- The name of the SQLite database file.
sqliteDBFile :: String
sqliteDBFile = storageDir ++ "MDB.db"

--- The ER description of the `Prerequisites` entity.
prerequisites_CDBI_Description
  :: Database.CDBI.Description.EntityDescription Prerequisites
prerequisites_CDBI_Description =
  Database.CDBI.Description.ED "Prerequisites"
   [Database.CDBI.Connection.SQLTypeInt,Database.CDBI.Connection.SQLTypeInt]
   (\(Prerequisites
       (ModDataID modDataPrerequisitesKey1)
       (ModDataID modDataPrerequisitesKey)) ->
     [Database.CDBI.Connection.SQLInt modDataPrerequisitesKey1
     ,Database.CDBI.Connection.SQLInt modDataPrerequisitesKey])
   (\(Prerequisites
       (ModDataID modDataPrerequisitesKey1)
       (ModDataID modDataPrerequisitesKey)) ->
     [Database.CDBI.Connection.SQLInt modDataPrerequisitesKey1
     ,Database.CDBI.Connection.SQLInt modDataPrerequisitesKey])
   (\[Database.CDBI.Connection.SQLInt modDataPrerequisitesKey1
     ,Database.CDBI.Connection.SQLInt modDataPrerequisitesKey] ->
     Prerequisites (ModDataID modDataPrerequisitesKey1)
      (ModDataID modDataPrerequisitesKey))

--- The database table of the `Prerequisites` entity.
prerequisitesTable :: Database.CDBI.Description.Table
prerequisitesTable = "Prerequisites"

--- The database column `ModDataPrerequisitesKey1` of the `Prerequisites` entity.
prerequisitesColumnModDataPrerequisitesKey1
  :: Database.CDBI.Description.Column ModDataID
prerequisitesColumnModDataPrerequisitesKey1 =
  Database.CDBI.Description.Column "\"ModDataPrerequisitesKey1\""
   "\"Prerequisites\".\"ModDataPrerequisitesKey1\""

--- The database column `ModDataPrerequisitesKey` of the `Prerequisites` entity.
prerequisitesColumnModDataPrerequisitesKey
  :: Database.CDBI.Description.Column ModDataID
prerequisitesColumnModDataPrerequisitesKey =
  Database.CDBI.Description.Column "\"ModDataPrerequisitesKey\""
   "\"Prerequisites\".\"ModDataPrerequisitesKey\""

--- The description of the database column `ModDataPrerequisitesKey1` of the `Prerequisites` entity.
prerequisitesModDataPrerequisitesKey1ColDesc
  :: Database.CDBI.Description.ColumnDescription ModDataID
prerequisitesModDataPrerequisitesKey1ColDesc =
  Database.CDBI.Description.ColDesc
   "\"Prerequisites\".\"ModDataPrerequisitesKey1\""
   Database.CDBI.Connection.SQLTypeInt
   (\(ModDataID modDataPrerequisitesKey1) ->
     Database.CDBI.Connection.SQLInt modDataPrerequisitesKey1)
   (\(Database.CDBI.Connection.SQLInt modDataPrerequisitesKey1) ->
     ModDataID modDataPrerequisitesKey1)

--- The description of the database column `ModDataPrerequisitesKey` of the `Prerequisites` entity.
prerequisitesModDataPrerequisitesKeyColDesc
  :: Database.CDBI.Description.ColumnDescription ModDataID
prerequisitesModDataPrerequisitesKeyColDesc =
  Database.CDBI.Description.ColDesc
   "\"Prerequisites\".\"ModDataPrerequisitesKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(ModDataID modDataPrerequisitesKey) ->
     Database.CDBI.Connection.SQLInt modDataPrerequisitesKey)
   (\(Database.CDBI.Connection.SQLInt modDataPrerequisitesKey) ->
     ModDataID modDataPrerequisitesKey)

--- Gets the attribute `ModDataPrerequisitesKey1` of the `Prerequisites` entity.
prerequisitesModDataPrerequisitesKey1 :: Prerequisites -> ModDataID
prerequisitesModDataPrerequisitesKey1 (Prerequisites a _) = a

--- Gets the attribute `ModDataPrerequisitesKey` of the `Prerequisites` entity.
prerequisitesModDataPrerequisitesKey :: Prerequisites -> ModDataID
prerequisitesModDataPrerequisitesKey (Prerequisites _ a) = a

--- Sets the attribute `ModDataPrerequisitesKey1` of the `Prerequisites` entity.
setPrerequisitesModDataPrerequisitesKey1
  :: Prerequisites -> ModDataID -> Prerequisites
setPrerequisitesModDataPrerequisitesKey1 (Prerequisites _ b1) a =
  Prerequisites a b1

--- Sets the attribute `ModDataPrerequisitesKey` of the `Prerequisites` entity.
setPrerequisitesModDataPrerequisitesKey
  :: Prerequisites -> ModDataID -> Prerequisites
setPrerequisitesModDataPrerequisitesKey (Prerequisites a2 _) a =
  Prerequisites a2 a

--- Inserts a new `Prerequisites` relation.
newPrerequisites
  :: ModDataID -> ModDataID -> Database.CDBI.Connection.DBAction ()
newPrerequisites k1 k2 =
  Database.CDBI.ER.insertEntry prerequisites_CDBI_Description
   (Prerequisites k1 k2)

--- Deletes an existing `Prerequisites` relation.
deletePrerequisites
  :: ModDataID -> ModDataID -> Database.CDBI.Connection.DBAction ()
deletePrerequisites k1 k2 =
  Database.CDBI.ER.deleteEntryR prerequisites_CDBI_Description
   prerequisitesColumnModDataPrerequisitesKey1
   (modDataID k1)
   prerequisitesColumnModDataPrerequisitesKey
   (modDataID k2)

--- Gets the associated `ModData` entities for a given `ModData` entity
--- w.r.t. the `Prerequisites` relation.
getModDataModDatas :: ModData -> Database.CDBI.Connection.DBAction [ModData]
getModDataModDatas en =
  Database.CDBI.ER.getEntriesWithColVal prerequisites_CDBI_Description
   prerequisitesColumnModDataPrerequisitesKey1
   (modDataID (modDataKey en))
   Database.CDBI.ER.>+= (\vals ->
     mapM getModData (map prerequisitesModDataPrerequisitesKey vals))

--- The ER description of the `Categorizing` entity.
categorizing_CDBI_Description
  :: Database.CDBI.Description.EntityDescription Categorizing
categorizing_CDBI_Description =
  Database.CDBI.Description.ED "Categorizing"
   [Database.CDBI.Connection.SQLTypeInt,Database.CDBI.Connection.SQLTypeInt]
   (\(Categorizing
       (ModDataID modDataCategorizingKey)
       (CategoryID categoryCategorizingKey)) ->
     [Database.CDBI.Connection.SQLInt modDataCategorizingKey
     ,Database.CDBI.Connection.SQLInt categoryCategorizingKey])
   (\(Categorizing
       (ModDataID modDataCategorizingKey)
       (CategoryID categoryCategorizingKey)) ->
     [Database.CDBI.Connection.SQLInt modDataCategorizingKey
     ,Database.CDBI.Connection.SQLInt categoryCategorizingKey])
   (\[Database.CDBI.Connection.SQLInt modDataCategorizingKey
     ,Database.CDBI.Connection.SQLInt categoryCategorizingKey] ->
     Categorizing (ModDataID modDataCategorizingKey)
      (CategoryID categoryCategorizingKey))

--- The database table of the `Categorizing` entity.
categorizingTable :: Database.CDBI.Description.Table
categorizingTable = "Categorizing"

--- The database column `ModDataCategorizingKey` of the `Categorizing` entity.
categorizingColumnModDataCategorizingKey
  :: Database.CDBI.Description.Column ModDataID
categorizingColumnModDataCategorizingKey =
  Database.CDBI.Description.Column "\"ModDataCategorizingKey\""
   "\"Categorizing\".\"ModDataCategorizingKey\""

--- The database column `CategoryCategorizingKey` of the `Categorizing` entity.
categorizingColumnCategoryCategorizingKey
  :: Database.CDBI.Description.Column CategoryID
categorizingColumnCategoryCategorizingKey =
  Database.CDBI.Description.Column "\"CategoryCategorizingKey\""
   "\"Categorizing\".\"CategoryCategorizingKey\""

--- The description of the database column `ModDataCategorizingKey` of the `Categorizing` entity.
categorizingModDataCategorizingKeyColDesc
  :: Database.CDBI.Description.ColumnDescription ModDataID
categorizingModDataCategorizingKeyColDesc =
  Database.CDBI.Description.ColDesc
   "\"Categorizing\".\"ModDataCategorizingKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(ModDataID modDataCategorizingKey) ->
     Database.CDBI.Connection.SQLInt modDataCategorizingKey)
   (\(Database.CDBI.Connection.SQLInt modDataCategorizingKey) ->
     ModDataID modDataCategorizingKey)

--- The description of the database column `CategoryCategorizingKey` of the `Categorizing` entity.
categorizingCategoryCategorizingKeyColDesc
  :: Database.CDBI.Description.ColumnDescription CategoryID
categorizingCategoryCategorizingKeyColDesc =
  Database.CDBI.Description.ColDesc
   "\"Categorizing\".\"CategoryCategorizingKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(CategoryID categoryCategorizingKey) ->
     Database.CDBI.Connection.SQLInt categoryCategorizingKey)
   (\(Database.CDBI.Connection.SQLInt categoryCategorizingKey) ->
     CategoryID categoryCategorizingKey)

--- Gets the attribute `ModDataCategorizingKey` of the `Categorizing` entity.
categorizingModDataCategorizingKey :: Categorizing -> ModDataID
categorizingModDataCategorizingKey (Categorizing a _) = a

--- Gets the attribute `CategoryCategorizingKey` of the `Categorizing` entity.
categorizingCategoryCategorizingKey :: Categorizing -> CategoryID
categorizingCategoryCategorizingKey (Categorizing _ a) = a

--- Sets the attribute `ModDataCategorizingKey` of the `Categorizing` entity.
setCategorizingModDataCategorizingKey
  :: Categorizing -> ModDataID -> Categorizing
setCategorizingModDataCategorizingKey (Categorizing _ b1) a = Categorizing a b1

--- Sets the attribute `CategoryCategorizingKey` of the `Categorizing` entity.
setCategorizingCategoryCategorizingKey
  :: Categorizing -> CategoryID -> Categorizing
setCategorizingCategoryCategorizingKey (Categorizing a2 _) a = Categorizing a2 a

--- Inserts a new `Categorizing` relation.
newCategorizing
  :: ModDataID -> CategoryID -> Database.CDBI.Connection.DBAction ()
newCategorizing k1 k2 =
  Database.CDBI.ER.insertEntry categorizing_CDBI_Description
   (Categorizing k1 k2)

--- Deletes an existing `Categorizing` relation.
deleteCategorizing
  :: ModDataID -> CategoryID -> Database.CDBI.Connection.DBAction ()
deleteCategorizing k1 k2 =
  Database.CDBI.ER.deleteEntryR categorizing_CDBI_Description
   categorizingColumnModDataCategorizingKey
   (modDataID k1)
   categorizingColumnCategoryCategorizingKey
   (categoryID k2)

--- Gets the associated `ModData` entities for a given `Category` entity
--- w.r.t. the `Categorizing` relation.
getModDataCategorys :: ModData -> Database.CDBI.Connection.DBAction [Category]
getModDataCategorys en =
  Database.CDBI.ER.getEntriesWithColVal categorizing_CDBI_Description
   categorizingColumnModDataCategorizingKey
   (modDataID (modDataKey en))
   Database.CDBI.ER.>+= (\vals ->
     mapM getCategory (map categorizingCategoryCategorizingKey vals))

--- The ER description of the `StudyProgram` entity.
studyProgram_CDBI_Description
  :: Database.CDBI.Description.EntityDescription StudyProgram
studyProgram_CDBI_Description =
  Database.CDBI.Description.ED "StudyProgram"
   [Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeInt]
   (\(StudyProgram
       (StudyProgramID key) name nameE shortName progKey position) ->
     [Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString name
     ,Database.CDBI.Description.sqlString nameE
     ,Database.CDBI.Connection.SQLString shortName
     ,Database.CDBI.Connection.SQLString progKey
     ,Database.CDBI.Connection.SQLInt position])
   (\(StudyProgram _ name nameE shortName progKey position) ->
     [Database.CDBI.Connection.SQLNull
     ,Database.CDBI.Connection.SQLString name
     ,Database.CDBI.Description.sqlString nameE
     ,Database.CDBI.Connection.SQLString shortName
     ,Database.CDBI.Connection.SQLString progKey
     ,Database.CDBI.Connection.SQLInt position])
   (\[Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString name
     ,nameE
     ,Database.CDBI.Connection.SQLString shortName
     ,Database.CDBI.Connection.SQLString progKey
     ,Database.CDBI.Connection.SQLInt position] ->
     StudyProgram (StudyProgramID key) name
      (Database.CDBI.Description.fromStringOrNull nameE)
      shortName
      progKey
      position)

--- The database table of the `StudyProgram` entity.
studyProgramTable :: Database.CDBI.Description.Table
studyProgramTable = "StudyProgram"

--- The database column `Key` of the `StudyProgram` entity.
studyProgramColumnKey :: Database.CDBI.Description.Column StudyProgramID
studyProgramColumnKey =
  Database.CDBI.Description.Column "\"Key\"" "\"StudyProgram\".\"Key\""

--- The database column `Name` of the `StudyProgram` entity.
studyProgramColumnName :: Database.CDBI.Description.Column String
studyProgramColumnName =
  Database.CDBI.Description.Column "\"Name\"" "\"StudyProgram\".\"Name\""

--- The database column `NameE` of the `StudyProgram` entity.
studyProgramColumnNameE :: Database.CDBI.Description.Column String
studyProgramColumnNameE =
  Database.CDBI.Description.Column "\"NameE\"" "\"StudyProgram\".\"NameE\""

--- The database column `ShortName` of the `StudyProgram` entity.
studyProgramColumnShortName :: Database.CDBI.Description.Column String
studyProgramColumnShortName =
  Database.CDBI.Description.Column "\"ShortName\""
   "\"StudyProgram\".\"ShortName\""

--- The database column `ProgKey` of the `StudyProgram` entity.
studyProgramColumnProgKey :: Database.CDBI.Description.Column String
studyProgramColumnProgKey =
  Database.CDBI.Description.Column "\"ProgKey\"" "\"StudyProgram\".\"ProgKey\""

--- The database column `Position` of the `StudyProgram` entity.
studyProgramColumnPosition :: Database.CDBI.Description.Column Int
studyProgramColumnPosition =
  Database.CDBI.Description.Column "\"Position\""
   "\"StudyProgram\".\"Position\""

--- The description of the database column `Key` of the `StudyProgram` entity.
studyProgramKeyColDesc
  :: Database.CDBI.Description.ColumnDescription StudyProgramID
studyProgramKeyColDesc =
  Database.CDBI.Description.ColDesc "\"StudyProgram\".\"Key\""
   Database.CDBI.Connection.SQLTypeInt
   (\(StudyProgramID key) -> Database.CDBI.Connection.SQLInt key)
   (\(Database.CDBI.Connection.SQLInt key) -> StudyProgramID key)

--- The description of the database column `Name` of the `StudyProgram` entity.
studyProgramNameColDesc :: Database.CDBI.Description.ColumnDescription String
studyProgramNameColDesc =
  Database.CDBI.Description.ColDesc "\"StudyProgram\".\"Name\""
   Database.CDBI.Connection.SQLTypeString
   (\name -> Database.CDBI.Connection.SQLString name)
   (\(Database.CDBI.Connection.SQLString name) -> name)

--- The description of the database column `NameE` of the `StudyProgram` entity.
studyProgramNameEColDesc :: Database.CDBI.Description.ColumnDescription String
studyProgramNameEColDesc =
  Database.CDBI.Description.ColDesc "\"StudyProgram\".\"NameE\""
   Database.CDBI.Connection.SQLTypeString
   (\nameE -> Database.CDBI.Description.sqlString nameE)
   (\nameE -> Database.CDBI.Description.fromStringOrNull nameE)

--- The description of the database column `ShortName` of the `StudyProgram` entity.
studyProgramShortNameColDesc
  :: Database.CDBI.Description.ColumnDescription String
studyProgramShortNameColDesc =
  Database.CDBI.Description.ColDesc "\"StudyProgram\".\"ShortName\""
   Database.CDBI.Connection.SQLTypeString
   (\shortName -> Database.CDBI.Connection.SQLString shortName)
   (\(Database.CDBI.Connection.SQLString shortName) -> shortName)

--- The description of the database column `ProgKey` of the `StudyProgram` entity.
studyProgramProgKeyColDesc :: Database.CDBI.Description.ColumnDescription String
studyProgramProgKeyColDesc =
  Database.CDBI.Description.ColDesc "\"StudyProgram\".\"ProgKey\""
   Database.CDBI.Connection.SQLTypeString
   (\progKey -> Database.CDBI.Connection.SQLString progKey)
   (\(Database.CDBI.Connection.SQLString progKey) -> progKey)

--- The description of the database column `Position` of the `StudyProgram` entity.
studyProgramPositionColDesc :: Database.CDBI.Description.ColumnDescription Int
studyProgramPositionColDesc =
  Database.CDBI.Description.ColDesc "\"StudyProgram\".\"Position\""
   Database.CDBI.Connection.SQLTypeInt
   (\position -> Database.CDBI.Connection.SQLInt position)
   (\(Database.CDBI.Connection.SQLInt position) -> position)

--- Gets the attribute `Key` of the `StudyProgram` entity.
studyProgramKey :: StudyProgram -> StudyProgramID
studyProgramKey (StudyProgram a _ _ _ _ _) = a

--- Gets the attribute `Name` of the `StudyProgram` entity.
studyProgramName :: StudyProgram -> String
studyProgramName (StudyProgram _ a _ _ _ _) = a

--- Gets the attribute `NameE` of the `StudyProgram` entity.
studyProgramNameE :: StudyProgram -> String
studyProgramNameE (StudyProgram _ _ a _ _ _) = a

--- Gets the attribute `ShortName` of the `StudyProgram` entity.
studyProgramShortName :: StudyProgram -> String
studyProgramShortName (StudyProgram _ _ _ a _ _) = a

--- Gets the attribute `ProgKey` of the `StudyProgram` entity.
studyProgramProgKey :: StudyProgram -> String
studyProgramProgKey (StudyProgram _ _ _ _ a _) = a

--- Gets the attribute `Position` of the `StudyProgram` entity.
studyProgramPosition :: StudyProgram -> Int
studyProgramPosition (StudyProgram _ _ _ _ _ a) = a

--- Sets the attribute `Key` of the `StudyProgram` entity.
setStudyProgramKey :: StudyProgram -> StudyProgramID -> StudyProgram
setStudyProgramKey (StudyProgram _ b5 b4 b3 b2 b1) a =
  StudyProgram a b5 b4 b3 b2 b1

--- Sets the attribute `Name` of the `StudyProgram` entity.
setStudyProgramName :: StudyProgram -> String -> StudyProgram
setStudyProgramName (StudyProgram a2 _ b4 b3 b2 b1) a =
  StudyProgram a2 a b4 b3 b2 b1

--- Sets the attribute `NameE` of the `StudyProgram` entity.
setStudyProgramNameE :: StudyProgram -> String -> StudyProgram
setStudyProgramNameE (StudyProgram a3 a2 _ b3 b2 b1) a =
  StudyProgram a3 a2 a b3 b2 b1

--- Sets the attribute `ShortName` of the `StudyProgram` entity.
setStudyProgramShortName :: StudyProgram -> String -> StudyProgram
setStudyProgramShortName (StudyProgram a4 a3 a2 _ b2 b1) a =
  StudyProgram a4 a3 a2 a b2 b1

--- Sets the attribute `ProgKey` of the `StudyProgram` entity.
setStudyProgramProgKey :: StudyProgram -> String -> StudyProgram
setStudyProgramProgKey (StudyProgram a5 a4 a3 a2 _ b1) a =
  StudyProgram a5 a4 a3 a2 a b1

--- Sets the attribute `Position` of the `StudyProgram` entity.
setStudyProgramPosition :: StudyProgram -> Int -> StudyProgram
setStudyProgramPosition (StudyProgram a6 a5 a4 a3 a2 _) a =
  StudyProgram a6 a5 a4 a3 a2 a

--- id-to-value function for entity `StudyProgram`.
studyProgramID :: StudyProgramID -> Database.CDBI.Criteria.Value StudyProgramID
studyProgramID (StudyProgramID key) = Database.CDBI.Criteria.idVal key

--- id-to-int function for entity `StudyProgram`.
studyProgramKeyToInt :: StudyProgramID -> Int
studyProgramKeyToInt (StudyProgramID key) = key

--- Shows the key of a `StudyProgram` entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showStudyProgramKey :: StudyProgram -> String
showStudyProgramKey entry =
  Database.CDBI.ER.showDatabaseKey "StudyProgram" studyProgramKeyToInt
   (studyProgramKey entry)

--- Transforms a string into a key of a `StudyProgram` entity.
--- Nothing is returned if the string does not represent a meaningful key.
readStudyProgramKey :: String -> Maybe StudyProgramID
readStudyProgramKey =
  Database.CDBI.ER.readDatabaseKey "StudyProgram" StudyProgramID

--- Gets all `StudyProgram` entities.
queryAllStudyPrograms :: Database.CDBI.Connection.DBAction [StudyProgram]
queryAllStudyPrograms =
  Database.CDBI.ER.getAllEntries studyProgram_CDBI_Description

--- Gets all `StudyProgram` entities satisfying a given predicate.
queryCondStudyProgram
  :: (StudyProgram -> Bool) -> Database.CDBI.Connection.DBAction [StudyProgram]
queryCondStudyProgram =
  Database.CDBI.ER.getCondEntries studyProgram_CDBI_Description

--- Gets a `StudyProgram` entry by a given key.
getStudyProgram
  :: StudyProgramID -> Database.CDBI.Connection.DBAction StudyProgram
getStudyProgram =
  Database.CDBI.ER.getEntryWithKey studyProgram_CDBI_Description
   studyProgramColumnKey
   studyProgramID

--- Inserts a new `StudyProgram` entity.
newStudyProgram
  :: String
  -> String
  -> String -> String -> Int -> Database.CDBI.Connection.DBAction StudyProgram
newStudyProgram name_p nameE_p shortName_p progKey_p position_p =
  Database.CDBI.ER.insertNewEntry studyProgram_CDBI_Description
   setStudyProgramKey
   StudyProgramID
   (StudyProgram (StudyProgramID 0) name_p nameE_p shortName_p progKey_p
     position_p)

--- Deletes an existing `StudyProgram` entry by its key.
deleteStudyProgram :: StudyProgram -> Database.CDBI.Connection.DBAction ()
deleteStudyProgram =
  Database.CDBI.ER.deleteEntry studyProgram_CDBI_Description
   studyProgramColumnKey
   (studyProgramID . studyProgramKey)

--- Updates an existing `StudyProgram` entry by its key.
updateStudyProgram :: StudyProgram -> Database.CDBI.Connection.DBAction ()
updateStudyProgram = Database.CDBI.ER.updateEntry studyProgram_CDBI_Description

--- The ER description of the `Category` entity.
category_CDBI_Description
  :: Database.CDBI.Description.EntityDescription Category
category_CDBI_Description =
  Database.CDBI.Description.ED "Category"
   [Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeInt]
   (\(Category
       (CategoryID key)
       name
       nameE
       shortName
       comment
       minECTS
       maxECTS
       position
       (StudyProgramID studyProgramProgramCategoriesKey)) ->
     [Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString name
     ,Database.CDBI.Description.sqlString nameE
     ,Database.CDBI.Connection.SQLString shortName
     ,Database.CDBI.Description.sqlString comment
     ,Database.CDBI.Connection.SQLInt minECTS
     ,Database.CDBI.Connection.SQLInt maxECTS
     ,Database.CDBI.Connection.SQLInt position
     ,Database.CDBI.Connection.SQLInt studyProgramProgramCategoriesKey])
   (\(Category
       _
       name
       nameE
       shortName
       comment
       minECTS
       maxECTS
       position
       (StudyProgramID studyProgramProgramCategoriesKey)) ->
     [Database.CDBI.Connection.SQLNull
     ,Database.CDBI.Connection.SQLString name
     ,Database.CDBI.Description.sqlString nameE
     ,Database.CDBI.Connection.SQLString shortName
     ,Database.CDBI.Description.sqlString comment
     ,Database.CDBI.Connection.SQLInt minECTS
     ,Database.CDBI.Connection.SQLInt maxECTS
     ,Database.CDBI.Connection.SQLInt position
     ,Database.CDBI.Connection.SQLInt studyProgramProgramCategoriesKey])
   (\[Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString name
     ,nameE
     ,Database.CDBI.Connection.SQLString shortName
     ,comment
     ,Database.CDBI.Connection.SQLInt minECTS
     ,Database.CDBI.Connection.SQLInt maxECTS
     ,Database.CDBI.Connection.SQLInt position
     ,Database.CDBI.Connection.SQLInt studyProgramProgramCategoriesKey] ->
     Category (CategoryID key) name
      (Database.CDBI.Description.fromStringOrNull nameE)
      shortName
      (Database.CDBI.Description.fromStringOrNull comment)
      minECTS
      maxECTS
      position
      (StudyProgramID studyProgramProgramCategoriesKey))

--- The database table of the `Category` entity.
categoryTable :: Database.CDBI.Description.Table
categoryTable = "Category"

--- The database column `Key` of the `Category` entity.
categoryColumnKey :: Database.CDBI.Description.Column CategoryID
categoryColumnKey =
  Database.CDBI.Description.Column "\"Key\"" "\"Category\".\"Key\""

--- The database column `Name` of the `Category` entity.
categoryColumnName :: Database.CDBI.Description.Column String
categoryColumnName =
  Database.CDBI.Description.Column "\"Name\"" "\"Category\".\"Name\""

--- The database column `NameE` of the `Category` entity.
categoryColumnNameE :: Database.CDBI.Description.Column String
categoryColumnNameE =
  Database.CDBI.Description.Column "\"NameE\"" "\"Category\".\"NameE\""

--- The database column `ShortName` of the `Category` entity.
categoryColumnShortName :: Database.CDBI.Description.Column String
categoryColumnShortName =
  Database.CDBI.Description.Column "\"ShortName\"" "\"Category\".\"ShortName\""

--- The database column `Comment` of the `Category` entity.
categoryColumnComment :: Database.CDBI.Description.Column String
categoryColumnComment =
  Database.CDBI.Description.Column "\"Comment\"" "\"Category\".\"Comment\""

--- The database column `MinECTS` of the `Category` entity.
categoryColumnMinECTS :: Database.CDBI.Description.Column Int
categoryColumnMinECTS =
  Database.CDBI.Description.Column "\"MinECTS\"" "\"Category\".\"MinECTS\""

--- The database column `MaxECTS` of the `Category` entity.
categoryColumnMaxECTS :: Database.CDBI.Description.Column Int
categoryColumnMaxECTS =
  Database.CDBI.Description.Column "\"MaxECTS\"" "\"Category\".\"MaxECTS\""

--- The database column `Position` of the `Category` entity.
categoryColumnPosition :: Database.CDBI.Description.Column Int
categoryColumnPosition =
  Database.CDBI.Description.Column "\"Position\"" "\"Category\".\"Position\""

--- The database column `StudyProgramProgramCategoriesKey` of the `Category` entity.
categoryColumnStudyProgramProgramCategoriesKey
  :: Database.CDBI.Description.Column StudyProgramID
categoryColumnStudyProgramProgramCategoriesKey =
  Database.CDBI.Description.Column "\"StudyProgramProgramCategoriesKey\""
   "\"Category\".\"StudyProgramProgramCategoriesKey\""

--- The description of the database column `Key` of the `Category` entity.
categoryKeyColDesc :: Database.CDBI.Description.ColumnDescription CategoryID
categoryKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Category\".\"Key\""
   Database.CDBI.Connection.SQLTypeInt
   (\(CategoryID key) -> Database.CDBI.Connection.SQLInt key)
   (\(Database.CDBI.Connection.SQLInt key) -> CategoryID key)

--- The description of the database column `Name` of the `Category` entity.
categoryNameColDesc :: Database.CDBI.Description.ColumnDescription String
categoryNameColDesc =
  Database.CDBI.Description.ColDesc "\"Category\".\"Name\""
   Database.CDBI.Connection.SQLTypeString
   (\name -> Database.CDBI.Connection.SQLString name)
   (\(Database.CDBI.Connection.SQLString name) -> name)

--- The description of the database column `NameE` of the `Category` entity.
categoryNameEColDesc :: Database.CDBI.Description.ColumnDescription String
categoryNameEColDesc =
  Database.CDBI.Description.ColDesc "\"Category\".\"NameE\""
   Database.CDBI.Connection.SQLTypeString
   (\nameE -> Database.CDBI.Description.sqlString nameE)
   (\nameE -> Database.CDBI.Description.fromStringOrNull nameE)

--- The description of the database column `ShortName` of the `Category` entity.
categoryShortNameColDesc :: Database.CDBI.Description.ColumnDescription String
categoryShortNameColDesc =
  Database.CDBI.Description.ColDesc "\"Category\".\"ShortName\""
   Database.CDBI.Connection.SQLTypeString
   (\shortName -> Database.CDBI.Connection.SQLString shortName)
   (\(Database.CDBI.Connection.SQLString shortName) -> shortName)

--- The description of the database column `Comment` of the `Category` entity.
categoryCommentColDesc :: Database.CDBI.Description.ColumnDescription String
categoryCommentColDesc =
  Database.CDBI.Description.ColDesc "\"Category\".\"Comment\""
   Database.CDBI.Connection.SQLTypeString
   (\comment -> Database.CDBI.Description.sqlString comment)
   (\comment -> Database.CDBI.Description.fromStringOrNull comment)

--- The description of the database column `MinECTS` of the `Category` entity.
categoryMinECTSColDesc :: Database.CDBI.Description.ColumnDescription Int
categoryMinECTSColDesc =
  Database.CDBI.Description.ColDesc "\"Category\".\"MinECTS\""
   Database.CDBI.Connection.SQLTypeInt
   (\minECTS -> Database.CDBI.Connection.SQLInt minECTS)
   (\(Database.CDBI.Connection.SQLInt minECTS) -> minECTS)

--- The description of the database column `MaxECTS` of the `Category` entity.
categoryMaxECTSColDesc :: Database.CDBI.Description.ColumnDescription Int
categoryMaxECTSColDesc =
  Database.CDBI.Description.ColDesc "\"Category\".\"MaxECTS\""
   Database.CDBI.Connection.SQLTypeInt
   (\maxECTS -> Database.CDBI.Connection.SQLInt maxECTS)
   (\(Database.CDBI.Connection.SQLInt maxECTS) -> maxECTS)

--- The description of the database column `Position` of the `Category` entity.
categoryPositionColDesc :: Database.CDBI.Description.ColumnDescription Int
categoryPositionColDesc =
  Database.CDBI.Description.ColDesc "\"Category\".\"Position\""
   Database.CDBI.Connection.SQLTypeInt
   (\position -> Database.CDBI.Connection.SQLInt position)
   (\(Database.CDBI.Connection.SQLInt position) -> position)

--- The description of the database column `StudyProgramProgramCategoriesKey` of the `Category` entity.
categoryStudyProgramProgramCategoriesKeyColDesc
  :: Database.CDBI.Description.ColumnDescription StudyProgramID
categoryStudyProgramProgramCategoriesKeyColDesc =
  Database.CDBI.Description.ColDesc
   "\"Category\".\"StudyProgramProgramCategoriesKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(StudyProgramID studyProgramProgramCategoriesKey) ->
     Database.CDBI.Connection.SQLInt studyProgramProgramCategoriesKey)
   (\(Database.CDBI.Connection.SQLInt studyProgramProgramCategoriesKey) ->
     StudyProgramID studyProgramProgramCategoriesKey)

--- Gets the attribute `Key` of the `Category` entity.
categoryKey :: Category -> CategoryID
categoryKey (Category a _ _ _ _ _ _ _ _) = a

--- Gets the attribute `Name` of the `Category` entity.
categoryName :: Category -> String
categoryName (Category _ a _ _ _ _ _ _ _) = a

--- Gets the attribute `NameE` of the `Category` entity.
categoryNameE :: Category -> String
categoryNameE (Category _ _ a _ _ _ _ _ _) = a

--- Gets the attribute `ShortName` of the `Category` entity.
categoryShortName :: Category -> String
categoryShortName (Category _ _ _ a _ _ _ _ _) = a

--- Gets the attribute `Comment` of the `Category` entity.
categoryComment :: Category -> String
categoryComment (Category _ _ _ _ a _ _ _ _) = a

--- Gets the attribute `MinECTS` of the `Category` entity.
categoryMinECTS :: Category -> Int
categoryMinECTS (Category _ _ _ _ _ a _ _ _) = a

--- Gets the attribute `MaxECTS` of the `Category` entity.
categoryMaxECTS :: Category -> Int
categoryMaxECTS (Category _ _ _ _ _ _ a _ _) = a

--- Gets the attribute `Position` of the `Category` entity.
categoryPosition :: Category -> Int
categoryPosition (Category _ _ _ _ _ _ _ a _) = a

--- Gets the attribute `StudyProgramProgramCategoriesKey` of the `Category` entity.
categoryStudyProgramProgramCategoriesKey :: Category -> StudyProgramID
categoryStudyProgramProgramCategoriesKey (Category _ _ _ _ _ _ _ _ a) = a

--- Sets the attribute `Key` of the `Category` entity.
setCategoryKey :: Category -> CategoryID -> Category
setCategoryKey (Category _ b8 b7 b6 b5 b4 b3 b2 b1) a =
  Category a b8 b7 b6 b5 b4 b3 b2 b1

--- Sets the attribute `Name` of the `Category` entity.
setCategoryName :: Category -> String -> Category
setCategoryName (Category a2 _ b7 b6 b5 b4 b3 b2 b1) a =
  Category a2 a b7 b6 b5 b4 b3 b2 b1

--- Sets the attribute `NameE` of the `Category` entity.
setCategoryNameE :: Category -> String -> Category
setCategoryNameE (Category a3 a2 _ b6 b5 b4 b3 b2 b1) a =
  Category a3 a2 a b6 b5 b4 b3 b2 b1

--- Sets the attribute `ShortName` of the `Category` entity.
setCategoryShortName :: Category -> String -> Category
setCategoryShortName (Category a4 a3 a2 _ b5 b4 b3 b2 b1) a =
  Category a4 a3 a2 a b5 b4 b3 b2 b1

--- Sets the attribute `Comment` of the `Category` entity.
setCategoryComment :: Category -> String -> Category
setCategoryComment (Category a5 a4 a3 a2 _ b4 b3 b2 b1) a =
  Category a5 a4 a3 a2 a b4 b3 b2 b1

--- Sets the attribute `MinECTS` of the `Category` entity.
setCategoryMinECTS :: Category -> Int -> Category
setCategoryMinECTS (Category a6 a5 a4 a3 a2 _ b3 b2 b1) a =
  Category a6 a5 a4 a3 a2 a b3 b2 b1

--- Sets the attribute `MaxECTS` of the `Category` entity.
setCategoryMaxECTS :: Category -> Int -> Category
setCategoryMaxECTS (Category a7 a6 a5 a4 a3 a2 _ b2 b1) a =
  Category a7 a6 a5 a4 a3 a2 a b2 b1

--- Sets the attribute `Position` of the `Category` entity.
setCategoryPosition :: Category -> Int -> Category
setCategoryPosition (Category a8 a7 a6 a5 a4 a3 a2 _ b1) a =
  Category a8 a7 a6 a5 a4 a3 a2 a b1

--- Sets the attribute `StudyProgramProgramCategoriesKey` of the `Category` entity.
setCategoryStudyProgramProgramCategoriesKey
  :: Category -> StudyProgramID -> Category
setCategoryStudyProgramProgramCategoriesKey
    (Category a9 a8 a7 a6 a5 a4 a3 a2 _) a =
  Category a9 a8 a7 a6 a5 a4 a3 a2 a

--- id-to-value function for entity `Category`.
categoryID :: CategoryID -> Database.CDBI.Criteria.Value CategoryID
categoryID (CategoryID key) = Database.CDBI.Criteria.idVal key

--- id-to-int function for entity `Category`.
categoryKeyToInt :: CategoryID -> Int
categoryKeyToInt (CategoryID key) = key

--- Shows the key of a `Category` entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showCategoryKey :: Category -> String
showCategoryKey entry =
  Database.CDBI.ER.showDatabaseKey "Category" categoryKeyToInt
   (categoryKey entry)

--- Transforms a string into a key of a `Category` entity.
--- Nothing is returned if the string does not represent a meaningful key.
readCategoryKey :: String -> Maybe CategoryID
readCategoryKey = Database.CDBI.ER.readDatabaseKey "Category" CategoryID

--- Gets all `Category` entities.
queryAllCategorys :: Database.CDBI.Connection.DBAction [Category]
queryAllCategorys = Database.CDBI.ER.getAllEntries category_CDBI_Description

--- Gets all `Category` entities satisfying a given predicate.
queryCondCategory
  :: (Category -> Bool) -> Database.CDBI.Connection.DBAction [Category]
queryCondCategory = Database.CDBI.ER.getCondEntries category_CDBI_Description

--- Gets a `Category` entry by a given key.
getCategory :: CategoryID -> Database.CDBI.Connection.DBAction Category
getCategory =
  Database.CDBI.ER.getEntryWithKey category_CDBI_Description categoryColumnKey
   categoryID

--- Inserts a new `Category` entity.
newCategoryWithStudyProgramProgramCategoriesKey
  :: String
  -> String
  -> String
  -> String
  -> Maybe Int
  -> Maybe Int
  -> Int -> StudyProgramID -> Database.CDBI.Connection.DBAction Category
newCategoryWithStudyProgramProgramCategoriesKey
    name_p
    nameE_p
    shortName_p
    comment_p
    minECTS_p
    maxECTS_p
    position_p
    studyProgramProgramCategoriesKey_p =
  Database.CDBI.ER.insertNewEntry category_CDBI_Description setCategoryKey
   CategoryID
   (Category (CategoryID 0) name_p nameE_p shortName_p comment_p
     (maybe 0 id minECTS_p)
     (maybe 180 id maxECTS_p)
     position_p
     studyProgramProgramCategoriesKey_p)

--- Deletes an existing `Category` entry by its key.
deleteCategory :: Category -> Database.CDBI.Connection.DBAction ()
deleteCategory =
  Database.CDBI.ER.deleteEntry category_CDBI_Description categoryColumnKey
   (categoryID . categoryKey)

--- Updates an existing `Category` entry by its key.
updateCategory :: Category -> Database.CDBI.Connection.DBAction ()
updateCategory = Database.CDBI.ER.updateEntry category_CDBI_Description

--- The ER description of the `MasterCoreArea` entity.
masterCoreArea_CDBI_Description
  :: Database.CDBI.Description.EntityDescription MasterCoreArea
masterCoreArea_CDBI_Description =
  Database.CDBI.Description.ED "MasterCoreArea"
   [Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeInt]
   (\(MasterCoreArea
       (MasterCoreAreaID key) name shortName description areaKey position) ->
     [Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString name
     ,Database.CDBI.Connection.SQLString shortName
     ,Database.CDBI.Description.sqlString description
     ,Database.CDBI.Connection.SQLString areaKey
     ,Database.CDBI.Connection.SQLInt position])
   (\(MasterCoreArea _ name shortName description areaKey position) ->
     [Database.CDBI.Connection.SQLNull
     ,Database.CDBI.Connection.SQLString name
     ,Database.CDBI.Connection.SQLString shortName
     ,Database.CDBI.Description.sqlString description
     ,Database.CDBI.Connection.SQLString areaKey
     ,Database.CDBI.Connection.SQLInt position])
   (\[Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString name
     ,Database.CDBI.Connection.SQLString shortName
     ,description
     ,Database.CDBI.Connection.SQLString areaKey
     ,Database.CDBI.Connection.SQLInt position] ->
     MasterCoreArea (MasterCoreAreaID key) name shortName
      (Database.CDBI.Description.fromStringOrNull description)
      areaKey
      position)

--- The database table of the `MasterCoreArea` entity.
masterCoreAreaTable :: Database.CDBI.Description.Table
masterCoreAreaTable = "MasterCoreArea"

--- The database column `Key` of the `MasterCoreArea` entity.
masterCoreAreaColumnKey :: Database.CDBI.Description.Column MasterCoreAreaID
masterCoreAreaColumnKey =
  Database.CDBI.Description.Column "\"Key\"" "\"MasterCoreArea\".\"Key\""

--- The database column `Name` of the `MasterCoreArea` entity.
masterCoreAreaColumnName :: Database.CDBI.Description.Column String
masterCoreAreaColumnName =
  Database.CDBI.Description.Column "\"Name\"" "\"MasterCoreArea\".\"Name\""

--- The database column `ShortName` of the `MasterCoreArea` entity.
masterCoreAreaColumnShortName :: Database.CDBI.Description.Column String
masterCoreAreaColumnShortName =
  Database.CDBI.Description.Column "\"ShortName\""
   "\"MasterCoreArea\".\"ShortName\""

--- The database column `Description` of the `MasterCoreArea` entity.
masterCoreAreaColumnDescription :: Database.CDBI.Description.Column String
masterCoreAreaColumnDescription =
  Database.CDBI.Description.Column "\"Description\""
   "\"MasterCoreArea\".\"Description\""

--- The database column `AreaKey` of the `MasterCoreArea` entity.
masterCoreAreaColumnAreaKey :: Database.CDBI.Description.Column String
masterCoreAreaColumnAreaKey =
  Database.CDBI.Description.Column "\"AreaKey\""
   "\"MasterCoreArea\".\"AreaKey\""

--- The database column `Position` of the `MasterCoreArea` entity.
masterCoreAreaColumnPosition :: Database.CDBI.Description.Column Int
masterCoreAreaColumnPosition =
  Database.CDBI.Description.Column "\"Position\""
   "\"MasterCoreArea\".\"Position\""

--- The description of the database column `Key` of the `MasterCoreArea` entity.
masterCoreAreaKeyColDesc
  :: Database.CDBI.Description.ColumnDescription MasterCoreAreaID
masterCoreAreaKeyColDesc =
  Database.CDBI.Description.ColDesc "\"MasterCoreArea\".\"Key\""
   Database.CDBI.Connection.SQLTypeInt
   (\(MasterCoreAreaID key) -> Database.CDBI.Connection.SQLInt key)
   (\(Database.CDBI.Connection.SQLInt key) -> MasterCoreAreaID key)

--- The description of the database column `Name` of the `MasterCoreArea` entity.
masterCoreAreaNameColDesc :: Database.CDBI.Description.ColumnDescription String
masterCoreAreaNameColDesc =
  Database.CDBI.Description.ColDesc "\"MasterCoreArea\".\"Name\""
   Database.CDBI.Connection.SQLTypeString
   (\name -> Database.CDBI.Connection.SQLString name)
   (\(Database.CDBI.Connection.SQLString name) -> name)

--- The description of the database column `ShortName` of the `MasterCoreArea` entity.
masterCoreAreaShortNameColDesc
  :: Database.CDBI.Description.ColumnDescription String
masterCoreAreaShortNameColDesc =
  Database.CDBI.Description.ColDesc "\"MasterCoreArea\".\"ShortName\""
   Database.CDBI.Connection.SQLTypeString
   (\shortName -> Database.CDBI.Connection.SQLString shortName)
   (\(Database.CDBI.Connection.SQLString shortName) -> shortName)

--- The description of the database column `Description` of the `MasterCoreArea` entity.
masterCoreAreaDescriptionColDesc
  :: Database.CDBI.Description.ColumnDescription String
masterCoreAreaDescriptionColDesc =
  Database.CDBI.Description.ColDesc "\"MasterCoreArea\".\"Description\""
   Database.CDBI.Connection.SQLTypeString
   (\description -> Database.CDBI.Description.sqlString description)
   (\description -> Database.CDBI.Description.fromStringOrNull description)

--- The description of the database column `AreaKey` of the `MasterCoreArea` entity.
masterCoreAreaAreaKeyColDesc
  :: Database.CDBI.Description.ColumnDescription String
masterCoreAreaAreaKeyColDesc =
  Database.CDBI.Description.ColDesc "\"MasterCoreArea\".\"AreaKey\""
   Database.CDBI.Connection.SQLTypeString
   (\areaKey -> Database.CDBI.Connection.SQLString areaKey)
   (\(Database.CDBI.Connection.SQLString areaKey) -> areaKey)

--- The description of the database column `Position` of the `MasterCoreArea` entity.
masterCoreAreaPositionColDesc :: Database.CDBI.Description.ColumnDescription Int
masterCoreAreaPositionColDesc =
  Database.CDBI.Description.ColDesc "\"MasterCoreArea\".\"Position\""
   Database.CDBI.Connection.SQLTypeInt
   (\position -> Database.CDBI.Connection.SQLInt position)
   (\(Database.CDBI.Connection.SQLInt position) -> position)

--- Gets the attribute `Key` of the `MasterCoreArea` entity.
masterCoreAreaKey :: MasterCoreArea -> MasterCoreAreaID
masterCoreAreaKey (MasterCoreArea a _ _ _ _ _) = a

--- Gets the attribute `Name` of the `MasterCoreArea` entity.
masterCoreAreaName :: MasterCoreArea -> String
masterCoreAreaName (MasterCoreArea _ a _ _ _ _) = a

--- Gets the attribute `ShortName` of the `MasterCoreArea` entity.
masterCoreAreaShortName :: MasterCoreArea -> String
masterCoreAreaShortName (MasterCoreArea _ _ a _ _ _) = a

--- Gets the attribute `Description` of the `MasterCoreArea` entity.
masterCoreAreaDescription :: MasterCoreArea -> String
masterCoreAreaDescription (MasterCoreArea _ _ _ a _ _) = a

--- Gets the attribute `AreaKey` of the `MasterCoreArea` entity.
masterCoreAreaAreaKey :: MasterCoreArea -> String
masterCoreAreaAreaKey (MasterCoreArea _ _ _ _ a _) = a

--- Gets the attribute `Position` of the `MasterCoreArea` entity.
masterCoreAreaPosition :: MasterCoreArea -> Int
masterCoreAreaPosition (MasterCoreArea _ _ _ _ _ a) = a

--- Sets the attribute `Key` of the `MasterCoreArea` entity.
setMasterCoreAreaKey :: MasterCoreArea -> MasterCoreAreaID -> MasterCoreArea
setMasterCoreAreaKey (MasterCoreArea _ b5 b4 b3 b2 b1) a =
  MasterCoreArea a b5 b4 b3 b2 b1

--- Sets the attribute `Name` of the `MasterCoreArea` entity.
setMasterCoreAreaName :: MasterCoreArea -> String -> MasterCoreArea
setMasterCoreAreaName (MasterCoreArea a2 _ b4 b3 b2 b1) a =
  MasterCoreArea a2 a b4 b3 b2 b1

--- Sets the attribute `ShortName` of the `MasterCoreArea` entity.
setMasterCoreAreaShortName :: MasterCoreArea -> String -> MasterCoreArea
setMasterCoreAreaShortName (MasterCoreArea a3 a2 _ b3 b2 b1) a =
  MasterCoreArea a3 a2 a b3 b2 b1

--- Sets the attribute `Description` of the `MasterCoreArea` entity.
setMasterCoreAreaDescription :: MasterCoreArea -> String -> MasterCoreArea
setMasterCoreAreaDescription (MasterCoreArea a4 a3 a2 _ b2 b1) a =
  MasterCoreArea a4 a3 a2 a b2 b1

--- Sets the attribute `AreaKey` of the `MasterCoreArea` entity.
setMasterCoreAreaAreaKey :: MasterCoreArea -> String -> MasterCoreArea
setMasterCoreAreaAreaKey (MasterCoreArea a5 a4 a3 a2 _ b1) a =
  MasterCoreArea a5 a4 a3 a2 a b1

--- Sets the attribute `Position` of the `MasterCoreArea` entity.
setMasterCoreAreaPosition :: MasterCoreArea -> Int -> MasterCoreArea
setMasterCoreAreaPosition (MasterCoreArea a6 a5 a4 a3 a2 _) a =
  MasterCoreArea a6 a5 a4 a3 a2 a

--- id-to-value function for entity `MasterCoreArea`.
masterCoreAreaID
  :: MasterCoreAreaID -> Database.CDBI.Criteria.Value MasterCoreAreaID
masterCoreAreaID (MasterCoreAreaID key) = Database.CDBI.Criteria.idVal key

--- id-to-int function for entity `MasterCoreArea`.
masterCoreAreaKeyToInt :: MasterCoreAreaID -> Int
masterCoreAreaKeyToInt (MasterCoreAreaID key) = key

--- Shows the key of a `MasterCoreArea` entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showMasterCoreAreaKey :: MasterCoreArea -> String
showMasterCoreAreaKey entry =
  Database.CDBI.ER.showDatabaseKey "MasterCoreArea" masterCoreAreaKeyToInt
   (masterCoreAreaKey entry)

--- Transforms a string into a key of a `MasterCoreArea` entity.
--- Nothing is returned if the string does not represent a meaningful key.
readMasterCoreAreaKey :: String -> Maybe MasterCoreAreaID
readMasterCoreAreaKey =
  Database.CDBI.ER.readDatabaseKey "MasterCoreArea" MasterCoreAreaID

--- Gets all `MasterCoreArea` entities.
queryAllMasterCoreAreas :: Database.CDBI.Connection.DBAction [MasterCoreArea]
queryAllMasterCoreAreas =
  Database.CDBI.ER.getAllEntries masterCoreArea_CDBI_Description

--- Gets all `MasterCoreArea` entities satisfying a given predicate.
queryCondMasterCoreArea
  :: (MasterCoreArea -> Bool)
  -> Database.CDBI.Connection.DBAction [MasterCoreArea]
queryCondMasterCoreArea =
  Database.CDBI.ER.getCondEntries masterCoreArea_CDBI_Description

--- Gets a `MasterCoreArea` entry by a given key.
getMasterCoreArea
  :: MasterCoreAreaID -> Database.CDBI.Connection.DBAction MasterCoreArea
getMasterCoreArea =
  Database.CDBI.ER.getEntryWithKey masterCoreArea_CDBI_Description
   masterCoreAreaColumnKey
   masterCoreAreaID

--- Inserts a new `MasterCoreArea` entity.
newMasterCoreArea
  :: String
  -> String
  -> String
  -> String -> Maybe Int -> Database.CDBI.Connection.DBAction MasterCoreArea
newMasterCoreArea name_p shortName_p description_p areaKey_p position_p =
  Database.CDBI.ER.insertNewEntry masterCoreArea_CDBI_Description
   setMasterCoreAreaKey
   MasterCoreAreaID
   (MasterCoreArea (MasterCoreAreaID 0) name_p shortName_p description_p
     areaKey_p
     (maybe 1 id position_p))

--- Deletes an existing `MasterCoreArea` entry by its key.
deleteMasterCoreArea :: MasterCoreArea -> Database.CDBI.Connection.DBAction ()
deleteMasterCoreArea =
  Database.CDBI.ER.deleteEntry masterCoreArea_CDBI_Description
   masterCoreAreaColumnKey
   (masterCoreAreaID . masterCoreAreaKey)

--- Updates an existing `MasterCoreArea` entry by its key.
updateMasterCoreArea :: MasterCoreArea -> Database.CDBI.Connection.DBAction ()
updateMasterCoreArea =
  Database.CDBI.ER.updateEntry masterCoreArea_CDBI_Description

--- The ER description of the `User` entity.
user_CDBI_Description :: Database.CDBI.Description.EntityDescription User
user_CDBI_Description =
  Database.CDBI.Description.ED "User"
   [Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeDate]
   (\(User (UserID key) login name first title email url password lastLogin) ->
     [Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString login
     ,Database.CDBI.Connection.SQLString name
     ,Database.CDBI.Description.sqlString first
     ,Database.CDBI.Description.sqlString title
     ,Database.CDBI.Description.sqlString email
     ,Database.CDBI.Description.sqlString url
     ,Database.CDBI.Description.sqlString password
     ,Database.CDBI.Connection.SQLDate lastLogin])
   (\(User _ login name first title email url password lastLogin) ->
     [Database.CDBI.Connection.SQLNull
     ,Database.CDBI.Connection.SQLString login
     ,Database.CDBI.Connection.SQLString name
     ,Database.CDBI.Description.sqlString first
     ,Database.CDBI.Description.sqlString title
     ,Database.CDBI.Description.sqlString email
     ,Database.CDBI.Description.sqlString url
     ,Database.CDBI.Description.sqlString password
     ,Database.CDBI.Connection.SQLDate lastLogin])
   (\[Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString login
     ,Database.CDBI.Connection.SQLString name
     ,first
     ,title
     ,email
     ,url
     ,password
     ,Database.CDBI.Connection.SQLDate lastLogin] ->
     User (UserID key) login name
      (Database.CDBI.Description.fromStringOrNull first)
      (Database.CDBI.Description.fromStringOrNull title)
      (Database.CDBI.Description.fromStringOrNull email)
      (Database.CDBI.Description.fromStringOrNull url)
      (Database.CDBI.Description.fromStringOrNull password)
      lastLogin)

--- The database table of the `User` entity.
userTable :: Database.CDBI.Description.Table
userTable = "User"

--- The database column `Key` of the `User` entity.
userColumnKey :: Database.CDBI.Description.Column UserID
userColumnKey = Database.CDBI.Description.Column "\"Key\"" "\"User\".\"Key\""

--- The database column `Login` of the `User` entity.
userColumnLogin :: Database.CDBI.Description.Column String
userColumnLogin =
  Database.CDBI.Description.Column "\"Login\"" "\"User\".\"Login\""

--- The database column `Name` of the `User` entity.
userColumnName :: Database.CDBI.Description.Column String
userColumnName = Database.CDBI.Description.Column "\"Name\"" "\"User\".\"Name\""

--- The database column `First` of the `User` entity.
userColumnFirst :: Database.CDBI.Description.Column String
userColumnFirst =
  Database.CDBI.Description.Column "\"First\"" "\"User\".\"First\""

--- The database column `Title` of the `User` entity.
userColumnTitle :: Database.CDBI.Description.Column String
userColumnTitle =
  Database.CDBI.Description.Column "\"Title\"" "\"User\".\"Title\""

--- The database column `Email` of the `User` entity.
userColumnEmail :: Database.CDBI.Description.Column String
userColumnEmail =
  Database.CDBI.Description.Column "\"Email\"" "\"User\".\"Email\""

--- The database column `Url` of the `User` entity.
userColumnUrl :: Database.CDBI.Description.Column String
userColumnUrl = Database.CDBI.Description.Column "\"Url\"" "\"User\".\"Url\""

--- The database column `Password` of the `User` entity.
userColumnPassword :: Database.CDBI.Description.Column String
userColumnPassword =
  Database.CDBI.Description.Column "\"Password\"" "\"User\".\"Password\""

--- The database column `LastLogin` of the `User` entity.
userColumnLastLogin :: Database.CDBI.Description.Column Data.Time.ClockTime
userColumnLastLogin =
  Database.CDBI.Description.Column "\"LastLogin\"" "\"User\".\"LastLogin\""

--- The description of the database column `Key` of the `User` entity.
userKeyColDesc :: Database.CDBI.Description.ColumnDescription UserID
userKeyColDesc =
  Database.CDBI.Description.ColDesc "\"User\".\"Key\""
   Database.CDBI.Connection.SQLTypeInt
   (\(UserID key) -> Database.CDBI.Connection.SQLInt key)
   (\(Database.CDBI.Connection.SQLInt key) -> UserID key)

--- The description of the database column `Login` of the `User` entity.
userLoginColDesc :: Database.CDBI.Description.ColumnDescription String
userLoginColDesc =
  Database.CDBI.Description.ColDesc "\"User\".\"Login\""
   Database.CDBI.Connection.SQLTypeString
   (\login -> Database.CDBI.Connection.SQLString login)
   (\(Database.CDBI.Connection.SQLString login) -> login)

--- The description of the database column `Name` of the `User` entity.
userNameColDesc :: Database.CDBI.Description.ColumnDescription String
userNameColDesc =
  Database.CDBI.Description.ColDesc "\"User\".\"Name\""
   Database.CDBI.Connection.SQLTypeString
   (\name -> Database.CDBI.Connection.SQLString name)
   (\(Database.CDBI.Connection.SQLString name) -> name)

--- The description of the database column `First` of the `User` entity.
userFirstColDesc :: Database.CDBI.Description.ColumnDescription String
userFirstColDesc =
  Database.CDBI.Description.ColDesc "\"User\".\"First\""
   Database.CDBI.Connection.SQLTypeString
   (\first -> Database.CDBI.Description.sqlString first)
   (\first -> Database.CDBI.Description.fromStringOrNull first)

--- The description of the database column `Title` of the `User` entity.
userTitleColDesc :: Database.CDBI.Description.ColumnDescription String
userTitleColDesc =
  Database.CDBI.Description.ColDesc "\"User\".\"Title\""
   Database.CDBI.Connection.SQLTypeString
   (\title -> Database.CDBI.Description.sqlString title)
   (\title -> Database.CDBI.Description.fromStringOrNull title)

--- The description of the database column `Email` of the `User` entity.
userEmailColDesc :: Database.CDBI.Description.ColumnDescription String
userEmailColDesc =
  Database.CDBI.Description.ColDesc "\"User\".\"Email\""
   Database.CDBI.Connection.SQLTypeString
   (\email -> Database.CDBI.Description.sqlString email)
   (\email -> Database.CDBI.Description.fromStringOrNull email)

--- The description of the database column `Url` of the `User` entity.
userUrlColDesc :: Database.CDBI.Description.ColumnDescription String
userUrlColDesc =
  Database.CDBI.Description.ColDesc "\"User\".\"Url\""
   Database.CDBI.Connection.SQLTypeString
   (\url -> Database.CDBI.Description.sqlString url)
   (\url -> Database.CDBI.Description.fromStringOrNull url)

--- The description of the database column `Password` of the `User` entity.
userPasswordColDesc :: Database.CDBI.Description.ColumnDescription String
userPasswordColDesc =
  Database.CDBI.Description.ColDesc "\"User\".\"Password\""
   Database.CDBI.Connection.SQLTypeString
   (\password -> Database.CDBI.Description.sqlString password)
   (\password -> Database.CDBI.Description.fromStringOrNull password)

--- The description of the database column `LastLogin` of the `User` entity.
userLastLoginColDesc
  :: Database.CDBI.Description.ColumnDescription Data.Time.ClockTime
userLastLoginColDesc =
  Database.CDBI.Description.ColDesc "\"User\".\"LastLogin\""
   Database.CDBI.Connection.SQLTypeDate
   (\lastLogin -> Database.CDBI.Connection.SQLDate lastLogin)
   (\(Database.CDBI.Connection.SQLDate lastLogin) -> lastLogin)

--- Gets the attribute `Key` of the `User` entity.
userKey :: User -> UserID
userKey (User a _ _ _ _ _ _ _ _) = a

--- Gets the attribute `Login` of the `User` entity.
userLogin :: User -> String
userLogin (User _ a _ _ _ _ _ _ _) = a

--- Gets the attribute `Name` of the `User` entity.
userName :: User -> String
userName (User _ _ a _ _ _ _ _ _) = a

--- Gets the attribute `First` of the `User` entity.
userFirst :: User -> String
userFirst (User _ _ _ a _ _ _ _ _) = a

--- Gets the attribute `Title` of the `User` entity.
userTitle :: User -> String
userTitle (User _ _ _ _ a _ _ _ _) = a

--- Gets the attribute `Email` of the `User` entity.
userEmail :: User -> String
userEmail (User _ _ _ _ _ a _ _ _) = a

--- Gets the attribute `Url` of the `User` entity.
userUrl :: User -> String
userUrl (User _ _ _ _ _ _ a _ _) = a

--- Gets the attribute `Password` of the `User` entity.
userPassword :: User -> String
userPassword (User _ _ _ _ _ _ _ a _) = a

--- Gets the attribute `LastLogin` of the `User` entity.
userLastLogin :: User -> Data.Time.ClockTime
userLastLogin (User _ _ _ _ _ _ _ _ a) = a

--- Sets the attribute `Key` of the `User` entity.
setUserKey :: User -> UserID -> User
setUserKey (User _ b8 b7 b6 b5 b4 b3 b2 b1) a = User a b8 b7 b6 b5 b4 b3 b2 b1

--- Sets the attribute `Login` of the `User` entity.
setUserLogin :: User -> String -> User
setUserLogin (User a2 _ b7 b6 b5 b4 b3 b2 b1) a = User a2 a b7 b6 b5 b4 b3 b2 b1

--- Sets the attribute `Name` of the `User` entity.
setUserName :: User -> String -> User
setUserName (User a3 a2 _ b6 b5 b4 b3 b2 b1) a = User a3 a2 a b6 b5 b4 b3 b2 b1

--- Sets the attribute `First` of the `User` entity.
setUserFirst :: User -> String -> User
setUserFirst (User a4 a3 a2 _ b5 b4 b3 b2 b1) a = User a4 a3 a2 a b5 b4 b3 b2 b1

--- Sets the attribute `Title` of the `User` entity.
setUserTitle :: User -> String -> User
setUserTitle (User a5 a4 a3 a2 _ b4 b3 b2 b1) a = User a5 a4 a3 a2 a b4 b3 b2 b1

--- Sets the attribute `Email` of the `User` entity.
setUserEmail :: User -> String -> User
setUserEmail (User a6 a5 a4 a3 a2 _ b3 b2 b1) a = User a6 a5 a4 a3 a2 a b3 b2 b1

--- Sets the attribute `Url` of the `User` entity.
setUserUrl :: User -> String -> User
setUserUrl (User a7 a6 a5 a4 a3 a2 _ b2 b1) a = User a7 a6 a5 a4 a3 a2 a b2 b1

--- Sets the attribute `Password` of the `User` entity.
setUserPassword :: User -> String -> User
setUserPassword (User a8 a7 a6 a5 a4 a3 a2 _ b1) a =
  User a8 a7 a6 a5 a4 a3 a2 a b1

--- Sets the attribute `LastLogin` of the `User` entity.
setUserLastLogin :: User -> Data.Time.ClockTime -> User
setUserLastLogin (User a9 a8 a7 a6 a5 a4 a3 a2 _) a =
  User a9 a8 a7 a6 a5 a4 a3 a2 a

--- id-to-value function for entity `User`.
userID :: UserID -> Database.CDBI.Criteria.Value UserID
userID (UserID key) = Database.CDBI.Criteria.idVal key

--- id-to-int function for entity `User`.
userKeyToInt :: UserID -> Int
userKeyToInt (UserID key) = key

--- Shows the key of a `User` entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showUserKey :: User -> String
showUserKey entry =
  Database.CDBI.ER.showDatabaseKey "User" userKeyToInt (userKey entry)

--- Transforms a string into a key of a `User` entity.
--- Nothing is returned if the string does not represent a meaningful key.
readUserKey :: String -> Maybe UserID
readUserKey = Database.CDBI.ER.readDatabaseKey "User" UserID

--- Gets all `User` entities.
queryAllUsers :: Database.CDBI.Connection.DBAction [User]
queryAllUsers = Database.CDBI.ER.getAllEntries user_CDBI_Description

--- Gets all `User` entities satisfying a given predicate.
queryCondUser :: (User -> Bool) -> Database.CDBI.Connection.DBAction [User]
queryCondUser = Database.CDBI.ER.getCondEntries user_CDBI_Description

--- Gets a `User` entry by a given key.
getUser :: UserID -> Database.CDBI.Connection.DBAction User
getUser =
  Database.CDBI.ER.getEntryWithKey user_CDBI_Description userColumnKey userID

--- Inserts a new `User` entity.
newUser
  :: String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> String -> Data.Time.ClockTime -> Database.CDBI.Connection.DBAction User
newUser login_p name_p first_p title_p email_p url_p password_p lastLogin_p =
  Database.CDBI.ER.insertNewEntry user_CDBI_Description setUserKey UserID
   (User (UserID 0) login_p name_p first_p title_p email_p url_p password_p
     lastLogin_p)

--- Deletes an existing `User` entry by its key.
deleteUser :: User -> Database.CDBI.Connection.DBAction ()
deleteUser =
  Database.CDBI.ER.deleteEntry user_CDBI_Description userColumnKey
   (userID . userKey)

--- Updates an existing `User` entry by its key.
updateUser :: User -> Database.CDBI.Connection.DBAction ()
updateUser = Database.CDBI.ER.updateEntry user_CDBI_Description

--- The ER description of the `ModData` entity.
modData_CDBI_Description :: Database.CDBI.Description.EntityDescription ModData
modData_CDBI_Description =
  Database.CDBI.Description.ED "ModData"
   [Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeBool
   ,Database.CDBI.Connection.SQLTypeInt]
   (\(ModData
       (ModDataID key)
       code
       nameG
       nameE
       cycle
       presence
       eCTS
       workload
       length
       uRL
       visible
       (UserID userResponsibleKey)) ->
     [Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString code
     ,Database.CDBI.Connection.SQLString nameG
     ,Database.CDBI.Description.sqlString nameE
     ,Database.CDBI.Description.sqlString cycle
     ,Database.CDBI.Description.sqlString presence
     ,Database.CDBI.Connection.SQLInt eCTS
     ,Database.CDBI.Description.sqlString workload
     ,Database.CDBI.Connection.SQLInt length
     ,Database.CDBI.Description.sqlString uRL
     ,Database.CDBI.Connection.SQLBool visible
     ,Database.CDBI.Connection.SQLInt userResponsibleKey])
   (\(ModData
       _
       code
       nameG
       nameE
       cycle
       presence
       eCTS
       workload
       length
       uRL
       visible
       (UserID userResponsibleKey)) ->
     [Database.CDBI.Connection.SQLNull
     ,Database.CDBI.Connection.SQLString code
     ,Database.CDBI.Connection.SQLString nameG
     ,Database.CDBI.Description.sqlString nameE
     ,Database.CDBI.Description.sqlString cycle
     ,Database.CDBI.Description.sqlString presence
     ,Database.CDBI.Connection.SQLInt eCTS
     ,Database.CDBI.Description.sqlString workload
     ,Database.CDBI.Connection.SQLInt length
     ,Database.CDBI.Description.sqlString uRL
     ,Database.CDBI.Connection.SQLBool visible
     ,Database.CDBI.Connection.SQLInt userResponsibleKey])
   (\[Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString code
     ,Database.CDBI.Connection.SQLString nameG
     ,nameE
     ,cycle
     ,presence
     ,Database.CDBI.Connection.SQLInt eCTS
     ,workload
     ,Database.CDBI.Connection.SQLInt length
     ,uRL
     ,Database.CDBI.Connection.SQLBool visible
     ,Database.CDBI.Connection.SQLInt userResponsibleKey] ->
     ModData (ModDataID key) code nameG
      (Database.CDBI.Description.fromStringOrNull nameE)
      (Database.CDBI.Description.fromStringOrNull cycle)
      (Database.CDBI.Description.fromStringOrNull presence)
      eCTS
      (Database.CDBI.Description.fromStringOrNull workload)
      length
      (Database.CDBI.Description.fromStringOrNull uRL)
      visible
      (UserID userResponsibleKey))

--- The database table of the `ModData` entity.
modDataTable :: Database.CDBI.Description.Table
modDataTable = "ModData"

--- The database column `Key` of the `ModData` entity.
modDataColumnKey :: Database.CDBI.Description.Column ModDataID
modDataColumnKey =
  Database.CDBI.Description.Column "\"Key\"" "\"ModData\".\"Key\""

--- The database column `Code` of the `ModData` entity.
modDataColumnCode :: Database.CDBI.Description.Column String
modDataColumnCode =
  Database.CDBI.Description.Column "\"Code\"" "\"ModData\".\"Code\""

--- The database column `NameG` of the `ModData` entity.
modDataColumnNameG :: Database.CDBI.Description.Column String
modDataColumnNameG =
  Database.CDBI.Description.Column "\"NameG\"" "\"ModData\".\"NameG\""

--- The database column `NameE` of the `ModData` entity.
modDataColumnNameE :: Database.CDBI.Description.Column String
modDataColumnNameE =
  Database.CDBI.Description.Column "\"NameE\"" "\"ModData\".\"NameE\""

--- The database column `Cycle` of the `ModData` entity.
modDataColumnCycle :: Database.CDBI.Description.Column String
modDataColumnCycle =
  Database.CDBI.Description.Column "\"Cycle\"" "\"ModData\".\"Cycle\""

--- The database column `Presence` of the `ModData` entity.
modDataColumnPresence :: Database.CDBI.Description.Column String
modDataColumnPresence =
  Database.CDBI.Description.Column "\"Presence\"" "\"ModData\".\"Presence\""

--- The database column `ECTS` of the `ModData` entity.
modDataColumnECTS :: Database.CDBI.Description.Column Int
modDataColumnECTS =
  Database.CDBI.Description.Column "\"ECTS\"" "\"ModData\".\"ECTS\""

--- The database column `Workload` of the `ModData` entity.
modDataColumnWorkload :: Database.CDBI.Description.Column String
modDataColumnWorkload =
  Database.CDBI.Description.Column "\"Workload\"" "\"ModData\".\"Workload\""

--- The database column `Length` of the `ModData` entity.
modDataColumnLength :: Database.CDBI.Description.Column Int
modDataColumnLength =
  Database.CDBI.Description.Column "\"Length\"" "\"ModData\".\"Length\""

--- The database column `URL` of the `ModData` entity.
modDataColumnURL :: Database.CDBI.Description.Column String
modDataColumnURL =
  Database.CDBI.Description.Column "\"URL\"" "\"ModData\".\"URL\""

--- The database column `Visible` of the `ModData` entity.
modDataColumnVisible :: Database.CDBI.Description.Column Bool
modDataColumnVisible =
  Database.CDBI.Description.Column "\"Visible\"" "\"ModData\".\"Visible\""

--- The database column `UserResponsibleKey` of the `ModData` entity.
modDataColumnUserResponsibleKey :: Database.CDBI.Description.Column UserID
modDataColumnUserResponsibleKey =
  Database.CDBI.Description.Column "\"UserResponsibleKey\""
   "\"ModData\".\"UserResponsibleKey\""

--- The description of the database column `Key` of the `ModData` entity.
modDataKeyColDesc :: Database.CDBI.Description.ColumnDescription ModDataID
modDataKeyColDesc =
  Database.CDBI.Description.ColDesc "\"ModData\".\"Key\""
   Database.CDBI.Connection.SQLTypeInt
   (\(ModDataID key) -> Database.CDBI.Connection.SQLInt key)
   (\(Database.CDBI.Connection.SQLInt key) -> ModDataID key)

--- The description of the database column `Code` of the `ModData` entity.
modDataCodeColDesc :: Database.CDBI.Description.ColumnDescription String
modDataCodeColDesc =
  Database.CDBI.Description.ColDesc "\"ModData\".\"Code\""
   Database.CDBI.Connection.SQLTypeString
   (\code -> Database.CDBI.Connection.SQLString code)
   (\(Database.CDBI.Connection.SQLString code) -> code)

--- The description of the database column `NameG` of the `ModData` entity.
modDataNameGColDesc :: Database.CDBI.Description.ColumnDescription String
modDataNameGColDesc =
  Database.CDBI.Description.ColDesc "\"ModData\".\"NameG\""
   Database.CDBI.Connection.SQLTypeString
   (\nameG -> Database.CDBI.Connection.SQLString nameG)
   (\(Database.CDBI.Connection.SQLString nameG) -> nameG)

--- The description of the database column `NameE` of the `ModData` entity.
modDataNameEColDesc :: Database.CDBI.Description.ColumnDescription String
modDataNameEColDesc =
  Database.CDBI.Description.ColDesc "\"ModData\".\"NameE\""
   Database.CDBI.Connection.SQLTypeString
   (\nameE -> Database.CDBI.Description.sqlString nameE)
   (\nameE -> Database.CDBI.Description.fromStringOrNull nameE)

--- The description of the database column `Cycle` of the `ModData` entity.
modDataCycleColDesc :: Database.CDBI.Description.ColumnDescription String
modDataCycleColDesc =
  Database.CDBI.Description.ColDesc "\"ModData\".\"Cycle\""
   Database.CDBI.Connection.SQLTypeString
   (\cycle -> Database.CDBI.Description.sqlString cycle)
   (\cycle -> Database.CDBI.Description.fromStringOrNull cycle)

--- The description of the database column `Presence` of the `ModData` entity.
modDataPresenceColDesc :: Database.CDBI.Description.ColumnDescription String
modDataPresenceColDesc =
  Database.CDBI.Description.ColDesc "\"ModData\".\"Presence\""
   Database.CDBI.Connection.SQLTypeString
   (\presence -> Database.CDBI.Description.sqlString presence)
   (\presence -> Database.CDBI.Description.fromStringOrNull presence)

--- The description of the database column `ECTS` of the `ModData` entity.
modDataECTSColDesc :: Database.CDBI.Description.ColumnDescription Int
modDataECTSColDesc =
  Database.CDBI.Description.ColDesc "\"ModData\".\"ECTS\""
   Database.CDBI.Connection.SQLTypeInt
   (\eCTS -> Database.CDBI.Connection.SQLInt eCTS)
   (\(Database.CDBI.Connection.SQLInt eCTS) -> eCTS)

--- The description of the database column `Workload` of the `ModData` entity.
modDataWorkloadColDesc :: Database.CDBI.Description.ColumnDescription String
modDataWorkloadColDesc =
  Database.CDBI.Description.ColDesc "\"ModData\".\"Workload\""
   Database.CDBI.Connection.SQLTypeString
   (\workload -> Database.CDBI.Description.sqlString workload)
   (\workload -> Database.CDBI.Description.fromStringOrNull workload)

--- The description of the database column `Length` of the `ModData` entity.
modDataLengthColDesc :: Database.CDBI.Description.ColumnDescription Int
modDataLengthColDesc =
  Database.CDBI.Description.ColDesc "\"ModData\".\"Length\""
   Database.CDBI.Connection.SQLTypeInt
   (\length -> Database.CDBI.Connection.SQLInt length)
   (\(Database.CDBI.Connection.SQLInt length) -> length)

--- The description of the database column `URL` of the `ModData` entity.
modDataURLColDesc :: Database.CDBI.Description.ColumnDescription String
modDataURLColDesc =
  Database.CDBI.Description.ColDesc "\"ModData\".\"URL\""
   Database.CDBI.Connection.SQLTypeString
   (\uRL -> Database.CDBI.Description.sqlString uRL)
   (\uRL -> Database.CDBI.Description.fromStringOrNull uRL)

--- The description of the database column `Visible` of the `ModData` entity.
modDataVisibleColDesc :: Database.CDBI.Description.ColumnDescription Bool
modDataVisibleColDesc =
  Database.CDBI.Description.ColDesc "\"ModData\".\"Visible\""
   Database.CDBI.Connection.SQLTypeBool
   (\visible -> Database.CDBI.Connection.SQLBool visible)
   (\(Database.CDBI.Connection.SQLBool visible) -> visible)

--- The description of the database column `UserResponsibleKey` of the `ModData` entity.
modDataUserResponsibleKeyColDesc
  :: Database.CDBI.Description.ColumnDescription UserID
modDataUserResponsibleKeyColDesc =
  Database.CDBI.Description.ColDesc "\"ModData\".\"UserResponsibleKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(UserID userResponsibleKey) ->
     Database.CDBI.Connection.SQLInt userResponsibleKey)
   (\(Database.CDBI.Connection.SQLInt userResponsibleKey) ->
     UserID userResponsibleKey)

--- Gets the attribute `Key` of the `ModData` entity.
modDataKey :: ModData -> ModDataID
modDataKey (ModData a _ _ _ _ _ _ _ _ _ _ _) = a

--- Gets the attribute `Code` of the `ModData` entity.
modDataCode :: ModData -> String
modDataCode (ModData _ a _ _ _ _ _ _ _ _ _ _) = a

--- Gets the attribute `NameG` of the `ModData` entity.
modDataNameG :: ModData -> String
modDataNameG (ModData _ _ a _ _ _ _ _ _ _ _ _) = a

--- Gets the attribute `NameE` of the `ModData` entity.
modDataNameE :: ModData -> String
modDataNameE (ModData _ _ _ a _ _ _ _ _ _ _ _) = a

--- Gets the attribute `Cycle` of the `ModData` entity.
modDataCycle :: ModData -> String
modDataCycle (ModData _ _ _ _ a _ _ _ _ _ _ _) = a

--- Gets the attribute `Presence` of the `ModData` entity.
modDataPresence :: ModData -> String
modDataPresence (ModData _ _ _ _ _ a _ _ _ _ _ _) = a

--- Gets the attribute `ECTS` of the `ModData` entity.
modDataECTS :: ModData -> Int
modDataECTS (ModData _ _ _ _ _ _ a _ _ _ _ _) = a

--- Gets the attribute `Workload` of the `ModData` entity.
modDataWorkload :: ModData -> String
modDataWorkload (ModData _ _ _ _ _ _ _ a _ _ _ _) = a

--- Gets the attribute `Length` of the `ModData` entity.
modDataLength :: ModData -> Int
modDataLength (ModData _ _ _ _ _ _ _ _ a _ _ _) = a

--- Gets the attribute `URL` of the `ModData` entity.
modDataURL :: ModData -> String
modDataURL (ModData _ _ _ _ _ _ _ _ _ a _ _) = a

--- Gets the attribute `Visible` of the `ModData` entity.
modDataVisible :: ModData -> Bool
modDataVisible (ModData _ _ _ _ _ _ _ _ _ _ a _) = a

--- Gets the attribute `UserResponsibleKey` of the `ModData` entity.
modDataUserResponsibleKey :: ModData -> UserID
modDataUserResponsibleKey (ModData _ _ _ _ _ _ _ _ _ _ _ a) = a

--- Sets the attribute `Key` of the `ModData` entity.
setModDataKey :: ModData -> ModDataID -> ModData
setModDataKey (ModData _ b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1) a =
  ModData a b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1

--- Sets the attribute `Code` of the `ModData` entity.
setModDataCode :: ModData -> String -> ModData
setModDataCode (ModData a2 _ b10 b9 b8 b7 b6 b5 b4 b3 b2 b1) a =
  ModData a2 a b10 b9 b8 b7 b6 b5 b4 b3 b2 b1

--- Sets the attribute `NameG` of the `ModData` entity.
setModDataNameG :: ModData -> String -> ModData
setModDataNameG (ModData a3 a2 _ b9 b8 b7 b6 b5 b4 b3 b2 b1) a =
  ModData a3 a2 a b9 b8 b7 b6 b5 b4 b3 b2 b1

--- Sets the attribute `NameE` of the `ModData` entity.
setModDataNameE :: ModData -> String -> ModData
setModDataNameE (ModData a4 a3 a2 _ b8 b7 b6 b5 b4 b3 b2 b1) a =
  ModData a4 a3 a2 a b8 b7 b6 b5 b4 b3 b2 b1

--- Sets the attribute `Cycle` of the `ModData` entity.
setModDataCycle :: ModData -> String -> ModData
setModDataCycle (ModData a5 a4 a3 a2 _ b7 b6 b5 b4 b3 b2 b1) a =
  ModData a5 a4 a3 a2 a b7 b6 b5 b4 b3 b2 b1

--- Sets the attribute `Presence` of the `ModData` entity.
setModDataPresence :: ModData -> String -> ModData
setModDataPresence (ModData a6 a5 a4 a3 a2 _ b6 b5 b4 b3 b2 b1) a =
  ModData a6 a5 a4 a3 a2 a b6 b5 b4 b3 b2 b1

--- Sets the attribute `ECTS` of the `ModData` entity.
setModDataECTS :: ModData -> Int -> ModData
setModDataECTS (ModData a7 a6 a5 a4 a3 a2 _ b5 b4 b3 b2 b1) a =
  ModData a7 a6 a5 a4 a3 a2 a b5 b4 b3 b2 b1

--- Sets the attribute `Workload` of the `ModData` entity.
setModDataWorkload :: ModData -> String -> ModData
setModDataWorkload (ModData a8 a7 a6 a5 a4 a3 a2 _ b4 b3 b2 b1) a =
  ModData a8 a7 a6 a5 a4 a3 a2 a b4 b3 b2 b1

--- Sets the attribute `Length` of the `ModData` entity.
setModDataLength :: ModData -> Int -> ModData
setModDataLength (ModData a9 a8 a7 a6 a5 a4 a3 a2 _ b3 b2 b1) a =
  ModData a9 a8 a7 a6 a5 a4 a3 a2 a b3 b2 b1

--- Sets the attribute `URL` of the `ModData` entity.
setModDataURL :: ModData -> String -> ModData
setModDataURL (ModData a10 a9 a8 a7 a6 a5 a4 a3 a2 _ b2 b1) a =
  ModData a10 a9 a8 a7 a6 a5 a4 a3 a2 a b2 b1

--- Sets the attribute `Visible` of the `ModData` entity.
setModDataVisible :: ModData -> Bool -> ModData
setModDataVisible (ModData a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 _ b1) a =
  ModData a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a b1

--- Sets the attribute `UserResponsibleKey` of the `ModData` entity.
setModDataUserResponsibleKey :: ModData -> UserID -> ModData
setModDataUserResponsibleKey (ModData a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 _) a =
  ModData a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a

--- id-to-value function for entity `ModData`.
modDataID :: ModDataID -> Database.CDBI.Criteria.Value ModDataID
modDataID (ModDataID key) = Database.CDBI.Criteria.idVal key

--- id-to-int function for entity `ModData`.
modDataKeyToInt :: ModDataID -> Int
modDataKeyToInt (ModDataID key) = key

--- Shows the key of a `ModData` entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showModDataKey :: ModData -> String
showModDataKey entry =
  Database.CDBI.ER.showDatabaseKey "ModData" modDataKeyToInt (modDataKey entry)

--- Transforms a string into a key of a `ModData` entity.
--- Nothing is returned if the string does not represent a meaningful key.
readModDataKey :: String -> Maybe ModDataID
readModDataKey = Database.CDBI.ER.readDatabaseKey "ModData" ModDataID

--- Gets all `ModData` entities.
queryAllModDatas :: Database.CDBI.Connection.DBAction [ModData]
queryAllModDatas = Database.CDBI.ER.getAllEntries modData_CDBI_Description

--- Gets all `ModData` entities satisfying a given predicate.
queryCondModData
  :: (ModData -> Bool) -> Database.CDBI.Connection.DBAction [ModData]
queryCondModData = Database.CDBI.ER.getCondEntries modData_CDBI_Description

--- Gets a `ModData` entry by a given key.
getModData :: ModDataID -> Database.CDBI.Connection.DBAction ModData
getModData =
  Database.CDBI.ER.getEntryWithKey modData_CDBI_Description modDataColumnKey
   modDataID

--- Inserts a new `ModData` entity.
newModDataWithUserResponsibleKey
  :: String
  -> String
  -> String
  -> String
  -> String
  -> Maybe Int
  -> String
  -> Maybe Int
  -> String -> Bool -> UserID -> Database.CDBI.Connection.DBAction ModData
newModDataWithUserResponsibleKey
    code_p
    nameG_p
    nameE_p
    cycle_p
    presence_p
    eCTS_p
    workload_p
    length_p
    uRL_p
    visible_p
    userResponsibleKey_p =
  Database.CDBI.ER.insertNewEntry modData_CDBI_Description setModDataKey
   ModDataID
   (ModData (ModDataID 0) code_p nameG_p nameE_p cycle_p presence_p
     (maybe 8 id eCTS_p)
     workload_p
     (maybe 1 id length_p)
     uRL_p
     visible_p
     userResponsibleKey_p)

--- Deletes an existing `ModData` entry by its key.
deleteModData :: ModData -> Database.CDBI.Connection.DBAction ()
deleteModData =
  Database.CDBI.ER.deleteEntry modData_CDBI_Description modDataColumnKey
   (modDataID . modDataKey)

--- Updates an existing `ModData` entry by its key.
updateModData :: ModData -> Database.CDBI.Connection.DBAction ()
updateModData = Database.CDBI.ER.updateEntry modData_CDBI_Description

--- The ER description of the `ModDescr` entity.
modDescr_CDBI_Description
  :: Database.CDBI.Description.EntityDescription ModDescr
modDescr_CDBI_Description =
  Database.CDBI.Description.ED "ModDescr"
   [Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeInt]
   (\(ModDescr
       (ModDescrID key)
       language
       shortDesc
       objectives
       contents
       prereq
       exam
       methods
       use
       literature
       links
       comments
       (ModDataID modDataDataDescKey)) ->
     [Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString language
     ,Database.CDBI.Description.sqlString shortDesc
     ,Database.CDBI.Description.sqlString objectives
     ,Database.CDBI.Description.sqlString contents
     ,Database.CDBI.Description.sqlString prereq
     ,Database.CDBI.Description.sqlString exam
     ,Database.CDBI.Description.sqlString methods
     ,Database.CDBI.Description.sqlString use
     ,Database.CDBI.Description.sqlString literature
     ,Database.CDBI.Description.sqlString links
     ,Database.CDBI.Description.sqlString comments
     ,Database.CDBI.Connection.SQLInt modDataDataDescKey])
   (\(ModDescr
       _
       language
       shortDesc
       objectives
       contents
       prereq
       exam
       methods
       use
       literature
       links
       comments
       (ModDataID modDataDataDescKey)) ->
     [Database.CDBI.Connection.SQLNull
     ,Database.CDBI.Connection.SQLString language
     ,Database.CDBI.Description.sqlString shortDesc
     ,Database.CDBI.Description.sqlString objectives
     ,Database.CDBI.Description.sqlString contents
     ,Database.CDBI.Description.sqlString prereq
     ,Database.CDBI.Description.sqlString exam
     ,Database.CDBI.Description.sqlString methods
     ,Database.CDBI.Description.sqlString use
     ,Database.CDBI.Description.sqlString literature
     ,Database.CDBI.Description.sqlString links
     ,Database.CDBI.Description.sqlString comments
     ,Database.CDBI.Connection.SQLInt modDataDataDescKey])
   (\[Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString language
     ,shortDesc
     ,objectives
     ,contents
     ,prereq
     ,exam
     ,methods
     ,use
     ,literature
     ,links
     ,comments
     ,Database.CDBI.Connection.SQLInt modDataDataDescKey] ->
     ModDescr (ModDescrID key) language
      (Database.CDBI.Description.fromStringOrNull shortDesc)
      (Database.CDBI.Description.fromStringOrNull objectives)
      (Database.CDBI.Description.fromStringOrNull contents)
      (Database.CDBI.Description.fromStringOrNull prereq)
      (Database.CDBI.Description.fromStringOrNull exam)
      (Database.CDBI.Description.fromStringOrNull methods)
      (Database.CDBI.Description.fromStringOrNull use)
      (Database.CDBI.Description.fromStringOrNull literature)
      (Database.CDBI.Description.fromStringOrNull links)
      (Database.CDBI.Description.fromStringOrNull comments)
      (ModDataID modDataDataDescKey))

--- The database table of the `ModDescr` entity.
modDescrTable :: Database.CDBI.Description.Table
modDescrTable = "ModDescr"

--- The database column `Key` of the `ModDescr` entity.
modDescrColumnKey :: Database.CDBI.Description.Column ModDescrID
modDescrColumnKey =
  Database.CDBI.Description.Column "\"Key\"" "\"ModDescr\".\"Key\""

--- The database column `Language` of the `ModDescr` entity.
modDescrColumnLanguage :: Database.CDBI.Description.Column String
modDescrColumnLanguage =
  Database.CDBI.Description.Column "\"Language\"" "\"ModDescr\".\"Language\""

--- The database column `ShortDesc` of the `ModDescr` entity.
modDescrColumnShortDesc :: Database.CDBI.Description.Column String
modDescrColumnShortDesc =
  Database.CDBI.Description.Column "\"ShortDesc\"" "\"ModDescr\".\"ShortDesc\""

--- The database column `Objectives` of the `ModDescr` entity.
modDescrColumnObjectives :: Database.CDBI.Description.Column String
modDescrColumnObjectives =
  Database.CDBI.Description.Column "\"Objectives\""
   "\"ModDescr\".\"Objectives\""

--- The database column `Contents` of the `ModDescr` entity.
modDescrColumnContents :: Database.CDBI.Description.Column String
modDescrColumnContents =
  Database.CDBI.Description.Column "\"Contents\"" "\"ModDescr\".\"Contents\""

--- The database column `Prereq` of the `ModDescr` entity.
modDescrColumnPrereq :: Database.CDBI.Description.Column String
modDescrColumnPrereq =
  Database.CDBI.Description.Column "\"Prereq\"" "\"ModDescr\".\"Prereq\""

--- The database column `Exam` of the `ModDescr` entity.
modDescrColumnExam :: Database.CDBI.Description.Column String
modDescrColumnExam =
  Database.CDBI.Description.Column "\"Exam\"" "\"ModDescr\".\"Exam\""

--- The database column `Methods` of the `ModDescr` entity.
modDescrColumnMethods :: Database.CDBI.Description.Column String
modDescrColumnMethods =
  Database.CDBI.Description.Column "\"Methods\"" "\"ModDescr\".\"Methods\""

--- The database column `Use` of the `ModDescr` entity.
modDescrColumnUse :: Database.CDBI.Description.Column String
modDescrColumnUse =
  Database.CDBI.Description.Column "\"Use\"" "\"ModDescr\".\"Use\""

--- The database column `Literature` of the `ModDescr` entity.
modDescrColumnLiterature :: Database.CDBI.Description.Column String
modDescrColumnLiterature =
  Database.CDBI.Description.Column "\"Literature\""
   "\"ModDescr\".\"Literature\""

--- The database column `Links` of the `ModDescr` entity.
modDescrColumnLinks :: Database.CDBI.Description.Column String
modDescrColumnLinks =
  Database.CDBI.Description.Column "\"Links\"" "\"ModDescr\".\"Links\""

--- The database column `Comments` of the `ModDescr` entity.
modDescrColumnComments :: Database.CDBI.Description.Column String
modDescrColumnComments =
  Database.CDBI.Description.Column "\"Comments\"" "\"ModDescr\".\"Comments\""

--- The database column `ModDataDataDescKey` of the `ModDescr` entity.
modDescrColumnModDataDataDescKey :: Database.CDBI.Description.Column ModDataID
modDescrColumnModDataDataDescKey =
  Database.CDBI.Description.Column "\"ModDataDataDescKey\""
   "\"ModDescr\".\"ModDataDataDescKey\""

--- The description of the database column `Key` of the `ModDescr` entity.
modDescrKeyColDesc :: Database.CDBI.Description.ColumnDescription ModDescrID
modDescrKeyColDesc =
  Database.CDBI.Description.ColDesc "\"ModDescr\".\"Key\""
   Database.CDBI.Connection.SQLTypeInt
   (\(ModDescrID key) -> Database.CDBI.Connection.SQLInt key)
   (\(Database.CDBI.Connection.SQLInt key) -> ModDescrID key)

--- The description of the database column `Language` of the `ModDescr` entity.
modDescrLanguageColDesc :: Database.CDBI.Description.ColumnDescription String
modDescrLanguageColDesc =
  Database.CDBI.Description.ColDesc "\"ModDescr\".\"Language\""
   Database.CDBI.Connection.SQLTypeString
   (\language -> Database.CDBI.Connection.SQLString language)
   (\(Database.CDBI.Connection.SQLString language) -> language)

--- The description of the database column `ShortDesc` of the `ModDescr` entity.
modDescrShortDescColDesc :: Database.CDBI.Description.ColumnDescription String
modDescrShortDescColDesc =
  Database.CDBI.Description.ColDesc "\"ModDescr\".\"ShortDesc\""
   Database.CDBI.Connection.SQLTypeString
   (\shortDesc -> Database.CDBI.Description.sqlString shortDesc)
   (\shortDesc -> Database.CDBI.Description.fromStringOrNull shortDesc)

--- The description of the database column `Objectives` of the `ModDescr` entity.
modDescrObjectivesColDesc :: Database.CDBI.Description.ColumnDescription String
modDescrObjectivesColDesc =
  Database.CDBI.Description.ColDesc "\"ModDescr\".\"Objectives\""
   Database.CDBI.Connection.SQLTypeString
   (\objectives -> Database.CDBI.Description.sqlString objectives)
   (\objectives -> Database.CDBI.Description.fromStringOrNull objectives)

--- The description of the database column `Contents` of the `ModDescr` entity.
modDescrContentsColDesc :: Database.CDBI.Description.ColumnDescription String
modDescrContentsColDesc =
  Database.CDBI.Description.ColDesc "\"ModDescr\".\"Contents\""
   Database.CDBI.Connection.SQLTypeString
   (\contents -> Database.CDBI.Description.sqlString contents)
   (\contents -> Database.CDBI.Description.fromStringOrNull contents)

--- The description of the database column `Prereq` of the `ModDescr` entity.
modDescrPrereqColDesc :: Database.CDBI.Description.ColumnDescription String
modDescrPrereqColDesc =
  Database.CDBI.Description.ColDesc "\"ModDescr\".\"Prereq\""
   Database.CDBI.Connection.SQLTypeString
   (\prereq -> Database.CDBI.Description.sqlString prereq)
   (\prereq -> Database.CDBI.Description.fromStringOrNull prereq)

--- The description of the database column `Exam` of the `ModDescr` entity.
modDescrExamColDesc :: Database.CDBI.Description.ColumnDescription String
modDescrExamColDesc =
  Database.CDBI.Description.ColDesc "\"ModDescr\".\"Exam\""
   Database.CDBI.Connection.SQLTypeString
   (\exam -> Database.CDBI.Description.sqlString exam)
   (\exam -> Database.CDBI.Description.fromStringOrNull exam)

--- The description of the database column `Methods` of the `ModDescr` entity.
modDescrMethodsColDesc :: Database.CDBI.Description.ColumnDescription String
modDescrMethodsColDesc =
  Database.CDBI.Description.ColDesc "\"ModDescr\".\"Methods\""
   Database.CDBI.Connection.SQLTypeString
   (\methods -> Database.CDBI.Description.sqlString methods)
   (\methods -> Database.CDBI.Description.fromStringOrNull methods)

--- The description of the database column `Use` of the `ModDescr` entity.
modDescrUseColDesc :: Database.CDBI.Description.ColumnDescription String
modDescrUseColDesc =
  Database.CDBI.Description.ColDesc "\"ModDescr\".\"Use\""
   Database.CDBI.Connection.SQLTypeString
   (\use -> Database.CDBI.Description.sqlString use)
   (\use -> Database.CDBI.Description.fromStringOrNull use)

--- The description of the database column `Literature` of the `ModDescr` entity.
modDescrLiteratureColDesc :: Database.CDBI.Description.ColumnDescription String
modDescrLiteratureColDesc =
  Database.CDBI.Description.ColDesc "\"ModDescr\".\"Literature\""
   Database.CDBI.Connection.SQLTypeString
   (\literature -> Database.CDBI.Description.sqlString literature)
   (\literature -> Database.CDBI.Description.fromStringOrNull literature)

--- The description of the database column `Links` of the `ModDescr` entity.
modDescrLinksColDesc :: Database.CDBI.Description.ColumnDescription String
modDescrLinksColDesc =
  Database.CDBI.Description.ColDesc "\"ModDescr\".\"Links\""
   Database.CDBI.Connection.SQLTypeString
   (\links -> Database.CDBI.Description.sqlString links)
   (\links -> Database.CDBI.Description.fromStringOrNull links)

--- The description of the database column `Comments` of the `ModDescr` entity.
modDescrCommentsColDesc :: Database.CDBI.Description.ColumnDescription String
modDescrCommentsColDesc =
  Database.CDBI.Description.ColDesc "\"ModDescr\".\"Comments\""
   Database.CDBI.Connection.SQLTypeString
   (\comments -> Database.CDBI.Description.sqlString comments)
   (\comments -> Database.CDBI.Description.fromStringOrNull comments)

--- The description of the database column `ModDataDataDescKey` of the `ModDescr` entity.
modDescrModDataDataDescKeyColDesc
  :: Database.CDBI.Description.ColumnDescription ModDataID
modDescrModDataDataDescKeyColDesc =
  Database.CDBI.Description.ColDesc "\"ModDescr\".\"ModDataDataDescKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(ModDataID modDataDataDescKey) ->
     Database.CDBI.Connection.SQLInt modDataDataDescKey)
   (\(Database.CDBI.Connection.SQLInt modDataDataDescKey) ->
     ModDataID modDataDataDescKey)

--- Gets the attribute `Key` of the `ModDescr` entity.
modDescrKey :: ModDescr -> ModDescrID
modDescrKey (ModDescr a _ _ _ _ _ _ _ _ _ _ _ _) = a

--- Gets the attribute `Language` of the `ModDescr` entity.
modDescrLanguage :: ModDescr -> String
modDescrLanguage (ModDescr _ a _ _ _ _ _ _ _ _ _ _ _) = a

--- Gets the attribute `ShortDesc` of the `ModDescr` entity.
modDescrShortDesc :: ModDescr -> String
modDescrShortDesc (ModDescr _ _ a _ _ _ _ _ _ _ _ _ _) = a

--- Gets the attribute `Objectives` of the `ModDescr` entity.
modDescrObjectives :: ModDescr -> String
modDescrObjectives (ModDescr _ _ _ a _ _ _ _ _ _ _ _ _) = a

--- Gets the attribute `Contents` of the `ModDescr` entity.
modDescrContents :: ModDescr -> String
modDescrContents (ModDescr _ _ _ _ a _ _ _ _ _ _ _ _) = a

--- Gets the attribute `Prereq` of the `ModDescr` entity.
modDescrPrereq :: ModDescr -> String
modDescrPrereq (ModDescr _ _ _ _ _ a _ _ _ _ _ _ _) = a

--- Gets the attribute `Exam` of the `ModDescr` entity.
modDescrExam :: ModDescr -> String
modDescrExam (ModDescr _ _ _ _ _ _ a _ _ _ _ _ _) = a

--- Gets the attribute `Methods` of the `ModDescr` entity.
modDescrMethods :: ModDescr -> String
modDescrMethods (ModDescr _ _ _ _ _ _ _ a _ _ _ _ _) = a

--- Gets the attribute `Use` of the `ModDescr` entity.
modDescrUse :: ModDescr -> String
modDescrUse (ModDescr _ _ _ _ _ _ _ _ a _ _ _ _) = a

--- Gets the attribute `Literature` of the `ModDescr` entity.
modDescrLiterature :: ModDescr -> String
modDescrLiterature (ModDescr _ _ _ _ _ _ _ _ _ a _ _ _) = a

--- Gets the attribute `Links` of the `ModDescr` entity.
modDescrLinks :: ModDescr -> String
modDescrLinks (ModDescr _ _ _ _ _ _ _ _ _ _ a _ _) = a

--- Gets the attribute `Comments` of the `ModDescr` entity.
modDescrComments :: ModDescr -> String
modDescrComments (ModDescr _ _ _ _ _ _ _ _ _ _ _ a _) = a

--- Gets the attribute `ModDataDataDescKey` of the `ModDescr` entity.
modDescrModDataDataDescKey :: ModDescr -> ModDataID
modDescrModDataDataDescKey (ModDescr _ _ _ _ _ _ _ _ _ _ _ _ a) = a

--- Sets the attribute `Key` of the `ModDescr` entity.
setModDescrKey :: ModDescr -> ModDescrID -> ModDescr
setModDescrKey (ModDescr _ b12 b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1) a =
  ModDescr a b12 b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1

--- Sets the attribute `Language` of the `ModDescr` entity.
setModDescrLanguage :: ModDescr -> String -> ModDescr
setModDescrLanguage (ModDescr a2 _ b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1) a =
  ModDescr a2 a b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1

--- Sets the attribute `ShortDesc` of the `ModDescr` entity.
setModDescrShortDesc :: ModDescr -> String -> ModDescr
setModDescrShortDesc (ModDescr a3 a2 _ b10 b9 b8 b7 b6 b5 b4 b3 b2 b1) a =
  ModDescr a3 a2 a b10 b9 b8 b7 b6 b5 b4 b3 b2 b1

--- Sets the attribute `Objectives` of the `ModDescr` entity.
setModDescrObjectives :: ModDescr -> String -> ModDescr
setModDescrObjectives (ModDescr a4 a3 a2 _ b9 b8 b7 b6 b5 b4 b3 b2 b1) a =
  ModDescr a4 a3 a2 a b9 b8 b7 b6 b5 b4 b3 b2 b1

--- Sets the attribute `Contents` of the `ModDescr` entity.
setModDescrContents :: ModDescr -> String -> ModDescr
setModDescrContents (ModDescr a5 a4 a3 a2 _ b8 b7 b6 b5 b4 b3 b2 b1) a =
  ModDescr a5 a4 a3 a2 a b8 b7 b6 b5 b4 b3 b2 b1

--- Sets the attribute `Prereq` of the `ModDescr` entity.
setModDescrPrereq :: ModDescr -> String -> ModDescr
setModDescrPrereq (ModDescr a6 a5 a4 a3 a2 _ b7 b6 b5 b4 b3 b2 b1) a =
  ModDescr a6 a5 a4 a3 a2 a b7 b6 b5 b4 b3 b2 b1

--- Sets the attribute `Exam` of the `ModDescr` entity.
setModDescrExam :: ModDescr -> String -> ModDescr
setModDescrExam (ModDescr a7 a6 a5 a4 a3 a2 _ b6 b5 b4 b3 b2 b1) a =
  ModDescr a7 a6 a5 a4 a3 a2 a b6 b5 b4 b3 b2 b1

--- Sets the attribute `Methods` of the `ModDescr` entity.
setModDescrMethods :: ModDescr -> String -> ModDescr
setModDescrMethods (ModDescr a8 a7 a6 a5 a4 a3 a2 _ b5 b4 b3 b2 b1) a =
  ModDescr a8 a7 a6 a5 a4 a3 a2 a b5 b4 b3 b2 b1

--- Sets the attribute `Use` of the `ModDescr` entity.
setModDescrUse :: ModDescr -> String -> ModDescr
setModDescrUse (ModDescr a9 a8 a7 a6 a5 a4 a3 a2 _ b4 b3 b2 b1) a =
  ModDescr a9 a8 a7 a6 a5 a4 a3 a2 a b4 b3 b2 b1

--- Sets the attribute `Literature` of the `ModDescr` entity.
setModDescrLiterature :: ModDescr -> String -> ModDescr
setModDescrLiterature (ModDescr a10 a9 a8 a7 a6 a5 a4 a3 a2 _ b3 b2 b1) a =
  ModDescr a10 a9 a8 a7 a6 a5 a4 a3 a2 a b3 b2 b1

--- Sets the attribute `Links` of the `ModDescr` entity.
setModDescrLinks :: ModDescr -> String -> ModDescr
setModDescrLinks (ModDescr a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 _ b2 b1) a =
  ModDescr a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a b2 b1

--- Sets the attribute `Comments` of the `ModDescr` entity.
setModDescrComments :: ModDescr -> String -> ModDescr
setModDescrComments (ModDescr a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 _ b1) a =
  ModDescr a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a b1

--- Sets the attribute `ModDataDataDescKey` of the `ModDescr` entity.
setModDescrModDataDataDescKey :: ModDescr -> ModDataID -> ModDescr
setModDescrModDataDataDescKey
    (ModDescr a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 _) a =
  ModDescr a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a

--- id-to-value function for entity `ModDescr`.
modDescrID :: ModDescrID -> Database.CDBI.Criteria.Value ModDescrID
modDescrID (ModDescrID key) = Database.CDBI.Criteria.idVal key

--- id-to-int function for entity `ModDescr`.
modDescrKeyToInt :: ModDescrID -> Int
modDescrKeyToInt (ModDescrID key) = key

--- Shows the key of a `ModDescr` entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showModDescrKey :: ModDescr -> String
showModDescrKey entry =
  Database.CDBI.ER.showDatabaseKey "ModDescr" modDescrKeyToInt
   (modDescrKey entry)

--- Transforms a string into a key of a `ModDescr` entity.
--- Nothing is returned if the string does not represent a meaningful key.
readModDescrKey :: String -> Maybe ModDescrID
readModDescrKey = Database.CDBI.ER.readDatabaseKey "ModDescr" ModDescrID

--- Gets all `ModDescr` entities.
queryAllModDescrs :: Database.CDBI.Connection.DBAction [ModDescr]
queryAllModDescrs = Database.CDBI.ER.getAllEntries modDescr_CDBI_Description

--- Gets all `ModDescr` entities satisfying a given predicate.
queryCondModDescr
  :: (ModDescr -> Bool) -> Database.CDBI.Connection.DBAction [ModDescr]
queryCondModDescr = Database.CDBI.ER.getCondEntries modDescr_CDBI_Description

--- Gets a `ModDescr` entry by a given key.
getModDescr :: ModDescrID -> Database.CDBI.Connection.DBAction ModDescr
getModDescr =
  Database.CDBI.ER.getEntryWithKey modDescr_CDBI_Description modDescrColumnKey
   modDescrID

--- Inserts a new `ModDescr` entity.
newModDescrWithModDataDataDescKey
  :: String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> String -> String -> ModDataID -> Database.CDBI.Connection.DBAction ModDescr
newModDescrWithModDataDataDescKey
    language_p
    shortDesc_p
    objectives_p
    contents_p
    prereq_p
    exam_p
    methods_p
    use_p
    literature_p
    links_p
    comments_p
    modDataDataDescKey_p =
  Database.CDBI.ER.insertNewEntry modDescr_CDBI_Description setModDescrKey
   ModDescrID
   (ModDescr (ModDescrID 0) language_p shortDesc_p objectives_p contents_p
     prereq_p
     exam_p
     methods_p
     use_p
     literature_p
     links_p
     comments_p
     modDataDataDescKey_p)

--- Deletes an existing `ModDescr` entry by its key.
deleteModDescr :: ModDescr -> Database.CDBI.Connection.DBAction ()
deleteModDescr =
  Database.CDBI.ER.deleteEntry modDescr_CDBI_Description modDescrColumnKey
   (modDescrID . modDescrKey)

--- Updates an existing `ModDescr` entry by its key.
updateModDescr :: ModDescr -> Database.CDBI.Connection.DBAction ()
updateModDescr = Database.CDBI.ER.updateEntry modDescr_CDBI_Description

--- The ER description of the `ModInst` entity.
modInst_CDBI_Description :: Database.CDBI.Description.EntityDescription ModInst
modInst_CDBI_Description =
  Database.CDBI.Description.ED "ModInst"
   [Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeInt]
   (\(ModInst
       (ModInstID key)
       term
       year
       (UserID userLecturerModsKey)
       (ModDataID modDataModuleInstancesKey)) ->
     [Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString term
     ,Database.CDBI.Connection.SQLInt year
     ,Database.CDBI.Connection.SQLInt userLecturerModsKey
     ,Database.CDBI.Connection.SQLInt modDataModuleInstancesKey])
   (\(ModInst
       _
       term
       year
       (UserID userLecturerModsKey)
       (ModDataID modDataModuleInstancesKey)) ->
     [Database.CDBI.Connection.SQLNull
     ,Database.CDBI.Connection.SQLString term
     ,Database.CDBI.Connection.SQLInt year
     ,Database.CDBI.Connection.SQLInt userLecturerModsKey
     ,Database.CDBI.Connection.SQLInt modDataModuleInstancesKey])
   (\[Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString term
     ,Database.CDBI.Connection.SQLInt year
     ,Database.CDBI.Connection.SQLInt userLecturerModsKey
     ,Database.CDBI.Connection.SQLInt modDataModuleInstancesKey] ->
     ModInst (ModInstID key) term year (UserID userLecturerModsKey)
      (ModDataID modDataModuleInstancesKey))

--- The database table of the `ModInst` entity.
modInstTable :: Database.CDBI.Description.Table
modInstTable = "ModInst"

--- The database column `Key` of the `ModInst` entity.
modInstColumnKey :: Database.CDBI.Description.Column ModInstID
modInstColumnKey =
  Database.CDBI.Description.Column "\"Key\"" "\"ModInst\".\"Key\""

--- The database column `Term` of the `ModInst` entity.
modInstColumnTerm :: Database.CDBI.Description.Column String
modInstColumnTerm =
  Database.CDBI.Description.Column "\"Term\"" "\"ModInst\".\"Term\""

--- The database column `Year` of the `ModInst` entity.
modInstColumnYear :: Database.CDBI.Description.Column Int
modInstColumnYear =
  Database.CDBI.Description.Column "\"Year\"" "\"ModInst\".\"Year\""

--- The database column `UserLecturerModsKey` of the `ModInst` entity.
modInstColumnUserLecturerModsKey :: Database.CDBI.Description.Column UserID
modInstColumnUserLecturerModsKey =
  Database.CDBI.Description.Column "\"UserLecturerModsKey\""
   "\"ModInst\".\"UserLecturerModsKey\""

--- The database column `ModDataModuleInstancesKey` of the `ModInst` entity.
modInstColumnModDataModuleInstancesKey
  :: Database.CDBI.Description.Column ModDataID
modInstColumnModDataModuleInstancesKey =
  Database.CDBI.Description.Column "\"ModDataModuleInstancesKey\""
   "\"ModInst\".\"ModDataModuleInstancesKey\""

--- The description of the database column `Key` of the `ModInst` entity.
modInstKeyColDesc :: Database.CDBI.Description.ColumnDescription ModInstID
modInstKeyColDesc =
  Database.CDBI.Description.ColDesc "\"ModInst\".\"Key\""
   Database.CDBI.Connection.SQLTypeInt
   (\(ModInstID key) -> Database.CDBI.Connection.SQLInt key)
   (\(Database.CDBI.Connection.SQLInt key) -> ModInstID key)

--- The description of the database column `Term` of the `ModInst` entity.
modInstTermColDesc :: Database.CDBI.Description.ColumnDescription String
modInstTermColDesc =
  Database.CDBI.Description.ColDesc "\"ModInst\".\"Term\""
   Database.CDBI.Connection.SQLTypeString
   (\term -> Database.CDBI.Connection.SQLString term)
   (\(Database.CDBI.Connection.SQLString term) -> term)

--- The description of the database column `Year` of the `ModInst` entity.
modInstYearColDesc :: Database.CDBI.Description.ColumnDescription Int
modInstYearColDesc =
  Database.CDBI.Description.ColDesc "\"ModInst\".\"Year\""
   Database.CDBI.Connection.SQLTypeInt
   (\year -> Database.CDBI.Connection.SQLInt year)
   (\(Database.CDBI.Connection.SQLInt year) -> year)

--- The description of the database column `UserLecturerModsKey` of the `ModInst` entity.
modInstUserLecturerModsKeyColDesc
  :: Database.CDBI.Description.ColumnDescription UserID
modInstUserLecturerModsKeyColDesc =
  Database.CDBI.Description.ColDesc "\"ModInst\".\"UserLecturerModsKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(UserID userLecturerModsKey) ->
     Database.CDBI.Connection.SQLInt userLecturerModsKey)
   (\(Database.CDBI.Connection.SQLInt userLecturerModsKey) ->
     UserID userLecturerModsKey)

--- The description of the database column `ModDataModuleInstancesKey` of the `ModInst` entity.
modInstModDataModuleInstancesKeyColDesc
  :: Database.CDBI.Description.ColumnDescription ModDataID
modInstModDataModuleInstancesKeyColDesc =
  Database.CDBI.Description.ColDesc "\"ModInst\".\"ModDataModuleInstancesKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(ModDataID modDataModuleInstancesKey) ->
     Database.CDBI.Connection.SQLInt modDataModuleInstancesKey)
   (\(Database.CDBI.Connection.SQLInt modDataModuleInstancesKey) ->
     ModDataID modDataModuleInstancesKey)

--- Gets the attribute `Key` of the `ModInst` entity.
modInstKey :: ModInst -> ModInstID
modInstKey (ModInst a _ _ _ _) = a

--- Gets the attribute `Term` of the `ModInst` entity.
modInstTerm :: ModInst -> String
modInstTerm (ModInst _ a _ _ _) = a

--- Gets the attribute `Year` of the `ModInst` entity.
modInstYear :: ModInst -> Int
modInstYear (ModInst _ _ a _ _) = a

--- Gets the attribute `UserLecturerModsKey` of the `ModInst` entity.
modInstUserLecturerModsKey :: ModInst -> UserID
modInstUserLecturerModsKey (ModInst _ _ _ a _) = a

--- Gets the attribute `ModDataModuleInstancesKey` of the `ModInst` entity.
modInstModDataModuleInstancesKey :: ModInst -> ModDataID
modInstModDataModuleInstancesKey (ModInst _ _ _ _ a) = a

--- Sets the attribute `Key` of the `ModInst` entity.
setModInstKey :: ModInst -> ModInstID -> ModInst
setModInstKey (ModInst _ b4 b3 b2 b1) a = ModInst a b4 b3 b2 b1

--- Sets the attribute `Term` of the `ModInst` entity.
setModInstTerm :: ModInst -> String -> ModInst
setModInstTerm (ModInst a2 _ b3 b2 b1) a = ModInst a2 a b3 b2 b1

--- Sets the attribute `Year` of the `ModInst` entity.
setModInstYear :: ModInst -> Int -> ModInst
setModInstYear (ModInst a3 a2 _ b2 b1) a = ModInst a3 a2 a b2 b1

--- Sets the attribute `UserLecturerModsKey` of the `ModInst` entity.
setModInstUserLecturerModsKey :: ModInst -> UserID -> ModInst
setModInstUserLecturerModsKey (ModInst a4 a3 a2 _ b1) a = ModInst a4 a3 a2 a b1

--- Sets the attribute `ModDataModuleInstancesKey` of the `ModInst` entity.
setModInstModDataModuleInstancesKey :: ModInst -> ModDataID -> ModInst
setModInstModDataModuleInstancesKey (ModInst a5 a4 a3 a2 _) a =
  ModInst a5 a4 a3 a2 a

--- id-to-value function for entity `ModInst`.
modInstID :: ModInstID -> Database.CDBI.Criteria.Value ModInstID
modInstID (ModInstID key) = Database.CDBI.Criteria.idVal key

--- id-to-int function for entity `ModInst`.
modInstKeyToInt :: ModInstID -> Int
modInstKeyToInt (ModInstID key) = key

--- Shows the key of a `ModInst` entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showModInstKey :: ModInst -> String
showModInstKey entry =
  Database.CDBI.ER.showDatabaseKey "ModInst" modInstKeyToInt (modInstKey entry)

--- Transforms a string into a key of a `ModInst` entity.
--- Nothing is returned if the string does not represent a meaningful key.
readModInstKey :: String -> Maybe ModInstID
readModInstKey = Database.CDBI.ER.readDatabaseKey "ModInst" ModInstID

--- Gets all `ModInst` entities.
queryAllModInsts :: Database.CDBI.Connection.DBAction [ModInst]
queryAllModInsts = Database.CDBI.ER.getAllEntries modInst_CDBI_Description

--- Gets all `ModInst` entities satisfying a given predicate.
queryCondModInst
  :: (ModInst -> Bool) -> Database.CDBI.Connection.DBAction [ModInst]
queryCondModInst = Database.CDBI.ER.getCondEntries modInst_CDBI_Description

--- Gets a `ModInst` entry by a given key.
getModInst :: ModInstID -> Database.CDBI.Connection.DBAction ModInst
getModInst =
  Database.CDBI.ER.getEntryWithKey modInst_CDBI_Description modInstColumnKey
   modInstID

--- Inserts a new `ModInst` entity.
newModInstWithUserLecturerModsKeyWithModDataModuleInstancesKey
  :: String
  -> Maybe Int
  -> UserID -> ModDataID -> Database.CDBI.Connection.DBAction ModInst
newModInstWithUserLecturerModsKeyWithModDataModuleInstancesKey
    term_p year_p userLecturerModsKey_p modDataModuleInstancesKey_p =
  Database.CDBI.ER.insertNewEntry modInst_CDBI_Description setModInstKey
   ModInstID
   (ModInst (ModInstID 0) term_p (maybe 2019 id year_p) userLecturerModsKey_p
     modDataModuleInstancesKey_p)

--- Deletes an existing `ModInst` entry by its key.
deleteModInst :: ModInst -> Database.CDBI.Connection.DBAction ()
deleteModInst =
  Database.CDBI.ER.deleteEntry modInst_CDBI_Description modInstColumnKey
   (modInstID . modInstKey)

--- Updates an existing `ModInst` entry by its key.
updateModInst :: ModInst -> Database.CDBI.Connection.DBAction ()
updateModInst = Database.CDBI.ER.updateEntry modInst_CDBI_Description

--- The ER description of the `AdvisorStudyProgram` entity.
advisorStudyProgram_CDBI_Description
  :: Database.CDBI.Description.EntityDescription AdvisorStudyProgram
advisorStudyProgram_CDBI_Description =
  Database.CDBI.Description.ED "AdvisorStudyProgram"
   [Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeBool
   ,Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeInt]
   (\(AdvisorStudyProgram
       (AdvisorStudyProgramID key)
       name
       term
       year
       desc
       prereq
       comments
       visible
       (UserID userStudyAdvisingKey)
       (StudyProgramID studyProgramStudyProgramsAdvisedKey)) ->
     [Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString name
     ,Database.CDBI.Connection.SQLString term
     ,Database.CDBI.Connection.SQLInt year
     ,Database.CDBI.Description.sqlString desc
     ,Database.CDBI.Description.sqlString prereq
     ,Database.CDBI.Description.sqlString comments
     ,Database.CDBI.Connection.SQLBool visible
     ,Database.CDBI.Connection.SQLInt userStudyAdvisingKey
     ,Database.CDBI.Connection.SQLInt studyProgramStudyProgramsAdvisedKey])
   (\(AdvisorStudyProgram
       _
       name
       term
       year
       desc
       prereq
       comments
       visible
       (UserID userStudyAdvisingKey)
       (StudyProgramID studyProgramStudyProgramsAdvisedKey)) ->
     [Database.CDBI.Connection.SQLNull
     ,Database.CDBI.Connection.SQLString name
     ,Database.CDBI.Connection.SQLString term
     ,Database.CDBI.Connection.SQLInt year
     ,Database.CDBI.Description.sqlString desc
     ,Database.CDBI.Description.sqlString prereq
     ,Database.CDBI.Description.sqlString comments
     ,Database.CDBI.Connection.SQLBool visible
     ,Database.CDBI.Connection.SQLInt userStudyAdvisingKey
     ,Database.CDBI.Connection.SQLInt studyProgramStudyProgramsAdvisedKey])
   (\[Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString name
     ,Database.CDBI.Connection.SQLString term
     ,Database.CDBI.Connection.SQLInt year
     ,desc
     ,prereq
     ,comments
     ,Database.CDBI.Connection.SQLBool visible
     ,Database.CDBI.Connection.SQLInt userStudyAdvisingKey
     ,Database.CDBI.Connection.SQLInt studyProgramStudyProgramsAdvisedKey] ->
     AdvisorStudyProgram (AdvisorStudyProgramID key) name term year
      (Database.CDBI.Description.fromStringOrNull desc)
      (Database.CDBI.Description.fromStringOrNull prereq)
      (Database.CDBI.Description.fromStringOrNull comments)
      visible
      (UserID userStudyAdvisingKey)
      (StudyProgramID studyProgramStudyProgramsAdvisedKey))

--- The database table of the `AdvisorStudyProgram` entity.
advisorStudyProgramTable :: Database.CDBI.Description.Table
advisorStudyProgramTable = "AdvisorStudyProgram"

--- The database column `Key` of the `AdvisorStudyProgram` entity.
advisorStudyProgramColumnKey
  :: Database.CDBI.Description.Column AdvisorStudyProgramID
advisorStudyProgramColumnKey =
  Database.CDBI.Description.Column "\"Key\"" "\"AdvisorStudyProgram\".\"Key\""

--- The database column `Name` of the `AdvisorStudyProgram` entity.
advisorStudyProgramColumnName :: Database.CDBI.Description.Column String
advisorStudyProgramColumnName =
  Database.CDBI.Description.Column "\"Name\"" "\"AdvisorStudyProgram\".\"Name\""

--- The database column `Term` of the `AdvisorStudyProgram` entity.
advisorStudyProgramColumnTerm :: Database.CDBI.Description.Column String
advisorStudyProgramColumnTerm =
  Database.CDBI.Description.Column "\"Term\"" "\"AdvisorStudyProgram\".\"Term\""

--- The database column `Year` of the `AdvisorStudyProgram` entity.
advisorStudyProgramColumnYear :: Database.CDBI.Description.Column Int
advisorStudyProgramColumnYear =
  Database.CDBI.Description.Column "\"Year\"" "\"AdvisorStudyProgram\".\"Year\""

--- The database column `Desc` of the `AdvisorStudyProgram` entity.
advisorStudyProgramColumnDesc :: Database.CDBI.Description.Column String
advisorStudyProgramColumnDesc =
  Database.CDBI.Description.Column "\"Desc\"" "\"AdvisorStudyProgram\".\"Desc\""

--- The database column `Prereq` of the `AdvisorStudyProgram` entity.
advisorStudyProgramColumnPrereq :: Database.CDBI.Description.Column String
advisorStudyProgramColumnPrereq =
  Database.CDBI.Description.Column "\"Prereq\""
   "\"AdvisorStudyProgram\".\"Prereq\""

--- The database column `Comments` of the `AdvisorStudyProgram` entity.
advisorStudyProgramColumnComments :: Database.CDBI.Description.Column String
advisorStudyProgramColumnComments =
  Database.CDBI.Description.Column "\"Comments\""
   "\"AdvisorStudyProgram\".\"Comments\""

--- The database column `Visible` of the `AdvisorStudyProgram` entity.
advisorStudyProgramColumnVisible :: Database.CDBI.Description.Column Bool
advisorStudyProgramColumnVisible =
  Database.CDBI.Description.Column "\"Visible\""
   "\"AdvisorStudyProgram\".\"Visible\""

--- The database column `UserStudyAdvisingKey` of the `AdvisorStudyProgram` entity.
advisorStudyProgramColumnUserStudyAdvisingKey
  :: Database.CDBI.Description.Column UserID
advisorStudyProgramColumnUserStudyAdvisingKey =
  Database.CDBI.Description.Column "\"UserStudyAdvisingKey\""
   "\"AdvisorStudyProgram\".\"UserStudyAdvisingKey\""

--- The database column `StudyProgramStudyProgramsAdvisedKey` of the `AdvisorStudyProgram` entity.
advisorStudyProgramColumnStudyProgramStudyProgramsAdvisedKey
  :: Database.CDBI.Description.Column StudyProgramID
advisorStudyProgramColumnStudyProgramStudyProgramsAdvisedKey =
  Database.CDBI.Description.Column "\"StudyProgramStudyProgramsAdvisedKey\""
   "\"AdvisorStudyProgram\".\"StudyProgramStudyProgramsAdvisedKey\""

--- The description of the database column `Key` of the `AdvisorStudyProgram` entity.
advisorStudyProgramKeyColDesc
  :: Database.CDBI.Description.ColumnDescription AdvisorStudyProgramID
advisorStudyProgramKeyColDesc =
  Database.CDBI.Description.ColDesc "\"AdvisorStudyProgram\".\"Key\""
   Database.CDBI.Connection.SQLTypeInt
   (\(AdvisorStudyProgramID key) -> Database.CDBI.Connection.SQLInt key)
   (\(Database.CDBI.Connection.SQLInt key) -> AdvisorStudyProgramID key)

--- The description of the database column `Name` of the `AdvisorStudyProgram` entity.
advisorStudyProgramNameColDesc
  :: Database.CDBI.Description.ColumnDescription String
advisorStudyProgramNameColDesc =
  Database.CDBI.Description.ColDesc "\"AdvisorStudyProgram\".\"Name\""
   Database.CDBI.Connection.SQLTypeString
   (\name -> Database.CDBI.Connection.SQLString name)
   (\(Database.CDBI.Connection.SQLString name) -> name)

--- The description of the database column `Term` of the `AdvisorStudyProgram` entity.
advisorStudyProgramTermColDesc
  :: Database.CDBI.Description.ColumnDescription String
advisorStudyProgramTermColDesc =
  Database.CDBI.Description.ColDesc "\"AdvisorStudyProgram\".\"Term\""
   Database.CDBI.Connection.SQLTypeString
   (\term -> Database.CDBI.Connection.SQLString term)
   (\(Database.CDBI.Connection.SQLString term) -> term)

--- The description of the database column `Year` of the `AdvisorStudyProgram` entity.
advisorStudyProgramYearColDesc
  :: Database.CDBI.Description.ColumnDescription Int
advisorStudyProgramYearColDesc =
  Database.CDBI.Description.ColDesc "\"AdvisorStudyProgram\".\"Year\""
   Database.CDBI.Connection.SQLTypeInt
   (\year -> Database.CDBI.Connection.SQLInt year)
   (\(Database.CDBI.Connection.SQLInt year) -> year)

--- The description of the database column `Desc` of the `AdvisorStudyProgram` entity.
advisorStudyProgramDescColDesc
  :: Database.CDBI.Description.ColumnDescription String
advisorStudyProgramDescColDesc =
  Database.CDBI.Description.ColDesc "\"AdvisorStudyProgram\".\"Desc\""
   Database.CDBI.Connection.SQLTypeString
   (\desc -> Database.CDBI.Description.sqlString desc)
   (\desc -> Database.CDBI.Description.fromStringOrNull desc)

--- The description of the database column `Prereq` of the `AdvisorStudyProgram` entity.
advisorStudyProgramPrereqColDesc
  :: Database.CDBI.Description.ColumnDescription String
advisorStudyProgramPrereqColDesc =
  Database.CDBI.Description.ColDesc "\"AdvisorStudyProgram\".\"Prereq\""
   Database.CDBI.Connection.SQLTypeString
   (\prereq -> Database.CDBI.Description.sqlString prereq)
   (\prereq -> Database.CDBI.Description.fromStringOrNull prereq)

--- The description of the database column `Comments` of the `AdvisorStudyProgram` entity.
advisorStudyProgramCommentsColDesc
  :: Database.CDBI.Description.ColumnDescription String
advisorStudyProgramCommentsColDesc =
  Database.CDBI.Description.ColDesc "\"AdvisorStudyProgram\".\"Comments\""
   Database.CDBI.Connection.SQLTypeString
   (\comments -> Database.CDBI.Description.sqlString comments)
   (\comments -> Database.CDBI.Description.fromStringOrNull comments)

--- The description of the database column `Visible` of the `AdvisorStudyProgram` entity.
advisorStudyProgramVisibleColDesc
  :: Database.CDBI.Description.ColumnDescription Bool
advisorStudyProgramVisibleColDesc =
  Database.CDBI.Description.ColDesc "\"AdvisorStudyProgram\".\"Visible\""
   Database.CDBI.Connection.SQLTypeBool
   (\visible -> Database.CDBI.Connection.SQLBool visible)
   (\(Database.CDBI.Connection.SQLBool visible) -> visible)

--- The description of the database column `UserStudyAdvisingKey` of the `AdvisorStudyProgram` entity.
advisorStudyProgramUserStudyAdvisingKeyColDesc
  :: Database.CDBI.Description.ColumnDescription UserID
advisorStudyProgramUserStudyAdvisingKeyColDesc =
  Database.CDBI.Description.ColDesc
   "\"AdvisorStudyProgram\".\"UserStudyAdvisingKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(UserID userStudyAdvisingKey) ->
     Database.CDBI.Connection.SQLInt userStudyAdvisingKey)
   (\(Database.CDBI.Connection.SQLInt userStudyAdvisingKey) ->
     UserID userStudyAdvisingKey)

--- The description of the database column `StudyProgramStudyProgramsAdvisedKey` of the `AdvisorStudyProgram` entity.
advisorStudyProgramStudyProgramStudyProgramsAdvisedKeyColDesc
  :: Database.CDBI.Description.ColumnDescription StudyProgramID
advisorStudyProgramStudyProgramStudyProgramsAdvisedKeyColDesc =
  Database.CDBI.Description.ColDesc
   "\"AdvisorStudyProgram\".\"StudyProgramStudyProgramsAdvisedKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(StudyProgramID studyProgramStudyProgramsAdvisedKey) ->
     Database.CDBI.Connection.SQLInt studyProgramStudyProgramsAdvisedKey)
   (\(Database.CDBI.Connection.SQLInt studyProgramStudyProgramsAdvisedKey) ->
     StudyProgramID studyProgramStudyProgramsAdvisedKey)

--- Gets the attribute `Key` of the `AdvisorStudyProgram` entity.
advisorStudyProgramKey :: AdvisorStudyProgram -> AdvisorStudyProgramID
advisorStudyProgramKey (AdvisorStudyProgram a _ _ _ _ _ _ _ _ _) = a

--- Gets the attribute `Name` of the `AdvisorStudyProgram` entity.
advisorStudyProgramName :: AdvisorStudyProgram -> String
advisorStudyProgramName (AdvisorStudyProgram _ a _ _ _ _ _ _ _ _) = a

--- Gets the attribute `Term` of the `AdvisorStudyProgram` entity.
advisorStudyProgramTerm :: AdvisorStudyProgram -> String
advisorStudyProgramTerm (AdvisorStudyProgram _ _ a _ _ _ _ _ _ _) = a

--- Gets the attribute `Year` of the `AdvisorStudyProgram` entity.
advisorStudyProgramYear :: AdvisorStudyProgram -> Int
advisorStudyProgramYear (AdvisorStudyProgram _ _ _ a _ _ _ _ _ _) = a

--- Gets the attribute `Desc` of the `AdvisorStudyProgram` entity.
advisorStudyProgramDesc :: AdvisorStudyProgram -> String
advisorStudyProgramDesc (AdvisorStudyProgram _ _ _ _ a _ _ _ _ _) = a

--- Gets the attribute `Prereq` of the `AdvisorStudyProgram` entity.
advisorStudyProgramPrereq :: AdvisorStudyProgram -> String
advisorStudyProgramPrereq (AdvisorStudyProgram _ _ _ _ _ a _ _ _ _) = a

--- Gets the attribute `Comments` of the `AdvisorStudyProgram` entity.
advisorStudyProgramComments :: AdvisorStudyProgram -> String
advisorStudyProgramComments (AdvisorStudyProgram _ _ _ _ _ _ a _ _ _) = a

--- Gets the attribute `Visible` of the `AdvisorStudyProgram` entity.
advisorStudyProgramVisible :: AdvisorStudyProgram -> Bool
advisorStudyProgramVisible (AdvisorStudyProgram _ _ _ _ _ _ _ a _ _) = a

--- Gets the attribute `UserStudyAdvisingKey` of the `AdvisorStudyProgram` entity.
advisorStudyProgramUserStudyAdvisingKey :: AdvisorStudyProgram -> UserID
advisorStudyProgramUserStudyAdvisingKey
    (AdvisorStudyProgram _ _ _ _ _ _ _ _ a _) =
  a

--- Gets the attribute `StudyProgramStudyProgramsAdvisedKey` of the `AdvisorStudyProgram` entity.
advisorStudyProgramStudyProgramStudyProgramsAdvisedKey
  :: AdvisorStudyProgram -> StudyProgramID
advisorStudyProgramStudyProgramStudyProgramsAdvisedKey
    (AdvisorStudyProgram _ _ _ _ _ _ _ _ _ a) =
  a

--- Sets the attribute `Key` of the `AdvisorStudyProgram` entity.
setAdvisorStudyProgramKey
  :: AdvisorStudyProgram -> AdvisorStudyProgramID -> AdvisorStudyProgram
setAdvisorStudyProgramKey (AdvisorStudyProgram _ b9 b8 b7 b6 b5 b4 b3 b2 b1) a =
  AdvisorStudyProgram a b9 b8 b7 b6 b5 b4 b3 b2 b1

--- Sets the attribute `Name` of the `AdvisorStudyProgram` entity.
setAdvisorStudyProgramName
  :: AdvisorStudyProgram -> String -> AdvisorStudyProgram
setAdvisorStudyProgramName
    (AdvisorStudyProgram a2 _ b8 b7 b6 b5 b4 b3 b2 b1) a =
  AdvisorStudyProgram a2 a b8 b7 b6 b5 b4 b3 b2 b1

--- Sets the attribute `Term` of the `AdvisorStudyProgram` entity.
setAdvisorStudyProgramTerm
  :: AdvisorStudyProgram -> String -> AdvisorStudyProgram
setAdvisorStudyProgramTerm
    (AdvisorStudyProgram a3 a2 _ b7 b6 b5 b4 b3 b2 b1) a =
  AdvisorStudyProgram a3 a2 a b7 b6 b5 b4 b3 b2 b1

--- Sets the attribute `Year` of the `AdvisorStudyProgram` entity.
setAdvisorStudyProgramYear :: AdvisorStudyProgram -> Int -> AdvisorStudyProgram
setAdvisorStudyProgramYear
    (AdvisorStudyProgram a4 a3 a2 _ b6 b5 b4 b3 b2 b1) a =
  AdvisorStudyProgram a4 a3 a2 a b6 b5 b4 b3 b2 b1

--- Sets the attribute `Desc` of the `AdvisorStudyProgram` entity.
setAdvisorStudyProgramDesc
  :: AdvisorStudyProgram -> String -> AdvisorStudyProgram
setAdvisorStudyProgramDesc
    (AdvisorStudyProgram a5 a4 a3 a2 _ b5 b4 b3 b2 b1) a =
  AdvisorStudyProgram a5 a4 a3 a2 a b5 b4 b3 b2 b1

--- Sets the attribute `Prereq` of the `AdvisorStudyProgram` entity.
setAdvisorStudyProgramPrereq
  :: AdvisorStudyProgram -> String -> AdvisorStudyProgram
setAdvisorStudyProgramPrereq
    (AdvisorStudyProgram a6 a5 a4 a3 a2 _ b4 b3 b2 b1) a =
  AdvisorStudyProgram a6 a5 a4 a3 a2 a b4 b3 b2 b1

--- Sets the attribute `Comments` of the `AdvisorStudyProgram` entity.
setAdvisorStudyProgramComments
  :: AdvisorStudyProgram -> String -> AdvisorStudyProgram
setAdvisorStudyProgramComments
    (AdvisorStudyProgram a7 a6 a5 a4 a3 a2 _ b3 b2 b1) a =
  AdvisorStudyProgram a7 a6 a5 a4 a3 a2 a b3 b2 b1

--- Sets the attribute `Visible` of the `AdvisorStudyProgram` entity.
setAdvisorStudyProgramVisible
  :: AdvisorStudyProgram -> Bool -> AdvisorStudyProgram
setAdvisorStudyProgramVisible
    (AdvisorStudyProgram a8 a7 a6 a5 a4 a3 a2 _ b2 b1) a =
  AdvisorStudyProgram a8 a7 a6 a5 a4 a3 a2 a b2 b1

--- Sets the attribute `UserStudyAdvisingKey` of the `AdvisorStudyProgram` entity.
setAdvisorStudyProgramUserStudyAdvisingKey
  :: AdvisorStudyProgram -> UserID -> AdvisorStudyProgram
setAdvisorStudyProgramUserStudyAdvisingKey
    (AdvisorStudyProgram a9 a8 a7 a6 a5 a4 a3 a2 _ b1) a =
  AdvisorStudyProgram a9 a8 a7 a6 a5 a4 a3 a2 a b1

--- Sets the attribute `StudyProgramStudyProgramsAdvisedKey` of the `AdvisorStudyProgram` entity.
setAdvisorStudyProgramStudyProgramStudyProgramsAdvisedKey
  :: AdvisorStudyProgram -> StudyProgramID -> AdvisorStudyProgram
setAdvisorStudyProgramStudyProgramStudyProgramsAdvisedKey
    (AdvisorStudyProgram a10 a9 a8 a7 a6 a5 a4 a3 a2 _) a =
  AdvisorStudyProgram a10 a9 a8 a7 a6 a5 a4 a3 a2 a

--- id-to-value function for entity `AdvisorStudyProgram`.
advisorStudyProgramID
  :: AdvisorStudyProgramID -> Database.CDBI.Criteria.Value AdvisorStudyProgramID
advisorStudyProgramID (AdvisorStudyProgramID key) =
  Database.CDBI.Criteria.idVal key

--- id-to-int function for entity `AdvisorStudyProgram`.
advisorStudyProgramKeyToInt :: AdvisorStudyProgramID -> Int
advisorStudyProgramKeyToInt (AdvisorStudyProgramID key) = key

--- Shows the key of a `AdvisorStudyProgram` entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showAdvisorStudyProgramKey :: AdvisorStudyProgram -> String
showAdvisorStudyProgramKey entry =
  Database.CDBI.ER.showDatabaseKey "AdvisorStudyProgram"
   advisorStudyProgramKeyToInt
   (advisorStudyProgramKey entry)

--- Transforms a string into a key of a `AdvisorStudyProgram` entity.
--- Nothing is returned if the string does not represent a meaningful key.
readAdvisorStudyProgramKey :: String -> Maybe AdvisorStudyProgramID
readAdvisorStudyProgramKey =
  Database.CDBI.ER.readDatabaseKey "AdvisorStudyProgram" AdvisorStudyProgramID

--- Gets all `AdvisorStudyProgram` entities.
queryAllAdvisorStudyPrograms
  :: Database.CDBI.Connection.DBAction [AdvisorStudyProgram]
queryAllAdvisorStudyPrograms =
  Database.CDBI.ER.getAllEntries advisorStudyProgram_CDBI_Description

--- Gets all `AdvisorStudyProgram` entities satisfying a given predicate.
queryCondAdvisorStudyProgram
  :: (AdvisorStudyProgram -> Bool)
  -> Database.CDBI.Connection.DBAction [AdvisorStudyProgram]
queryCondAdvisorStudyProgram =
  Database.CDBI.ER.getCondEntries advisorStudyProgram_CDBI_Description

--- Gets a `AdvisorStudyProgram` entry by a given key.
getAdvisorStudyProgram
  :: AdvisorStudyProgramID
  -> Database.CDBI.Connection.DBAction AdvisorStudyProgram
getAdvisorStudyProgram =
  Database.CDBI.ER.getEntryWithKey advisorStudyProgram_CDBI_Description
   advisorStudyProgramColumnKey
   advisorStudyProgramID

--- Inserts a new `AdvisorStudyProgram` entity.
newAdvisorStudyProgramWithUserStudyAdvisingKeyWithStudyProgramStudyProgramsAdvisedKey
  :: String
  -> String
  -> Maybe Int
  -> String
  -> String
  -> String
  -> Bool
  -> UserID
  -> StudyProgramID -> Database.CDBI.Connection.DBAction AdvisorStudyProgram
newAdvisorStudyProgramWithUserStudyAdvisingKeyWithStudyProgramStudyProgramsAdvisedKey
    name_p
    term_p
    year_p
    desc_p
    prereq_p
    comments_p
    visible_p
    userStudyAdvisingKey_p
    studyProgramStudyProgramsAdvisedKey_p =
  Database.CDBI.ER.insertNewEntry advisorStudyProgram_CDBI_Description
   setAdvisorStudyProgramKey
   AdvisorStudyProgramID
   (AdvisorStudyProgram (AdvisorStudyProgramID 0) name_p term_p
     (maybe 2019 id year_p)
     desc_p
     prereq_p
     comments_p
     visible_p
     userStudyAdvisingKey_p
     studyProgramStudyProgramsAdvisedKey_p)

--- Deletes an existing `AdvisorStudyProgram` entry by its key.
deleteAdvisorStudyProgram
  :: AdvisorStudyProgram -> Database.CDBI.Connection.DBAction ()
deleteAdvisorStudyProgram =
  Database.CDBI.ER.deleteEntry advisorStudyProgram_CDBI_Description
   advisorStudyProgramColumnKey
   (advisorStudyProgramID . advisorStudyProgramKey)

--- Updates an existing `AdvisorStudyProgram` entry by its key.
updateAdvisorStudyProgram
  :: AdvisorStudyProgram -> Database.CDBI.Connection.DBAction ()
updateAdvisorStudyProgram =
  Database.CDBI.ER.updateEntry advisorStudyProgram_CDBI_Description

--- The ER description of the `AdvisorModule` entity.
advisorModule_CDBI_Description
  :: Database.CDBI.Description.EntityDescription AdvisorModule
advisorModule_CDBI_Description =
  Database.CDBI.Description.ED "AdvisorModule"
   [Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeBool
   ,Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeInt]
   (\(AdvisorModule
       (AdvisorModuleID key)
       mandatory
       (AdvisorStudyProgramID advisorStudyProgramAdvisorProgramModulesKey)
       (CategoryID categoryAdvisorCategorizingKey)
       (ModInstID modInstAdvisedProgramModuleInstancesKey)) ->
     [Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLBool mandatory
     ,Database.CDBI.Connection.SQLInt
       advisorStudyProgramAdvisorProgramModulesKey
     ,Database.CDBI.Connection.SQLInt categoryAdvisorCategorizingKey
     ,Database.CDBI.Connection.SQLInt modInstAdvisedProgramModuleInstancesKey])
   (\(AdvisorModule
       _
       mandatory
       (AdvisorStudyProgramID advisorStudyProgramAdvisorProgramModulesKey)
       (CategoryID categoryAdvisorCategorizingKey)
       (ModInstID modInstAdvisedProgramModuleInstancesKey)) ->
     [Database.CDBI.Connection.SQLNull
     ,Database.CDBI.Connection.SQLBool mandatory
     ,Database.CDBI.Connection.SQLInt
       advisorStudyProgramAdvisorProgramModulesKey
     ,Database.CDBI.Connection.SQLInt categoryAdvisorCategorizingKey
     ,Database.CDBI.Connection.SQLInt modInstAdvisedProgramModuleInstancesKey])
   (\[Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLBool mandatory
     ,Database.CDBI.Connection.SQLInt
       advisorStudyProgramAdvisorProgramModulesKey
     ,Database.CDBI.Connection.SQLInt categoryAdvisorCategorizingKey
     ,Database.CDBI.Connection.SQLInt
       modInstAdvisedProgramModuleInstancesKey] ->
     AdvisorModule (AdvisorModuleID key) mandatory
      (AdvisorStudyProgramID advisorStudyProgramAdvisorProgramModulesKey)
      (CategoryID categoryAdvisorCategorizingKey)
      (ModInstID modInstAdvisedProgramModuleInstancesKey))

--- The database table of the `AdvisorModule` entity.
advisorModuleTable :: Database.CDBI.Description.Table
advisorModuleTable = "AdvisorModule"

--- The database column `Key` of the `AdvisorModule` entity.
advisorModuleColumnKey :: Database.CDBI.Description.Column AdvisorModuleID
advisorModuleColumnKey =
  Database.CDBI.Description.Column "\"Key\"" "\"AdvisorModule\".\"Key\""

--- The database column `Mandatory` of the `AdvisorModule` entity.
advisorModuleColumnMandatory :: Database.CDBI.Description.Column Bool
advisorModuleColumnMandatory =
  Database.CDBI.Description.Column "\"Mandatory\""
   "\"AdvisorModule\".\"Mandatory\""

--- The database column `AdvisorStudyProgramAdvisorProgramModulesKey` of the `AdvisorModule` entity.
advisorModuleColumnAdvisorStudyProgramAdvisorProgramModulesKey
  :: Database.CDBI.Description.Column AdvisorStudyProgramID
advisorModuleColumnAdvisorStudyProgramAdvisorProgramModulesKey =
  Database.CDBI.Description.Column
   "\"AdvisorStudyProgramAdvisorProgramModulesKey\""
   "\"AdvisorModule\".\"AdvisorStudyProgramAdvisorProgramModulesKey\""

--- The database column `CategoryAdvisorCategorizingKey` of the `AdvisorModule` entity.
advisorModuleColumnCategoryAdvisorCategorizingKey
  :: Database.CDBI.Description.Column CategoryID
advisorModuleColumnCategoryAdvisorCategorizingKey =
  Database.CDBI.Description.Column "\"CategoryAdvisorCategorizingKey\""
   "\"AdvisorModule\".\"CategoryAdvisorCategorizingKey\""

--- The database column `ModInstAdvisedProgramModuleInstancesKey` of the `AdvisorModule` entity.
advisorModuleColumnModInstAdvisedProgramModuleInstancesKey
  :: Database.CDBI.Description.Column ModInstID
advisorModuleColumnModInstAdvisedProgramModuleInstancesKey =
  Database.CDBI.Description.Column "\"ModInstAdvisedProgramModuleInstancesKey\""
   "\"AdvisorModule\".\"ModInstAdvisedProgramModuleInstancesKey\""

--- The description of the database column `Key` of the `AdvisorModule` entity.
advisorModuleKeyColDesc
  :: Database.CDBI.Description.ColumnDescription AdvisorModuleID
advisorModuleKeyColDesc =
  Database.CDBI.Description.ColDesc "\"AdvisorModule\".\"Key\""
   Database.CDBI.Connection.SQLTypeInt
   (\(AdvisorModuleID key) -> Database.CDBI.Connection.SQLInt key)
   (\(Database.CDBI.Connection.SQLInt key) -> AdvisorModuleID key)

--- The description of the database column `Mandatory` of the `AdvisorModule` entity.
advisorModuleMandatoryColDesc
  :: Database.CDBI.Description.ColumnDescription Bool
advisorModuleMandatoryColDesc =
  Database.CDBI.Description.ColDesc "\"AdvisorModule\".\"Mandatory\""
   Database.CDBI.Connection.SQLTypeBool
   (\mandatory -> Database.CDBI.Connection.SQLBool mandatory)
   (\(Database.CDBI.Connection.SQLBool mandatory) -> mandatory)

--- The description of the database column `AdvisorStudyProgramAdvisorProgramModulesKey` of the `AdvisorModule` entity.
advisorModuleAdvisorStudyProgramAdvisorProgramModulesKeyColDesc
  :: Database.CDBI.Description.ColumnDescription AdvisorStudyProgramID
advisorModuleAdvisorStudyProgramAdvisorProgramModulesKeyColDesc =
  Database.CDBI.Description.ColDesc
   "\"AdvisorModule\".\"AdvisorStudyProgramAdvisorProgramModulesKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(AdvisorStudyProgramID advisorStudyProgramAdvisorProgramModulesKey) ->
     Database.CDBI.Connection.SQLInt
      advisorStudyProgramAdvisorProgramModulesKey)
   (\(Database.CDBI.Connection.SQLInt
       advisorStudyProgramAdvisorProgramModulesKey) ->
     AdvisorStudyProgramID advisorStudyProgramAdvisorProgramModulesKey)

--- The description of the database column `CategoryAdvisorCategorizingKey` of the `AdvisorModule` entity.
advisorModuleCategoryAdvisorCategorizingKeyColDesc
  :: Database.CDBI.Description.ColumnDescription CategoryID
advisorModuleCategoryAdvisorCategorizingKeyColDesc =
  Database.CDBI.Description.ColDesc
   "\"AdvisorModule\".\"CategoryAdvisorCategorizingKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(CategoryID categoryAdvisorCategorizingKey) ->
     Database.CDBI.Connection.SQLInt categoryAdvisorCategorizingKey)
   (\(Database.CDBI.Connection.SQLInt categoryAdvisorCategorizingKey) ->
     CategoryID categoryAdvisorCategorizingKey)

--- The description of the database column `ModInstAdvisedProgramModuleInstancesKey` of the `AdvisorModule` entity.
advisorModuleModInstAdvisedProgramModuleInstancesKeyColDesc
  :: Database.CDBI.Description.ColumnDescription ModInstID
advisorModuleModInstAdvisedProgramModuleInstancesKeyColDesc =
  Database.CDBI.Description.ColDesc
   "\"AdvisorModule\".\"ModInstAdvisedProgramModuleInstancesKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(ModInstID modInstAdvisedProgramModuleInstancesKey) ->
     Database.CDBI.Connection.SQLInt modInstAdvisedProgramModuleInstancesKey)
   (\(Database.CDBI.Connection.SQLInt
       modInstAdvisedProgramModuleInstancesKey) ->
     ModInstID modInstAdvisedProgramModuleInstancesKey)

--- Gets the attribute `Key` of the `AdvisorModule` entity.
advisorModuleKey :: AdvisorModule -> AdvisorModuleID
advisorModuleKey (AdvisorModule a _ _ _ _) = a

--- Gets the attribute `Mandatory` of the `AdvisorModule` entity.
advisorModuleMandatory :: AdvisorModule -> Bool
advisorModuleMandatory (AdvisorModule _ a _ _ _) = a

--- Gets the attribute `AdvisorStudyProgramAdvisorProgramModulesKey` of the `AdvisorModule` entity.
advisorModuleAdvisorStudyProgramAdvisorProgramModulesKey
  :: AdvisorModule -> AdvisorStudyProgramID
advisorModuleAdvisorStudyProgramAdvisorProgramModulesKey
    (AdvisorModule _ _ a _ _) =
  a

--- Gets the attribute `CategoryAdvisorCategorizingKey` of the `AdvisorModule` entity.
advisorModuleCategoryAdvisorCategorizingKey :: AdvisorModule -> CategoryID
advisorModuleCategoryAdvisorCategorizingKey (AdvisorModule _ _ _ a _) = a

--- Gets the attribute `ModInstAdvisedProgramModuleInstancesKey` of the `AdvisorModule` entity.
advisorModuleModInstAdvisedProgramModuleInstancesKey
  :: AdvisorModule -> ModInstID
advisorModuleModInstAdvisedProgramModuleInstancesKey (AdvisorModule _ _ _ _ a) =
  a

--- Sets the attribute `Key` of the `AdvisorModule` entity.
setAdvisorModuleKey :: AdvisorModule -> AdvisorModuleID -> AdvisorModule
setAdvisorModuleKey (AdvisorModule _ b4 b3 b2 b1) a =
  AdvisorModule a b4 b3 b2 b1

--- Sets the attribute `Mandatory` of the `AdvisorModule` entity.
setAdvisorModuleMandatory :: AdvisorModule -> Bool -> AdvisorModule
setAdvisorModuleMandatory (AdvisorModule a2 _ b3 b2 b1) a =
  AdvisorModule a2 a b3 b2 b1

--- Sets the attribute `AdvisorStudyProgramAdvisorProgramModulesKey` of the `AdvisorModule` entity.
setAdvisorModuleAdvisorStudyProgramAdvisorProgramModulesKey
  :: AdvisorModule -> AdvisorStudyProgramID -> AdvisorModule
setAdvisorModuleAdvisorStudyProgramAdvisorProgramModulesKey
    (AdvisorModule a3 a2 _ b2 b1) a =
  AdvisorModule a3 a2 a b2 b1

--- Sets the attribute `CategoryAdvisorCategorizingKey` of the `AdvisorModule` entity.
setAdvisorModuleCategoryAdvisorCategorizingKey
  :: AdvisorModule -> CategoryID -> AdvisorModule
setAdvisorModuleCategoryAdvisorCategorizingKey (AdvisorModule a4 a3 a2 _ b1) a =
  AdvisorModule a4 a3 a2 a b1

--- Sets the attribute `ModInstAdvisedProgramModuleInstancesKey` of the `AdvisorModule` entity.
setAdvisorModuleModInstAdvisedProgramModuleInstancesKey
  :: AdvisorModule -> ModInstID -> AdvisorModule
setAdvisorModuleModInstAdvisedProgramModuleInstancesKey
    (AdvisorModule a5 a4 a3 a2 _) a =
  AdvisorModule a5 a4 a3 a2 a

--- id-to-value function for entity `AdvisorModule`.
advisorModuleID
  :: AdvisorModuleID -> Database.CDBI.Criteria.Value AdvisorModuleID
advisorModuleID (AdvisorModuleID key) = Database.CDBI.Criteria.idVal key

--- id-to-int function for entity `AdvisorModule`.
advisorModuleKeyToInt :: AdvisorModuleID -> Int
advisorModuleKeyToInt (AdvisorModuleID key) = key

--- Shows the key of a `AdvisorModule` entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showAdvisorModuleKey :: AdvisorModule -> String
showAdvisorModuleKey entry =
  Database.CDBI.ER.showDatabaseKey "AdvisorModule" advisorModuleKeyToInt
   (advisorModuleKey entry)

--- Transforms a string into a key of a `AdvisorModule` entity.
--- Nothing is returned if the string does not represent a meaningful key.
readAdvisorModuleKey :: String -> Maybe AdvisorModuleID
readAdvisorModuleKey =
  Database.CDBI.ER.readDatabaseKey "AdvisorModule" AdvisorModuleID

--- Gets all `AdvisorModule` entities.
queryAllAdvisorModules :: Database.CDBI.Connection.DBAction [AdvisorModule]
queryAllAdvisorModules =
  Database.CDBI.ER.getAllEntries advisorModule_CDBI_Description

--- Gets all `AdvisorModule` entities satisfying a given predicate.
queryCondAdvisorModule
  :: (AdvisorModule -> Bool)
  -> Database.CDBI.Connection.DBAction [AdvisorModule]
queryCondAdvisorModule =
  Database.CDBI.ER.getCondEntries advisorModule_CDBI_Description

--- Gets a `AdvisorModule` entry by a given key.
getAdvisorModule
  :: AdvisorModuleID -> Database.CDBI.Connection.DBAction AdvisorModule
getAdvisorModule =
  Database.CDBI.ER.getEntryWithKey advisorModule_CDBI_Description
   advisorModuleColumnKey
   advisorModuleID

--- Inserts a new `AdvisorModule` entity.
newAdvisorModuleWithAdvisorStudyProgramAdvisorProgramModulesKeyWithCategoryAdvisorCategorizingKeyWithModInstAdvisedProgramModuleInstancesKey
  :: Bool
  -> AdvisorStudyProgramID
  -> CategoryID -> ModInstID -> Database.CDBI.Connection.DBAction AdvisorModule
newAdvisorModuleWithAdvisorStudyProgramAdvisorProgramModulesKeyWithCategoryAdvisorCategorizingKeyWithModInstAdvisedProgramModuleInstancesKey
    mandatory_p
    advisorStudyProgramAdvisorProgramModulesKey_p
    categoryAdvisorCategorizingKey_p
    modInstAdvisedProgramModuleInstancesKey_p =
  Database.CDBI.ER.insertNewEntry advisorModule_CDBI_Description
   setAdvisorModuleKey
   AdvisorModuleID
   (AdvisorModule (AdvisorModuleID 0) mandatory_p
     advisorStudyProgramAdvisorProgramModulesKey_p
     categoryAdvisorCategorizingKey_p
     modInstAdvisedProgramModuleInstancesKey_p)

--- Deletes an existing `AdvisorModule` entry by its key.
deleteAdvisorModule :: AdvisorModule -> Database.CDBI.Connection.DBAction ()
deleteAdvisorModule =
  Database.CDBI.ER.deleteEntry advisorModule_CDBI_Description
   advisorModuleColumnKey
   (advisorModuleID . advisorModuleKey)

--- Updates an existing `AdvisorModule` entry by its key.
updateAdvisorModule :: AdvisorModule -> Database.CDBI.Connection.DBAction ()
updateAdvisorModule =
  Database.CDBI.ER.updateEntry advisorModule_CDBI_Description

--- The ER description of the `MasterProgram` entity.
masterProgram_CDBI_Description
  :: Database.CDBI.Description.EntityDescription MasterProgram
masterProgram_CDBI_Description =
  Database.CDBI.Description.ED "MasterProgram"
   [Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeBool
   ,Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeInt]
   (\(MasterProgram
       (MasterProgramID key)
       name
       term
       year
       desc
       prereq
       comments
       visible
       (UserID userAdvisingKey)
       (MasterCoreAreaID masterCoreAreaAreaProgramsKey)) ->
     [Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString name
     ,Database.CDBI.Connection.SQLString term
     ,Database.CDBI.Connection.SQLInt year
     ,Database.CDBI.Description.sqlString desc
     ,Database.CDBI.Description.sqlString prereq
     ,Database.CDBI.Description.sqlString comments
     ,Database.CDBI.Connection.SQLBool visible
     ,Database.CDBI.Connection.SQLInt userAdvisingKey
     ,Database.CDBI.Connection.SQLInt masterCoreAreaAreaProgramsKey])
   (\(MasterProgram
       _
       name
       term
       year
       desc
       prereq
       comments
       visible
       (UserID userAdvisingKey)
       (MasterCoreAreaID masterCoreAreaAreaProgramsKey)) ->
     [Database.CDBI.Connection.SQLNull
     ,Database.CDBI.Connection.SQLString name
     ,Database.CDBI.Connection.SQLString term
     ,Database.CDBI.Connection.SQLInt year
     ,Database.CDBI.Description.sqlString desc
     ,Database.CDBI.Description.sqlString prereq
     ,Database.CDBI.Description.sqlString comments
     ,Database.CDBI.Connection.SQLBool visible
     ,Database.CDBI.Connection.SQLInt userAdvisingKey
     ,Database.CDBI.Connection.SQLInt masterCoreAreaAreaProgramsKey])
   (\[Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString name
     ,Database.CDBI.Connection.SQLString term
     ,Database.CDBI.Connection.SQLInt year
     ,desc
     ,prereq
     ,comments
     ,Database.CDBI.Connection.SQLBool visible
     ,Database.CDBI.Connection.SQLInt userAdvisingKey
     ,Database.CDBI.Connection.SQLInt masterCoreAreaAreaProgramsKey] ->
     MasterProgram (MasterProgramID key) name term year
      (Database.CDBI.Description.fromStringOrNull desc)
      (Database.CDBI.Description.fromStringOrNull prereq)
      (Database.CDBI.Description.fromStringOrNull comments)
      visible
      (UserID userAdvisingKey)
      (MasterCoreAreaID masterCoreAreaAreaProgramsKey))

--- The database table of the `MasterProgram` entity.
masterProgramTable :: Database.CDBI.Description.Table
masterProgramTable = "MasterProgram"

--- The database column `Key` of the `MasterProgram` entity.
masterProgramColumnKey :: Database.CDBI.Description.Column MasterProgramID
masterProgramColumnKey =
  Database.CDBI.Description.Column "\"Key\"" "\"MasterProgram\".\"Key\""

--- The database column `Name` of the `MasterProgram` entity.
masterProgramColumnName :: Database.CDBI.Description.Column String
masterProgramColumnName =
  Database.CDBI.Description.Column "\"Name\"" "\"MasterProgram\".\"Name\""

--- The database column `Term` of the `MasterProgram` entity.
masterProgramColumnTerm :: Database.CDBI.Description.Column String
masterProgramColumnTerm =
  Database.CDBI.Description.Column "\"Term\"" "\"MasterProgram\".\"Term\""

--- The database column `Year` of the `MasterProgram` entity.
masterProgramColumnYear :: Database.CDBI.Description.Column Int
masterProgramColumnYear =
  Database.CDBI.Description.Column "\"Year\"" "\"MasterProgram\".\"Year\""

--- The database column `Desc` of the `MasterProgram` entity.
masterProgramColumnDesc :: Database.CDBI.Description.Column String
masterProgramColumnDesc =
  Database.CDBI.Description.Column "\"Desc\"" "\"MasterProgram\".\"Desc\""

--- The database column `Prereq` of the `MasterProgram` entity.
masterProgramColumnPrereq :: Database.CDBI.Description.Column String
masterProgramColumnPrereq =
  Database.CDBI.Description.Column "\"Prereq\"" "\"MasterProgram\".\"Prereq\""

--- The database column `Comments` of the `MasterProgram` entity.
masterProgramColumnComments :: Database.CDBI.Description.Column String
masterProgramColumnComments =
  Database.CDBI.Description.Column "\"Comments\""
   "\"MasterProgram\".\"Comments\""

--- The database column `Visible` of the `MasterProgram` entity.
masterProgramColumnVisible :: Database.CDBI.Description.Column Bool
masterProgramColumnVisible =
  Database.CDBI.Description.Column "\"Visible\"" "\"MasterProgram\".\"Visible\""

--- The database column `UserAdvisingKey` of the `MasterProgram` entity.
masterProgramColumnUserAdvisingKey :: Database.CDBI.Description.Column UserID
masterProgramColumnUserAdvisingKey =
  Database.CDBI.Description.Column "\"UserAdvisingKey\""
   "\"MasterProgram\".\"UserAdvisingKey\""

--- The database column `MasterCoreAreaAreaProgramsKey` of the `MasterProgram` entity.
masterProgramColumnMasterCoreAreaAreaProgramsKey
  :: Database.CDBI.Description.Column MasterCoreAreaID
masterProgramColumnMasterCoreAreaAreaProgramsKey =
  Database.CDBI.Description.Column "\"MasterCoreAreaAreaProgramsKey\""
   "\"MasterProgram\".\"MasterCoreAreaAreaProgramsKey\""

--- The description of the database column `Key` of the `MasterProgram` entity.
masterProgramKeyColDesc
  :: Database.CDBI.Description.ColumnDescription MasterProgramID
masterProgramKeyColDesc =
  Database.CDBI.Description.ColDesc "\"MasterProgram\".\"Key\""
   Database.CDBI.Connection.SQLTypeInt
   (\(MasterProgramID key) -> Database.CDBI.Connection.SQLInt key)
   (\(Database.CDBI.Connection.SQLInt key) -> MasterProgramID key)

--- The description of the database column `Name` of the `MasterProgram` entity.
masterProgramNameColDesc :: Database.CDBI.Description.ColumnDescription String
masterProgramNameColDesc =
  Database.CDBI.Description.ColDesc "\"MasterProgram\".\"Name\""
   Database.CDBI.Connection.SQLTypeString
   (\name -> Database.CDBI.Connection.SQLString name)
   (\(Database.CDBI.Connection.SQLString name) -> name)

--- The description of the database column `Term` of the `MasterProgram` entity.
masterProgramTermColDesc :: Database.CDBI.Description.ColumnDescription String
masterProgramTermColDesc =
  Database.CDBI.Description.ColDesc "\"MasterProgram\".\"Term\""
   Database.CDBI.Connection.SQLTypeString
   (\term -> Database.CDBI.Connection.SQLString term)
   (\(Database.CDBI.Connection.SQLString term) -> term)

--- The description of the database column `Year` of the `MasterProgram` entity.
masterProgramYearColDesc :: Database.CDBI.Description.ColumnDescription Int
masterProgramYearColDesc =
  Database.CDBI.Description.ColDesc "\"MasterProgram\".\"Year\""
   Database.CDBI.Connection.SQLTypeInt
   (\year -> Database.CDBI.Connection.SQLInt year)
   (\(Database.CDBI.Connection.SQLInt year) -> year)

--- The description of the database column `Desc` of the `MasterProgram` entity.
masterProgramDescColDesc :: Database.CDBI.Description.ColumnDescription String
masterProgramDescColDesc =
  Database.CDBI.Description.ColDesc "\"MasterProgram\".\"Desc\""
   Database.CDBI.Connection.SQLTypeString
   (\desc -> Database.CDBI.Description.sqlString desc)
   (\desc -> Database.CDBI.Description.fromStringOrNull desc)

--- The description of the database column `Prereq` of the `MasterProgram` entity.
masterProgramPrereqColDesc :: Database.CDBI.Description.ColumnDescription String
masterProgramPrereqColDesc =
  Database.CDBI.Description.ColDesc "\"MasterProgram\".\"Prereq\""
   Database.CDBI.Connection.SQLTypeString
   (\prereq -> Database.CDBI.Description.sqlString prereq)
   (\prereq -> Database.CDBI.Description.fromStringOrNull prereq)

--- The description of the database column `Comments` of the `MasterProgram` entity.
masterProgramCommentsColDesc
  :: Database.CDBI.Description.ColumnDescription String
masterProgramCommentsColDesc =
  Database.CDBI.Description.ColDesc "\"MasterProgram\".\"Comments\""
   Database.CDBI.Connection.SQLTypeString
   (\comments -> Database.CDBI.Description.sqlString comments)
   (\comments -> Database.CDBI.Description.fromStringOrNull comments)

--- The description of the database column `Visible` of the `MasterProgram` entity.
masterProgramVisibleColDesc :: Database.CDBI.Description.ColumnDescription Bool
masterProgramVisibleColDesc =
  Database.CDBI.Description.ColDesc "\"MasterProgram\".\"Visible\""
   Database.CDBI.Connection.SQLTypeBool
   (\visible -> Database.CDBI.Connection.SQLBool visible)
   (\(Database.CDBI.Connection.SQLBool visible) -> visible)

--- The description of the database column `UserAdvisingKey` of the `MasterProgram` entity.
masterProgramUserAdvisingKeyColDesc
  :: Database.CDBI.Description.ColumnDescription UserID
masterProgramUserAdvisingKeyColDesc =
  Database.CDBI.Description.ColDesc "\"MasterProgram\".\"UserAdvisingKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(UserID userAdvisingKey) ->
     Database.CDBI.Connection.SQLInt userAdvisingKey)
   (\(Database.CDBI.Connection.SQLInt userAdvisingKey) ->
     UserID userAdvisingKey)

--- The description of the database column `MasterCoreAreaAreaProgramsKey` of the `MasterProgram` entity.
masterProgramMasterCoreAreaAreaProgramsKeyColDesc
  :: Database.CDBI.Description.ColumnDescription MasterCoreAreaID
masterProgramMasterCoreAreaAreaProgramsKeyColDesc =
  Database.CDBI.Description.ColDesc
   "\"MasterProgram\".\"MasterCoreAreaAreaProgramsKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(MasterCoreAreaID masterCoreAreaAreaProgramsKey) ->
     Database.CDBI.Connection.SQLInt masterCoreAreaAreaProgramsKey)
   (\(Database.CDBI.Connection.SQLInt masterCoreAreaAreaProgramsKey) ->
     MasterCoreAreaID masterCoreAreaAreaProgramsKey)

--- Gets the attribute `Key` of the `MasterProgram` entity.
masterProgramKey :: MasterProgram -> MasterProgramID
masterProgramKey (MasterProgram a _ _ _ _ _ _ _ _ _) = a

--- Gets the attribute `Name` of the `MasterProgram` entity.
masterProgramName :: MasterProgram -> String
masterProgramName (MasterProgram _ a _ _ _ _ _ _ _ _) = a

--- Gets the attribute `Term` of the `MasterProgram` entity.
masterProgramTerm :: MasterProgram -> String
masterProgramTerm (MasterProgram _ _ a _ _ _ _ _ _ _) = a

--- Gets the attribute `Year` of the `MasterProgram` entity.
masterProgramYear :: MasterProgram -> Int
masterProgramYear (MasterProgram _ _ _ a _ _ _ _ _ _) = a

--- Gets the attribute `Desc` of the `MasterProgram` entity.
masterProgramDesc :: MasterProgram -> String
masterProgramDesc (MasterProgram _ _ _ _ a _ _ _ _ _) = a

--- Gets the attribute `Prereq` of the `MasterProgram` entity.
masterProgramPrereq :: MasterProgram -> String
masterProgramPrereq (MasterProgram _ _ _ _ _ a _ _ _ _) = a

--- Gets the attribute `Comments` of the `MasterProgram` entity.
masterProgramComments :: MasterProgram -> String
masterProgramComments (MasterProgram _ _ _ _ _ _ a _ _ _) = a

--- Gets the attribute `Visible` of the `MasterProgram` entity.
masterProgramVisible :: MasterProgram -> Bool
masterProgramVisible (MasterProgram _ _ _ _ _ _ _ a _ _) = a

--- Gets the attribute `UserAdvisingKey` of the `MasterProgram` entity.
masterProgramUserAdvisingKey :: MasterProgram -> UserID
masterProgramUserAdvisingKey (MasterProgram _ _ _ _ _ _ _ _ a _) = a

--- Gets the attribute `MasterCoreAreaAreaProgramsKey` of the `MasterProgram` entity.
masterProgramMasterCoreAreaAreaProgramsKey :: MasterProgram -> MasterCoreAreaID
masterProgramMasterCoreAreaAreaProgramsKey (MasterProgram _ _ _ _ _ _ _ _ _ a) =
  a

--- Sets the attribute `Key` of the `MasterProgram` entity.
setMasterProgramKey :: MasterProgram -> MasterProgramID -> MasterProgram
setMasterProgramKey (MasterProgram _ b9 b8 b7 b6 b5 b4 b3 b2 b1) a =
  MasterProgram a b9 b8 b7 b6 b5 b4 b3 b2 b1

--- Sets the attribute `Name` of the `MasterProgram` entity.
setMasterProgramName :: MasterProgram -> String -> MasterProgram
setMasterProgramName (MasterProgram a2 _ b8 b7 b6 b5 b4 b3 b2 b1) a =
  MasterProgram a2 a b8 b7 b6 b5 b4 b3 b2 b1

--- Sets the attribute `Term` of the `MasterProgram` entity.
setMasterProgramTerm :: MasterProgram -> String -> MasterProgram
setMasterProgramTerm (MasterProgram a3 a2 _ b7 b6 b5 b4 b3 b2 b1) a =
  MasterProgram a3 a2 a b7 b6 b5 b4 b3 b2 b1

--- Sets the attribute `Year` of the `MasterProgram` entity.
setMasterProgramYear :: MasterProgram -> Int -> MasterProgram
setMasterProgramYear (MasterProgram a4 a3 a2 _ b6 b5 b4 b3 b2 b1) a =
  MasterProgram a4 a3 a2 a b6 b5 b4 b3 b2 b1

--- Sets the attribute `Desc` of the `MasterProgram` entity.
setMasterProgramDesc :: MasterProgram -> String -> MasterProgram
setMasterProgramDesc (MasterProgram a5 a4 a3 a2 _ b5 b4 b3 b2 b1) a =
  MasterProgram a5 a4 a3 a2 a b5 b4 b3 b2 b1

--- Sets the attribute `Prereq` of the `MasterProgram` entity.
setMasterProgramPrereq :: MasterProgram -> String -> MasterProgram
setMasterProgramPrereq (MasterProgram a6 a5 a4 a3 a2 _ b4 b3 b2 b1) a =
  MasterProgram a6 a5 a4 a3 a2 a b4 b3 b2 b1

--- Sets the attribute `Comments` of the `MasterProgram` entity.
setMasterProgramComments :: MasterProgram -> String -> MasterProgram
setMasterProgramComments (MasterProgram a7 a6 a5 a4 a3 a2 _ b3 b2 b1) a =
  MasterProgram a7 a6 a5 a4 a3 a2 a b3 b2 b1

--- Sets the attribute `Visible` of the `MasterProgram` entity.
setMasterProgramVisible :: MasterProgram -> Bool -> MasterProgram
setMasterProgramVisible (MasterProgram a8 a7 a6 a5 a4 a3 a2 _ b2 b1) a =
  MasterProgram a8 a7 a6 a5 a4 a3 a2 a b2 b1

--- Sets the attribute `UserAdvisingKey` of the `MasterProgram` entity.
setMasterProgramUserAdvisingKey :: MasterProgram -> UserID -> MasterProgram
setMasterProgramUserAdvisingKey (MasterProgram a9 a8 a7 a6 a5 a4 a3 a2 _ b1) a =
  MasterProgram a9 a8 a7 a6 a5 a4 a3 a2 a b1

--- Sets the attribute `MasterCoreAreaAreaProgramsKey` of the `MasterProgram` entity.
setMasterProgramMasterCoreAreaAreaProgramsKey
  :: MasterProgram -> MasterCoreAreaID -> MasterProgram
setMasterProgramMasterCoreAreaAreaProgramsKey
    (MasterProgram a10 a9 a8 a7 a6 a5 a4 a3 a2 _) a =
  MasterProgram a10 a9 a8 a7 a6 a5 a4 a3 a2 a

--- id-to-value function for entity `MasterProgram`.
masterProgramID
  :: MasterProgramID -> Database.CDBI.Criteria.Value MasterProgramID
masterProgramID (MasterProgramID key) = Database.CDBI.Criteria.idVal key

--- id-to-int function for entity `MasterProgram`.
masterProgramKeyToInt :: MasterProgramID -> Int
masterProgramKeyToInt (MasterProgramID key) = key

--- Shows the key of a `MasterProgram` entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showMasterProgramKey :: MasterProgram -> String
showMasterProgramKey entry =
  Database.CDBI.ER.showDatabaseKey "MasterProgram" masterProgramKeyToInt
   (masterProgramKey entry)

--- Transforms a string into a key of a `MasterProgram` entity.
--- Nothing is returned if the string does not represent a meaningful key.
readMasterProgramKey :: String -> Maybe MasterProgramID
readMasterProgramKey =
  Database.CDBI.ER.readDatabaseKey "MasterProgram" MasterProgramID

--- Gets all `MasterProgram` entities.
queryAllMasterPrograms :: Database.CDBI.Connection.DBAction [MasterProgram]
queryAllMasterPrograms =
  Database.CDBI.ER.getAllEntries masterProgram_CDBI_Description

--- Gets all `MasterProgram` entities satisfying a given predicate.
queryCondMasterProgram
  :: (MasterProgram -> Bool)
  -> Database.CDBI.Connection.DBAction [MasterProgram]
queryCondMasterProgram =
  Database.CDBI.ER.getCondEntries masterProgram_CDBI_Description

--- Gets a `MasterProgram` entry by a given key.
getMasterProgram
  :: MasterProgramID -> Database.CDBI.Connection.DBAction MasterProgram
getMasterProgram =
  Database.CDBI.ER.getEntryWithKey masterProgram_CDBI_Description
   masterProgramColumnKey
   masterProgramID

--- Inserts a new `MasterProgram` entity.
newMasterProgramWithUserAdvisingKeyWithMasterCoreAreaAreaProgramsKey
  :: String
  -> String
  -> Maybe Int
  -> String
  -> String
  -> String
  -> Bool
  -> UserID
  -> MasterCoreAreaID -> Database.CDBI.Connection.DBAction MasterProgram
newMasterProgramWithUserAdvisingKeyWithMasterCoreAreaAreaProgramsKey
    name_p
    term_p
    year_p
    desc_p
    prereq_p
    comments_p
    visible_p
    userAdvisingKey_p
    masterCoreAreaAreaProgramsKey_p =
  Database.CDBI.ER.insertNewEntry masterProgram_CDBI_Description
   setMasterProgramKey
   MasterProgramID
   (MasterProgram (MasterProgramID 0) name_p term_p (maybe 2019 id year_p)
     desc_p
     prereq_p
     comments_p
     visible_p
     userAdvisingKey_p
     masterCoreAreaAreaProgramsKey_p)

--- Deletes an existing `MasterProgram` entry by its key.
deleteMasterProgram :: MasterProgram -> Database.CDBI.Connection.DBAction ()
deleteMasterProgram =
  Database.CDBI.ER.deleteEntry masterProgram_CDBI_Description
   masterProgramColumnKey
   (masterProgramID . masterProgramKey)

--- Updates an existing `MasterProgram` entry by its key.
updateMasterProgram :: MasterProgram -> Database.CDBI.Connection.DBAction ()
updateMasterProgram =
  Database.CDBI.ER.updateEntry masterProgram_CDBI_Description

--- The ER description of the `MasterProgInfo` entity.
masterProgInfo_CDBI_Description
  :: Database.CDBI.Description.EntityDescription MasterProgInfo
masterProgInfo_CDBI_Description =
  Database.CDBI.Description.ED "MasterProgInfo"
   [Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeInt]
   (\(MasterProgInfo
       (MasterProgInfoID key)
       progModules
       praktikum
       seminar
       thesis
       allgGrundlagen
       anwendungsfach
       (MasterProgramID masterProgramProgramInfoKey)) ->
     [Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString progModules
     ,Database.CDBI.Description.sqlString praktikum
     ,Database.CDBI.Description.sqlString seminar
     ,Database.CDBI.Description.sqlString thesis
     ,Database.CDBI.Description.sqlString allgGrundlagen
     ,Database.CDBI.Description.sqlString anwendungsfach
     ,Database.CDBI.Connection.SQLInt masterProgramProgramInfoKey])
   (\(MasterProgInfo
       _
       progModules
       praktikum
       seminar
       thesis
       allgGrundlagen
       anwendungsfach
       (MasterProgramID masterProgramProgramInfoKey)) ->
     [Database.CDBI.Connection.SQLNull
     ,Database.CDBI.Connection.SQLString progModules
     ,Database.CDBI.Description.sqlString praktikum
     ,Database.CDBI.Description.sqlString seminar
     ,Database.CDBI.Description.sqlString thesis
     ,Database.CDBI.Description.sqlString allgGrundlagen
     ,Database.CDBI.Description.sqlString anwendungsfach
     ,Database.CDBI.Connection.SQLInt masterProgramProgramInfoKey])
   (\[Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString progModules
     ,praktikum
     ,seminar
     ,thesis
     ,allgGrundlagen
     ,anwendungsfach
     ,Database.CDBI.Connection.SQLInt masterProgramProgramInfoKey] ->
     MasterProgInfo (MasterProgInfoID key) progModules
      (Database.CDBI.Description.fromStringOrNull praktikum)
      (Database.CDBI.Description.fromStringOrNull seminar)
      (Database.CDBI.Description.fromStringOrNull thesis)
      (Database.CDBI.Description.fromStringOrNull allgGrundlagen)
      (Database.CDBI.Description.fromStringOrNull anwendungsfach)
      (MasterProgramID masterProgramProgramInfoKey))

--- The database table of the `MasterProgInfo` entity.
masterProgInfoTable :: Database.CDBI.Description.Table
masterProgInfoTable = "MasterProgInfo"

--- The database column `Key` of the `MasterProgInfo` entity.
masterProgInfoColumnKey :: Database.CDBI.Description.Column MasterProgInfoID
masterProgInfoColumnKey =
  Database.CDBI.Description.Column "\"Key\"" "\"MasterProgInfo\".\"Key\""

--- The database column `ProgModules` of the `MasterProgInfo` entity.
masterProgInfoColumnProgModules :: Database.CDBI.Description.Column String
masterProgInfoColumnProgModules =
  Database.CDBI.Description.Column "\"ProgModules\""
   "\"MasterProgInfo\".\"ProgModules\""

--- The database column `Praktikum` of the `MasterProgInfo` entity.
masterProgInfoColumnPraktikum :: Database.CDBI.Description.Column String
masterProgInfoColumnPraktikum =
  Database.CDBI.Description.Column "\"Praktikum\""
   "\"MasterProgInfo\".\"Praktikum\""

--- The database column `Seminar` of the `MasterProgInfo` entity.
masterProgInfoColumnSeminar :: Database.CDBI.Description.Column String
masterProgInfoColumnSeminar =
  Database.CDBI.Description.Column "\"Seminar\""
   "\"MasterProgInfo\".\"Seminar\""

--- The database column `Thesis` of the `MasterProgInfo` entity.
masterProgInfoColumnThesis :: Database.CDBI.Description.Column String
masterProgInfoColumnThesis =
  Database.CDBI.Description.Column "\"Thesis\"" "\"MasterProgInfo\".\"Thesis\""

--- The database column `AllgGrundlagen` of the `MasterProgInfo` entity.
masterProgInfoColumnAllgGrundlagen :: Database.CDBI.Description.Column String
masterProgInfoColumnAllgGrundlagen =
  Database.CDBI.Description.Column "\"AllgGrundlagen\""
   "\"MasterProgInfo\".\"AllgGrundlagen\""

--- The database column `Anwendungsfach` of the `MasterProgInfo` entity.
masterProgInfoColumnAnwendungsfach :: Database.CDBI.Description.Column String
masterProgInfoColumnAnwendungsfach =
  Database.CDBI.Description.Column "\"Anwendungsfach\""
   "\"MasterProgInfo\".\"Anwendungsfach\""

--- The database column `MasterProgramProgramInfoKey` of the `MasterProgInfo` entity.
masterProgInfoColumnMasterProgramProgramInfoKey
  :: Database.CDBI.Description.Column MasterProgramID
masterProgInfoColumnMasterProgramProgramInfoKey =
  Database.CDBI.Description.Column "\"MasterProgramProgramInfoKey\""
   "\"MasterProgInfo\".\"MasterProgramProgramInfoKey\""

--- The description of the database column `Key` of the `MasterProgInfo` entity.
masterProgInfoKeyColDesc
  :: Database.CDBI.Description.ColumnDescription MasterProgInfoID
masterProgInfoKeyColDesc =
  Database.CDBI.Description.ColDesc "\"MasterProgInfo\".\"Key\""
   Database.CDBI.Connection.SQLTypeInt
   (\(MasterProgInfoID key) -> Database.CDBI.Connection.SQLInt key)
   (\(Database.CDBI.Connection.SQLInt key) -> MasterProgInfoID key)

--- The description of the database column `ProgModules` of the `MasterProgInfo` entity.
masterProgInfoProgModulesColDesc
  :: Database.CDBI.Description.ColumnDescription String
masterProgInfoProgModulesColDesc =
  Database.CDBI.Description.ColDesc "\"MasterProgInfo\".\"ProgModules\""
   Database.CDBI.Connection.SQLTypeString
   (\progModules -> Database.CDBI.Connection.SQLString progModules)
   (\(Database.CDBI.Connection.SQLString progModules) -> progModules)

--- The description of the database column `Praktikum` of the `MasterProgInfo` entity.
masterProgInfoPraktikumColDesc
  :: Database.CDBI.Description.ColumnDescription String
masterProgInfoPraktikumColDesc =
  Database.CDBI.Description.ColDesc "\"MasterProgInfo\".\"Praktikum\""
   Database.CDBI.Connection.SQLTypeString
   (\praktikum -> Database.CDBI.Description.sqlString praktikum)
   (\praktikum -> Database.CDBI.Description.fromStringOrNull praktikum)

--- The description of the database column `Seminar` of the `MasterProgInfo` entity.
masterProgInfoSeminarColDesc
  :: Database.CDBI.Description.ColumnDescription String
masterProgInfoSeminarColDesc =
  Database.CDBI.Description.ColDesc "\"MasterProgInfo\".\"Seminar\""
   Database.CDBI.Connection.SQLTypeString
   (\seminar -> Database.CDBI.Description.sqlString seminar)
   (\seminar -> Database.CDBI.Description.fromStringOrNull seminar)

--- The description of the database column `Thesis` of the `MasterProgInfo` entity.
masterProgInfoThesisColDesc
  :: Database.CDBI.Description.ColumnDescription String
masterProgInfoThesisColDesc =
  Database.CDBI.Description.ColDesc "\"MasterProgInfo\".\"Thesis\""
   Database.CDBI.Connection.SQLTypeString
   (\thesis -> Database.CDBI.Description.sqlString thesis)
   (\thesis -> Database.CDBI.Description.fromStringOrNull thesis)

--- The description of the database column `AllgGrundlagen` of the `MasterProgInfo` entity.
masterProgInfoAllgGrundlagenColDesc
  :: Database.CDBI.Description.ColumnDescription String
masterProgInfoAllgGrundlagenColDesc =
  Database.CDBI.Description.ColDesc "\"MasterProgInfo\".\"AllgGrundlagen\""
   Database.CDBI.Connection.SQLTypeString
   (\allgGrundlagen -> Database.CDBI.Description.sqlString allgGrundlagen)
   (\allgGrundlagen ->
     Database.CDBI.Description.fromStringOrNull allgGrundlagen)

--- The description of the database column `Anwendungsfach` of the `MasterProgInfo` entity.
masterProgInfoAnwendungsfachColDesc
  :: Database.CDBI.Description.ColumnDescription String
masterProgInfoAnwendungsfachColDesc =
  Database.CDBI.Description.ColDesc "\"MasterProgInfo\".\"Anwendungsfach\""
   Database.CDBI.Connection.SQLTypeString
   (\anwendungsfach -> Database.CDBI.Description.sqlString anwendungsfach)
   (\anwendungsfach ->
     Database.CDBI.Description.fromStringOrNull anwendungsfach)

--- The description of the database column `MasterProgramProgramInfoKey` of the `MasterProgInfo` entity.
masterProgInfoMasterProgramProgramInfoKeyColDesc
  :: Database.CDBI.Description.ColumnDescription MasterProgramID
masterProgInfoMasterProgramProgramInfoKeyColDesc =
  Database.CDBI.Description.ColDesc
   "\"MasterProgInfo\".\"MasterProgramProgramInfoKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(MasterProgramID masterProgramProgramInfoKey) ->
     Database.CDBI.Connection.SQLInt masterProgramProgramInfoKey)
   (\(Database.CDBI.Connection.SQLInt masterProgramProgramInfoKey) ->
     MasterProgramID masterProgramProgramInfoKey)

--- Gets the attribute `Key` of the `MasterProgInfo` entity.
masterProgInfoKey :: MasterProgInfo -> MasterProgInfoID
masterProgInfoKey (MasterProgInfo a _ _ _ _ _ _ _) = a

--- Gets the attribute `ProgModules` of the `MasterProgInfo` entity.
masterProgInfoProgModules :: MasterProgInfo -> String
masterProgInfoProgModules (MasterProgInfo _ a _ _ _ _ _ _) = a

--- Gets the attribute `Praktikum` of the `MasterProgInfo` entity.
masterProgInfoPraktikum :: MasterProgInfo -> String
masterProgInfoPraktikum (MasterProgInfo _ _ a _ _ _ _ _) = a

--- Gets the attribute `Seminar` of the `MasterProgInfo` entity.
masterProgInfoSeminar :: MasterProgInfo -> String
masterProgInfoSeminar (MasterProgInfo _ _ _ a _ _ _ _) = a

--- Gets the attribute `Thesis` of the `MasterProgInfo` entity.
masterProgInfoThesis :: MasterProgInfo -> String
masterProgInfoThesis (MasterProgInfo _ _ _ _ a _ _ _) = a

--- Gets the attribute `AllgGrundlagen` of the `MasterProgInfo` entity.
masterProgInfoAllgGrundlagen :: MasterProgInfo -> String
masterProgInfoAllgGrundlagen (MasterProgInfo _ _ _ _ _ a _ _) = a

--- Gets the attribute `Anwendungsfach` of the `MasterProgInfo` entity.
masterProgInfoAnwendungsfach :: MasterProgInfo -> String
masterProgInfoAnwendungsfach (MasterProgInfo _ _ _ _ _ _ a _) = a

--- Gets the attribute `MasterProgramProgramInfoKey` of the `MasterProgInfo` entity.
masterProgInfoMasterProgramProgramInfoKey :: MasterProgInfo -> MasterProgramID
masterProgInfoMasterProgramProgramInfoKey (MasterProgInfo _ _ _ _ _ _ _ a) = a

--- Sets the attribute `Key` of the `MasterProgInfo` entity.
setMasterProgInfoKey :: MasterProgInfo -> MasterProgInfoID -> MasterProgInfo
setMasterProgInfoKey (MasterProgInfo _ b7 b6 b5 b4 b3 b2 b1) a =
  MasterProgInfo a b7 b6 b5 b4 b3 b2 b1

--- Sets the attribute `ProgModules` of the `MasterProgInfo` entity.
setMasterProgInfoProgModules :: MasterProgInfo -> String -> MasterProgInfo
setMasterProgInfoProgModules (MasterProgInfo a2 _ b6 b5 b4 b3 b2 b1) a =
  MasterProgInfo a2 a b6 b5 b4 b3 b2 b1

--- Sets the attribute `Praktikum` of the `MasterProgInfo` entity.
setMasterProgInfoPraktikum :: MasterProgInfo -> String -> MasterProgInfo
setMasterProgInfoPraktikum (MasterProgInfo a3 a2 _ b5 b4 b3 b2 b1) a =
  MasterProgInfo a3 a2 a b5 b4 b3 b2 b1

--- Sets the attribute `Seminar` of the `MasterProgInfo` entity.
setMasterProgInfoSeminar :: MasterProgInfo -> String -> MasterProgInfo
setMasterProgInfoSeminar (MasterProgInfo a4 a3 a2 _ b4 b3 b2 b1) a =
  MasterProgInfo a4 a3 a2 a b4 b3 b2 b1

--- Sets the attribute `Thesis` of the `MasterProgInfo` entity.
setMasterProgInfoThesis :: MasterProgInfo -> String -> MasterProgInfo
setMasterProgInfoThesis (MasterProgInfo a5 a4 a3 a2 _ b3 b2 b1) a =
  MasterProgInfo a5 a4 a3 a2 a b3 b2 b1

--- Sets the attribute `AllgGrundlagen` of the `MasterProgInfo` entity.
setMasterProgInfoAllgGrundlagen :: MasterProgInfo -> String -> MasterProgInfo
setMasterProgInfoAllgGrundlagen (MasterProgInfo a6 a5 a4 a3 a2 _ b2 b1) a =
  MasterProgInfo a6 a5 a4 a3 a2 a b2 b1

--- Sets the attribute `Anwendungsfach` of the `MasterProgInfo` entity.
setMasterProgInfoAnwendungsfach :: MasterProgInfo -> String -> MasterProgInfo
setMasterProgInfoAnwendungsfach (MasterProgInfo a7 a6 a5 a4 a3 a2 _ b1) a =
  MasterProgInfo a7 a6 a5 a4 a3 a2 a b1

--- Sets the attribute `MasterProgramProgramInfoKey` of the `MasterProgInfo` entity.
setMasterProgInfoMasterProgramProgramInfoKey
  :: MasterProgInfo -> MasterProgramID -> MasterProgInfo
setMasterProgInfoMasterProgramProgramInfoKey
    (MasterProgInfo a8 a7 a6 a5 a4 a3 a2 _) a =
  MasterProgInfo a8 a7 a6 a5 a4 a3 a2 a

--- id-to-value function for entity `MasterProgInfo`.
masterProgInfoID
  :: MasterProgInfoID -> Database.CDBI.Criteria.Value MasterProgInfoID
masterProgInfoID (MasterProgInfoID key) = Database.CDBI.Criteria.idVal key

--- id-to-int function for entity `MasterProgInfo`.
masterProgInfoKeyToInt :: MasterProgInfoID -> Int
masterProgInfoKeyToInt (MasterProgInfoID key) = key

--- Shows the key of a `MasterProgInfo` entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showMasterProgInfoKey :: MasterProgInfo -> String
showMasterProgInfoKey entry =
  Database.CDBI.ER.showDatabaseKey "MasterProgInfo" masterProgInfoKeyToInt
   (masterProgInfoKey entry)

--- Transforms a string into a key of a `MasterProgInfo` entity.
--- Nothing is returned if the string does not represent a meaningful key.
readMasterProgInfoKey :: String -> Maybe MasterProgInfoID
readMasterProgInfoKey =
  Database.CDBI.ER.readDatabaseKey "MasterProgInfo" MasterProgInfoID

--- Gets all `MasterProgInfo` entities.
queryAllMasterProgInfos :: Database.CDBI.Connection.DBAction [MasterProgInfo]
queryAllMasterProgInfos =
  Database.CDBI.ER.getAllEntries masterProgInfo_CDBI_Description

--- Gets all `MasterProgInfo` entities satisfying a given predicate.
queryCondMasterProgInfo
  :: (MasterProgInfo -> Bool)
  -> Database.CDBI.Connection.DBAction [MasterProgInfo]
queryCondMasterProgInfo =
  Database.CDBI.ER.getCondEntries masterProgInfo_CDBI_Description

--- Gets a `MasterProgInfo` entry by a given key.
getMasterProgInfo
  :: MasterProgInfoID -> Database.CDBI.Connection.DBAction MasterProgInfo
getMasterProgInfo =
  Database.CDBI.ER.getEntryWithKey masterProgInfo_CDBI_Description
   masterProgInfoColumnKey
   masterProgInfoID

--- Inserts a new `MasterProgInfo` entity.
newMasterProgInfoWithMasterProgramProgramInfoKey
  :: String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> MasterProgramID -> Database.CDBI.Connection.DBAction MasterProgInfo
newMasterProgInfoWithMasterProgramProgramInfoKey
    progModules_p
    praktikum_p
    seminar_p
    thesis_p
    allgGrundlagen_p
    anwendungsfach_p
    masterProgramProgramInfoKey_p =
  Database.CDBI.ER.insertNewEntry masterProgInfo_CDBI_Description
   setMasterProgInfoKey
   MasterProgInfoID
   (MasterProgInfo (MasterProgInfoID 0) progModules_p praktikum_p seminar_p
     thesis_p
     allgGrundlagen_p
     anwendungsfach_p
     masterProgramProgramInfoKey_p)

--- Deletes an existing `MasterProgInfo` entry by its key.
deleteMasterProgInfo :: MasterProgInfo -> Database.CDBI.Connection.DBAction ()
deleteMasterProgInfo =
  Database.CDBI.ER.deleteEntry masterProgInfo_CDBI_Description
   masterProgInfoColumnKey
   (masterProgInfoID . masterProgInfoKey)

--- Updates an existing `MasterProgInfo` entry by its key.
updateMasterProgInfo :: MasterProgInfo -> Database.CDBI.Connection.DBAction ()
updateMasterProgInfo =
  Database.CDBI.ER.updateEntry masterProgInfo_CDBI_Description

--- The ER description of the `UnivisInfo` entity.
univisInfo_CDBI_Description
  :: Database.CDBI.Description.EntityDescription UnivisInfo
univisInfo_CDBI_Description =
  Database.CDBI.Description.ED "UnivisInfo"
   [Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeString]
   (\(UnivisInfo (UnivisInfoID key) code term year uRL) ->
     [Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString code
     ,Database.CDBI.Connection.SQLString term
     ,Database.CDBI.Connection.SQLInt year
     ,Database.CDBI.Connection.SQLString uRL])
   (\(UnivisInfo _ code term year uRL) ->
     [Database.CDBI.Connection.SQLNull
     ,Database.CDBI.Connection.SQLString code
     ,Database.CDBI.Connection.SQLString term
     ,Database.CDBI.Connection.SQLInt year
     ,Database.CDBI.Connection.SQLString uRL])
   (\[Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString code
     ,Database.CDBI.Connection.SQLString term
     ,Database.CDBI.Connection.SQLInt year
     ,Database.CDBI.Connection.SQLString uRL] ->
     UnivisInfo (UnivisInfoID key) code term year uRL)

--- The database table of the `UnivisInfo` entity.
univisInfoTable :: Database.CDBI.Description.Table
univisInfoTable = "UnivisInfo"

--- The database column `Key` of the `UnivisInfo` entity.
univisInfoColumnKey :: Database.CDBI.Description.Column UnivisInfoID
univisInfoColumnKey =
  Database.CDBI.Description.Column "\"Key\"" "\"UnivisInfo\".\"Key\""

--- The database column `Code` of the `UnivisInfo` entity.
univisInfoColumnCode :: Database.CDBI.Description.Column String
univisInfoColumnCode =
  Database.CDBI.Description.Column "\"Code\"" "\"UnivisInfo\".\"Code\""

--- The database column `Term` of the `UnivisInfo` entity.
univisInfoColumnTerm :: Database.CDBI.Description.Column String
univisInfoColumnTerm =
  Database.CDBI.Description.Column "\"Term\"" "\"UnivisInfo\".\"Term\""

--- The database column `Year` of the `UnivisInfo` entity.
univisInfoColumnYear :: Database.CDBI.Description.Column Int
univisInfoColumnYear =
  Database.CDBI.Description.Column "\"Year\"" "\"UnivisInfo\".\"Year\""

--- The database column `URL` of the `UnivisInfo` entity.
univisInfoColumnURL :: Database.CDBI.Description.Column String
univisInfoColumnURL =
  Database.CDBI.Description.Column "\"URL\"" "\"UnivisInfo\".\"URL\""

--- The description of the database column `Key` of the `UnivisInfo` entity.
univisInfoKeyColDesc :: Database.CDBI.Description.ColumnDescription UnivisInfoID
univisInfoKeyColDesc =
  Database.CDBI.Description.ColDesc "\"UnivisInfo\".\"Key\""
   Database.CDBI.Connection.SQLTypeInt
   (\(UnivisInfoID key) -> Database.CDBI.Connection.SQLInt key)
   (\(Database.CDBI.Connection.SQLInt key) -> UnivisInfoID key)

--- The description of the database column `Code` of the `UnivisInfo` entity.
univisInfoCodeColDesc :: Database.CDBI.Description.ColumnDescription String
univisInfoCodeColDesc =
  Database.CDBI.Description.ColDesc "\"UnivisInfo\".\"Code\""
   Database.CDBI.Connection.SQLTypeString
   (\code -> Database.CDBI.Connection.SQLString code)
   (\(Database.CDBI.Connection.SQLString code) -> code)

--- The description of the database column `Term` of the `UnivisInfo` entity.
univisInfoTermColDesc :: Database.CDBI.Description.ColumnDescription String
univisInfoTermColDesc =
  Database.CDBI.Description.ColDesc "\"UnivisInfo\".\"Term\""
   Database.CDBI.Connection.SQLTypeString
   (\term -> Database.CDBI.Connection.SQLString term)
   (\(Database.CDBI.Connection.SQLString term) -> term)

--- The description of the database column `Year` of the `UnivisInfo` entity.
univisInfoYearColDesc :: Database.CDBI.Description.ColumnDescription Int
univisInfoYearColDesc =
  Database.CDBI.Description.ColDesc "\"UnivisInfo\".\"Year\""
   Database.CDBI.Connection.SQLTypeInt
   (\year -> Database.CDBI.Connection.SQLInt year)
   (\(Database.CDBI.Connection.SQLInt year) -> year)

--- The description of the database column `URL` of the `UnivisInfo` entity.
univisInfoURLColDesc :: Database.CDBI.Description.ColumnDescription String
univisInfoURLColDesc =
  Database.CDBI.Description.ColDesc "\"UnivisInfo\".\"URL\""
   Database.CDBI.Connection.SQLTypeString
   (\uRL -> Database.CDBI.Connection.SQLString uRL)
   (\(Database.CDBI.Connection.SQLString uRL) -> uRL)

--- Gets the attribute `Key` of the `UnivisInfo` entity.
univisInfoKey :: UnivisInfo -> UnivisInfoID
univisInfoKey (UnivisInfo a _ _ _ _) = a

--- Gets the attribute `Code` of the `UnivisInfo` entity.
univisInfoCode :: UnivisInfo -> String
univisInfoCode (UnivisInfo _ a _ _ _) = a

--- Gets the attribute `Term` of the `UnivisInfo` entity.
univisInfoTerm :: UnivisInfo -> String
univisInfoTerm (UnivisInfo _ _ a _ _) = a

--- Gets the attribute `Year` of the `UnivisInfo` entity.
univisInfoYear :: UnivisInfo -> Int
univisInfoYear (UnivisInfo _ _ _ a _) = a

--- Gets the attribute `URL` of the `UnivisInfo` entity.
univisInfoURL :: UnivisInfo -> String
univisInfoURL (UnivisInfo _ _ _ _ a) = a

--- Sets the attribute `Key` of the `UnivisInfo` entity.
setUnivisInfoKey :: UnivisInfo -> UnivisInfoID -> UnivisInfo
setUnivisInfoKey (UnivisInfo _ b4 b3 b2 b1) a = UnivisInfo a b4 b3 b2 b1

--- Sets the attribute `Code` of the `UnivisInfo` entity.
setUnivisInfoCode :: UnivisInfo -> String -> UnivisInfo
setUnivisInfoCode (UnivisInfo a2 _ b3 b2 b1) a = UnivisInfo a2 a b3 b2 b1

--- Sets the attribute `Term` of the `UnivisInfo` entity.
setUnivisInfoTerm :: UnivisInfo -> String -> UnivisInfo
setUnivisInfoTerm (UnivisInfo a3 a2 _ b2 b1) a = UnivisInfo a3 a2 a b2 b1

--- Sets the attribute `Year` of the `UnivisInfo` entity.
setUnivisInfoYear :: UnivisInfo -> Int -> UnivisInfo
setUnivisInfoYear (UnivisInfo a4 a3 a2 _ b1) a = UnivisInfo a4 a3 a2 a b1

--- Sets the attribute `URL` of the `UnivisInfo` entity.
setUnivisInfoURL :: UnivisInfo -> String -> UnivisInfo
setUnivisInfoURL (UnivisInfo a5 a4 a3 a2 _) a = UnivisInfo a5 a4 a3 a2 a

--- id-to-value function for entity `UnivisInfo`.
univisInfoID :: UnivisInfoID -> Database.CDBI.Criteria.Value UnivisInfoID
univisInfoID (UnivisInfoID key) = Database.CDBI.Criteria.idVal key

--- id-to-int function for entity `UnivisInfo`.
univisInfoKeyToInt :: UnivisInfoID -> Int
univisInfoKeyToInt (UnivisInfoID key) = key

--- Shows the key of a `UnivisInfo` entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showUnivisInfoKey :: UnivisInfo -> String
showUnivisInfoKey entry =
  Database.CDBI.ER.showDatabaseKey "UnivisInfo" univisInfoKeyToInt
   (univisInfoKey entry)

--- Transforms a string into a key of a `UnivisInfo` entity.
--- Nothing is returned if the string does not represent a meaningful key.
readUnivisInfoKey :: String -> Maybe UnivisInfoID
readUnivisInfoKey = Database.CDBI.ER.readDatabaseKey "UnivisInfo" UnivisInfoID

--- Gets all `UnivisInfo` entities.
queryAllUnivisInfos :: Database.CDBI.Connection.DBAction [UnivisInfo]
queryAllUnivisInfos = Database.CDBI.ER.getAllEntries univisInfo_CDBI_Description

--- Gets all `UnivisInfo` entities satisfying a given predicate.
queryCondUnivisInfo
  :: (UnivisInfo -> Bool) -> Database.CDBI.Connection.DBAction [UnivisInfo]
queryCondUnivisInfo =
  Database.CDBI.ER.getCondEntries univisInfo_CDBI_Description

--- Gets a `UnivisInfo` entry by a given key.
getUnivisInfo :: UnivisInfoID -> Database.CDBI.Connection.DBAction UnivisInfo
getUnivisInfo =
  Database.CDBI.ER.getEntryWithKey univisInfo_CDBI_Description
   univisInfoColumnKey
   univisInfoID

--- Inserts a new `UnivisInfo` entity.
newUnivisInfo
  :: String
  -> String -> Int -> String -> Database.CDBI.Connection.DBAction UnivisInfo
newUnivisInfo code_p term_p year_p uRL_p =
  Database.CDBI.ER.insertNewEntry univisInfo_CDBI_Description setUnivisInfoKey
   UnivisInfoID
   (UnivisInfo (UnivisInfoID 0) code_p term_p year_p uRL_p)

--- Deletes an existing `UnivisInfo` entry by its key.
deleteUnivisInfo :: UnivisInfo -> Database.CDBI.Connection.DBAction ()
deleteUnivisInfo =
  Database.CDBI.ER.deleteEntry univisInfo_CDBI_Description univisInfoColumnKey
   (univisInfoID . univisInfoKey)

--- Updates an existing `UnivisInfo` entry by its key.
updateUnivisInfo :: UnivisInfo -> Database.CDBI.Connection.DBAction ()
updateUnivisInfo = Database.CDBI.ER.updateEntry univisInfo_CDBI_Description

--- The ER description of the `Student` entity.
student_CDBI_Description :: Database.CDBI.Description.EntityDescription Student
student_CDBI_Description =
  Database.CDBI.Description.ED "Student"
   [Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeDate]
   (\(Student (StudentID key) email name first tAN lastLogin) ->
     [Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString email
     ,Database.CDBI.Connection.SQLString name
     ,Database.CDBI.Description.sqlString first
     ,Database.CDBI.Connection.SQLString tAN
     ,Database.CDBI.Connection.SQLDate lastLogin])
   (\(Student _ email name first tAN lastLogin) ->
     [Database.CDBI.Connection.SQLNull
     ,Database.CDBI.Connection.SQLString email
     ,Database.CDBI.Connection.SQLString name
     ,Database.CDBI.Description.sqlString first
     ,Database.CDBI.Connection.SQLString tAN
     ,Database.CDBI.Connection.SQLDate lastLogin])
   (\[Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString email
     ,Database.CDBI.Connection.SQLString name
     ,first
     ,Database.CDBI.Connection.SQLString tAN
     ,Database.CDBI.Connection.SQLDate lastLogin] ->
     Student (StudentID key) email name
      (Database.CDBI.Description.fromStringOrNull first)
      tAN
      lastLogin)

--- The database table of the `Student` entity.
studentTable :: Database.CDBI.Description.Table
studentTable = "Student"

--- The database column `Key` of the `Student` entity.
studentColumnKey :: Database.CDBI.Description.Column StudentID
studentColumnKey =
  Database.CDBI.Description.Column "\"Key\"" "\"Student\".\"Key\""

--- The database column `Email` of the `Student` entity.
studentColumnEmail :: Database.CDBI.Description.Column String
studentColumnEmail =
  Database.CDBI.Description.Column "\"Email\"" "\"Student\".\"Email\""

--- The database column `Name` of the `Student` entity.
studentColumnName :: Database.CDBI.Description.Column String
studentColumnName =
  Database.CDBI.Description.Column "\"Name\"" "\"Student\".\"Name\""

--- The database column `First` of the `Student` entity.
studentColumnFirst :: Database.CDBI.Description.Column String
studentColumnFirst =
  Database.CDBI.Description.Column "\"First\"" "\"Student\".\"First\""

--- The database column `TAN` of the `Student` entity.
studentColumnTAN :: Database.CDBI.Description.Column String
studentColumnTAN =
  Database.CDBI.Description.Column "\"TAN\"" "\"Student\".\"TAN\""

--- The database column `LastLogin` of the `Student` entity.
studentColumnLastLogin :: Database.CDBI.Description.Column Data.Time.ClockTime
studentColumnLastLogin =
  Database.CDBI.Description.Column "\"LastLogin\"" "\"Student\".\"LastLogin\""

--- The description of the database column `Key` of the `Student` entity.
studentKeyColDesc :: Database.CDBI.Description.ColumnDescription StudentID
studentKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Student\".\"Key\""
   Database.CDBI.Connection.SQLTypeInt
   (\(StudentID key) -> Database.CDBI.Connection.SQLInt key)
   (\(Database.CDBI.Connection.SQLInt key) -> StudentID key)

--- The description of the database column `Email` of the `Student` entity.
studentEmailColDesc :: Database.CDBI.Description.ColumnDescription String
studentEmailColDesc =
  Database.CDBI.Description.ColDesc "\"Student\".\"Email\""
   Database.CDBI.Connection.SQLTypeString
   (\email -> Database.CDBI.Connection.SQLString email)
   (\(Database.CDBI.Connection.SQLString email) -> email)

--- The description of the database column `Name` of the `Student` entity.
studentNameColDesc :: Database.CDBI.Description.ColumnDescription String
studentNameColDesc =
  Database.CDBI.Description.ColDesc "\"Student\".\"Name\""
   Database.CDBI.Connection.SQLTypeString
   (\name -> Database.CDBI.Connection.SQLString name)
   (\(Database.CDBI.Connection.SQLString name) -> name)

--- The description of the database column `First` of the `Student` entity.
studentFirstColDesc :: Database.CDBI.Description.ColumnDescription String
studentFirstColDesc =
  Database.CDBI.Description.ColDesc "\"Student\".\"First\""
   Database.CDBI.Connection.SQLTypeString
   (\first -> Database.CDBI.Description.sqlString first)
   (\first -> Database.CDBI.Description.fromStringOrNull first)

--- The description of the database column `TAN` of the `Student` entity.
studentTANColDesc :: Database.CDBI.Description.ColumnDescription String
studentTANColDesc =
  Database.CDBI.Description.ColDesc "\"Student\".\"TAN\""
   Database.CDBI.Connection.SQLTypeString
   (\tAN -> Database.CDBI.Connection.SQLString tAN)
   (\(Database.CDBI.Connection.SQLString tAN) -> tAN)

--- The description of the database column `LastLogin` of the `Student` entity.
studentLastLoginColDesc
  :: Database.CDBI.Description.ColumnDescription Data.Time.ClockTime
studentLastLoginColDesc =
  Database.CDBI.Description.ColDesc "\"Student\".\"LastLogin\""
   Database.CDBI.Connection.SQLTypeDate
   (\lastLogin -> Database.CDBI.Connection.SQLDate lastLogin)
   (\(Database.CDBI.Connection.SQLDate lastLogin) -> lastLogin)

--- Gets the attribute `Key` of the `Student` entity.
studentKey :: Student -> StudentID
studentKey (Student a _ _ _ _ _) = a

--- Gets the attribute `Email` of the `Student` entity.
studentEmail :: Student -> String
studentEmail (Student _ a _ _ _ _) = a

--- Gets the attribute `Name` of the `Student` entity.
studentName :: Student -> String
studentName (Student _ _ a _ _ _) = a

--- Gets the attribute `First` of the `Student` entity.
studentFirst :: Student -> String
studentFirst (Student _ _ _ a _ _) = a

--- Gets the attribute `TAN` of the `Student` entity.
studentTAN :: Student -> String
studentTAN (Student _ _ _ _ a _) = a

--- Gets the attribute `LastLogin` of the `Student` entity.
studentLastLogin :: Student -> Data.Time.ClockTime
studentLastLogin (Student _ _ _ _ _ a) = a

--- Sets the attribute `Key` of the `Student` entity.
setStudentKey :: Student -> StudentID -> Student
setStudentKey (Student _ b5 b4 b3 b2 b1) a = Student a b5 b4 b3 b2 b1

--- Sets the attribute `Email` of the `Student` entity.
setStudentEmail :: Student -> String -> Student
setStudentEmail (Student a2 _ b4 b3 b2 b1) a = Student a2 a b4 b3 b2 b1

--- Sets the attribute `Name` of the `Student` entity.
setStudentName :: Student -> String -> Student
setStudentName (Student a3 a2 _ b3 b2 b1) a = Student a3 a2 a b3 b2 b1

--- Sets the attribute `First` of the `Student` entity.
setStudentFirst :: Student -> String -> Student
setStudentFirst (Student a4 a3 a2 _ b2 b1) a = Student a4 a3 a2 a b2 b1

--- Sets the attribute `TAN` of the `Student` entity.
setStudentTAN :: Student -> String -> Student
setStudentTAN (Student a5 a4 a3 a2 _ b1) a = Student a5 a4 a3 a2 a b1

--- Sets the attribute `LastLogin` of the `Student` entity.
setStudentLastLogin :: Student -> Data.Time.ClockTime -> Student
setStudentLastLogin (Student a6 a5 a4 a3 a2 _) a = Student a6 a5 a4 a3 a2 a

--- id-to-value function for entity `Student`.
studentID :: StudentID -> Database.CDBI.Criteria.Value StudentID
studentID (StudentID key) = Database.CDBI.Criteria.idVal key

--- id-to-int function for entity `Student`.
studentKeyToInt :: StudentID -> Int
studentKeyToInt (StudentID key) = key

--- Shows the key of a `Student` entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showStudentKey :: Student -> String
showStudentKey entry =
  Database.CDBI.ER.showDatabaseKey "Student" studentKeyToInt (studentKey entry)

--- Transforms a string into a key of a `Student` entity.
--- Nothing is returned if the string does not represent a meaningful key.
readStudentKey :: String -> Maybe StudentID
readStudentKey = Database.CDBI.ER.readDatabaseKey "Student" StudentID

--- Gets all `Student` entities.
queryAllStudents :: Database.CDBI.Connection.DBAction [Student]
queryAllStudents = Database.CDBI.ER.getAllEntries student_CDBI_Description

--- Gets all `Student` entities satisfying a given predicate.
queryCondStudent
  :: (Student -> Bool) -> Database.CDBI.Connection.DBAction [Student]
queryCondStudent = Database.CDBI.ER.getCondEntries student_CDBI_Description

--- Gets a `Student` entry by a given key.
getStudent :: StudentID -> Database.CDBI.Connection.DBAction Student
getStudent =
  Database.CDBI.ER.getEntryWithKey student_CDBI_Description studentColumnKey
   studentID

--- Inserts a new `Student` entity.
newStudent
  :: String
  -> String
  -> String
  -> String -> Data.Time.ClockTime -> Database.CDBI.Connection.DBAction Student
newStudent email_p name_p first_p tAN_p lastLogin_p =
  Database.CDBI.ER.insertNewEntry student_CDBI_Description setStudentKey
   StudentID
   (Student (StudentID 0) email_p name_p first_p tAN_p lastLogin_p)

--- Deletes an existing `Student` entry by its key.
deleteStudent :: Student -> Database.CDBI.Connection.DBAction ()
deleteStudent =
  Database.CDBI.ER.deleteEntry student_CDBI_Description studentColumnKey
   (studentID . studentKey)

--- Updates an existing `Student` entry by its key.
updateStudent :: Student -> Database.CDBI.Connection.DBAction ()
updateStudent = Database.CDBI.ER.updateEntry student_CDBI_Description

--- The ER description of the `StudentCourse` entity.
studentCourse_CDBI_Description
  :: Database.CDBI.Description.EntityDescription StudentCourse
studentCourse_CDBI_Description =
  Database.CDBI.Description.ED "StudentCourse"
   [Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeDate
   ,Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeInt]
   (\(StudentCourse
       (StudentCourseID key)
       selectDate
       (StudentID studentStudentCoursesKey)
       (ModInstID modInstStudentCourseInstancesKey)) ->
     [Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLDate selectDate
     ,Database.CDBI.Connection.SQLInt studentStudentCoursesKey
     ,Database.CDBI.Connection.SQLInt modInstStudentCourseInstancesKey])
   (\(StudentCourse
       _
       selectDate
       (StudentID studentStudentCoursesKey)
       (ModInstID modInstStudentCourseInstancesKey)) ->
     [Database.CDBI.Connection.SQLNull
     ,Database.CDBI.Connection.SQLDate selectDate
     ,Database.CDBI.Connection.SQLInt studentStudentCoursesKey
     ,Database.CDBI.Connection.SQLInt modInstStudentCourseInstancesKey])
   (\[Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLDate selectDate
     ,Database.CDBI.Connection.SQLInt studentStudentCoursesKey
     ,Database.CDBI.Connection.SQLInt modInstStudentCourseInstancesKey] ->
     StudentCourse (StudentCourseID key) selectDate
      (StudentID studentStudentCoursesKey)
      (ModInstID modInstStudentCourseInstancesKey))

--- The database table of the `StudentCourse` entity.
studentCourseTable :: Database.CDBI.Description.Table
studentCourseTable = "StudentCourse"

--- The database column `Key` of the `StudentCourse` entity.
studentCourseColumnKey :: Database.CDBI.Description.Column StudentCourseID
studentCourseColumnKey =
  Database.CDBI.Description.Column "\"Key\"" "\"StudentCourse\".\"Key\""

--- The database column `SelectDate` of the `StudentCourse` entity.
studentCourseColumnSelectDate :: Database.CDBI.Description.Column Data.Time.ClockTime
studentCourseColumnSelectDate =
  Database.CDBI.Description.Column "\"SelectDate\""
   "\"StudentCourse\".\"SelectDate\""

--- The database column `StudentStudentCoursesKey` of the `StudentCourse` entity.
studentCourseColumnStudentStudentCoursesKey
  :: Database.CDBI.Description.Column StudentID
studentCourseColumnStudentStudentCoursesKey =
  Database.CDBI.Description.Column "\"StudentStudentCoursesKey\""
   "\"StudentCourse\".\"StudentStudentCoursesKey\""

--- The database column `ModInstStudentCourseInstancesKey` of the `StudentCourse` entity.
studentCourseColumnModInstStudentCourseInstancesKey
  :: Database.CDBI.Description.Column ModInstID
studentCourseColumnModInstStudentCourseInstancesKey =
  Database.CDBI.Description.Column "\"ModInstStudentCourseInstancesKey\""
   "\"StudentCourse\".\"ModInstStudentCourseInstancesKey\""

--- The description of the database column `Key` of the `StudentCourse` entity.
studentCourseKeyColDesc
  :: Database.CDBI.Description.ColumnDescription StudentCourseID
studentCourseKeyColDesc =
  Database.CDBI.Description.ColDesc "\"StudentCourse\".\"Key\""
   Database.CDBI.Connection.SQLTypeInt
   (\(StudentCourseID key) -> Database.CDBI.Connection.SQLInt key)
   (\(Database.CDBI.Connection.SQLInt key) -> StudentCourseID key)

--- The description of the database column `SelectDate` of the `StudentCourse` entity.
studentCourseSelectDateColDesc
  :: Database.CDBI.Description.ColumnDescription Data.Time.ClockTime
studentCourseSelectDateColDesc =
  Database.CDBI.Description.ColDesc "\"StudentCourse\".\"SelectDate\""
   Database.CDBI.Connection.SQLTypeDate
   (\selectDate -> Database.CDBI.Connection.SQLDate selectDate)
   (\(Database.CDBI.Connection.SQLDate selectDate) -> selectDate)

--- The description of the database column `StudentStudentCoursesKey` of the `StudentCourse` entity.
studentCourseStudentStudentCoursesKeyColDesc
  :: Database.CDBI.Description.ColumnDescription StudentID
studentCourseStudentStudentCoursesKeyColDesc =
  Database.CDBI.Description.ColDesc
   "\"StudentCourse\".\"StudentStudentCoursesKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(StudentID studentStudentCoursesKey) ->
     Database.CDBI.Connection.SQLInt studentStudentCoursesKey)
   (\(Database.CDBI.Connection.SQLInt studentStudentCoursesKey) ->
     StudentID studentStudentCoursesKey)

--- The description of the database column `ModInstStudentCourseInstancesKey` of the `StudentCourse` entity.
studentCourseModInstStudentCourseInstancesKeyColDesc
  :: Database.CDBI.Description.ColumnDescription ModInstID
studentCourseModInstStudentCourseInstancesKeyColDesc =
  Database.CDBI.Description.ColDesc
   "\"StudentCourse\".\"ModInstStudentCourseInstancesKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(ModInstID modInstStudentCourseInstancesKey) ->
     Database.CDBI.Connection.SQLInt modInstStudentCourseInstancesKey)
   (\(Database.CDBI.Connection.SQLInt modInstStudentCourseInstancesKey) ->
     ModInstID modInstStudentCourseInstancesKey)

--- Gets the attribute `Key` of the `StudentCourse` entity.
studentCourseKey :: StudentCourse -> StudentCourseID
studentCourseKey (StudentCourse a _ _ _) = a

--- Gets the attribute `SelectDate` of the `StudentCourse` entity.
studentCourseSelectDate :: StudentCourse -> Data.Time.ClockTime
studentCourseSelectDate (StudentCourse _ a _ _) = a

--- Gets the attribute `StudentStudentCoursesKey` of the `StudentCourse` entity.
studentCourseStudentStudentCoursesKey :: StudentCourse -> StudentID
studentCourseStudentStudentCoursesKey (StudentCourse _ _ a _) = a

--- Gets the attribute `ModInstStudentCourseInstancesKey` of the `StudentCourse` entity.
studentCourseModInstStudentCourseInstancesKey :: StudentCourse -> ModInstID
studentCourseModInstStudentCourseInstancesKey (StudentCourse _ _ _ a) = a

--- Sets the attribute `Key` of the `StudentCourse` entity.
setStudentCourseKey :: StudentCourse -> StudentCourseID -> StudentCourse
setStudentCourseKey (StudentCourse _ b3 b2 b1) a = StudentCourse a b3 b2 b1

--- Sets the attribute `SelectDate` of the `StudentCourse` entity.
setStudentCourseSelectDate :: StudentCourse -> Data.Time.ClockTime -> StudentCourse
setStudentCourseSelectDate (StudentCourse a2 _ b2 b1) a =
  StudentCourse a2 a b2 b1

--- Sets the attribute `StudentStudentCoursesKey` of the `StudentCourse` entity.
setStudentCourseStudentStudentCoursesKey
  :: StudentCourse -> StudentID -> StudentCourse
setStudentCourseStudentStudentCoursesKey (StudentCourse a3 a2 _ b1) a =
  StudentCourse a3 a2 a b1

--- Sets the attribute `ModInstStudentCourseInstancesKey` of the `StudentCourse` entity.
setStudentCourseModInstStudentCourseInstancesKey
  :: StudentCourse -> ModInstID -> StudentCourse
setStudentCourseModInstStudentCourseInstancesKey (StudentCourse a4 a3 a2 _) a =
  StudentCourse a4 a3 a2 a

--- id-to-value function for entity `StudentCourse`.
studentCourseID
  :: StudentCourseID -> Database.CDBI.Criteria.Value StudentCourseID
studentCourseID (StudentCourseID key) = Database.CDBI.Criteria.idVal key

--- id-to-int function for entity `StudentCourse`.
studentCourseKeyToInt :: StudentCourseID -> Int
studentCourseKeyToInt (StudentCourseID key) = key

--- Shows the key of a `StudentCourse` entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showStudentCourseKey :: StudentCourse -> String
showStudentCourseKey entry =
  Database.CDBI.ER.showDatabaseKey "StudentCourse" studentCourseKeyToInt
   (studentCourseKey entry)

--- Transforms a string into a key of a `StudentCourse` entity.
--- Nothing is returned if the string does not represent a meaningful key.
readStudentCourseKey :: String -> Maybe StudentCourseID
readStudentCourseKey =
  Database.CDBI.ER.readDatabaseKey "StudentCourse" StudentCourseID

--- Gets all `StudentCourse` entities.
queryAllStudentCourses :: Database.CDBI.Connection.DBAction [StudentCourse]
queryAllStudentCourses =
  Database.CDBI.ER.getAllEntries studentCourse_CDBI_Description

--- Gets all `StudentCourse` entities satisfying a given predicate.
queryCondStudentCourse
  :: (StudentCourse -> Bool)
  -> Database.CDBI.Connection.DBAction [StudentCourse]
queryCondStudentCourse =
  Database.CDBI.ER.getCondEntries studentCourse_CDBI_Description

--- Gets a `StudentCourse` entry by a given key.
getStudentCourse
  :: StudentCourseID -> Database.CDBI.Connection.DBAction StudentCourse
getStudentCourse =
  Database.CDBI.ER.getEntryWithKey studentCourse_CDBI_Description
   studentCourseColumnKey
   studentCourseID

--- Inserts a new `StudentCourse` entity.
newStudentCourseWithStudentStudentCoursesKeyWithModInstStudentCourseInstancesKey
  :: Data.Time.ClockTime
  -> StudentID -> ModInstID -> Database.CDBI.Connection.DBAction StudentCourse
newStudentCourseWithStudentStudentCoursesKeyWithModInstStudentCourseInstancesKey
    selectDate_p studentStudentCoursesKey_p modInstStudentCourseInstancesKey_p =
  Database.CDBI.ER.insertNewEntry studentCourse_CDBI_Description
   setStudentCourseKey
   StudentCourseID
   (StudentCourse (StudentCourseID 0) selectDate_p studentStudentCoursesKey_p
     modInstStudentCourseInstancesKey_p)

--- Deletes an existing `StudentCourse` entry by its key.
deleteStudentCourse :: StudentCourse -> Database.CDBI.Connection.DBAction ()
deleteStudentCourse =
  Database.CDBI.ER.deleteEntry studentCourse_CDBI_Description
   studentCourseColumnKey
   (studentCourseID . studentCourseKey)

--- Updates an existing `StudentCourse` entry by its key.
updateStudentCourse :: StudentCourse -> Database.CDBI.Connection.DBAction ()
updateStudentCourse =
  Database.CDBI.ER.updateEntry studentCourse_CDBI_Description

--- Generates a new database (name provided as the parameter) and
--- creates its schema.
createNewDB :: String -> IO ()
createNewDB dbfile =
  do conn <- Database.CDBI.Connection.connectSQLite dbfile
     Database.CDBI.Connection.writeConnection cstr conn
     Database.CDBI.Connection.disconnect conn
  where
    cstr =
      unlines
       ["create table 'Prerequisites'('ModDataPrerequisitesKey1' int REFERENCES 'ModData'(Key) not null ,'ModDataPrerequisitesKey' int REFERENCES 'ModData'(Key) not null, primary key ('ModDataPrerequisitesKey1', 'ModDataPrerequisitesKey'));"
       ,"create table 'Categorizing'('ModDataCategorizingKey' int REFERENCES 'ModData'(Key) not null ,'CategoryCategorizingKey' int REFERENCES 'Category'(Key) not null, primary key ('ModDataCategorizingKey', 'CategoryCategorizingKey'));"
       ,"create table 'StudyProgram'('Key' integer primary key ,'Name' string not null ,'NameE' string ,'ShortName' string unique not null ,'ProgKey' string unique not null ,'Position' not null);"
       ,"create table 'Category'('Key' integer primary key ,'Name' string not null ,'NameE' string ,'ShortName' string not null ,'Comment' string ,'MinECTS' int not null ,'MaxECTS' int not null ,'Position' not null ,'StudyProgramProgramCategoriesKey' int REFERENCES 'StudyProgram'(Key) not null);"
       ,"create table 'MasterCoreArea'('Key' integer primary key ,'Name' string not null ,'ShortName' string not null ,'Description' string ,'AreaKey' string unique not null ,'Position' int not null);"
       ,"create table 'User'('Key' integer primary key ,'Login' string unique not null ,'Name' string not null ,'First' string ,'Title' string ,'Email' string ,'Url' string ,'Password' string ,'LastLogin' string not null);"
       ,"create table 'ModData'('Key' integer primary key ,'Code' string unique not null ,'NameG' string not null ,'NameE' string ,'Cycle' string ,'Presence' string ,'ECTS' int not null ,'Workload' string ,'Length' int not null ,'URL' string ,'Visible' string not null ,'UserResponsibleKey' int REFERENCES 'User'(Key) not null);"
       ,"create table 'ModDescr'('Key' integer primary key ,'Language' string not null ,'ShortDesc' string ,'Objectives' string ,'Contents' string ,'Prereq' string ,'Exam' string ,'Methods' string ,'Use' string ,'Literature' string ,'Links' string ,'Comments' string ,'ModDataDataDescKey' int REFERENCES 'ModData'(Key) unique not null);"
       ,"create table 'ModInst'('Key' integer primary key ,'Term' string not null ,'Year' int not null ,'UserLecturerModsKey' int REFERENCES 'User'(Key) not null ,'ModDataModuleInstancesKey' int REFERENCES 'ModData'(Key) not null);"
       ,"create table 'AdvisorStudyProgram'('Key' integer primary key ,'Name' string not null ,'Term' string not null ,'Year' int not null ,'Desc' string ,'Prereq' string ,'Comments' string ,'Visible' string not null ,'UserStudyAdvisingKey' int REFERENCES 'User'(Key) not null ,'StudyProgramStudyProgramsAdvisedKey' int REFERENCES 'StudyProgram'(Key) not null);"
       ,"create table 'AdvisorModule'('Key' integer primary key ,'Mandatory' string not null ,'AdvisorStudyProgramAdvisorProgramModulesKey' int REFERENCES 'AdvisorStudyProgram'(Key) not null ,'CategoryAdvisorCategorizingKey' int REFERENCES 'Category'(Key) not null ,'ModInstAdvisedProgramModuleInstancesKey' int REFERENCES 'ModInst'(Key) not null);"
       ,"create table 'MasterProgram'('Key' integer primary key ,'Name' string not null ,'Term' string not null ,'Year' int not null ,'Desc' string ,'Prereq' string ,'Comments' string ,'Visible' string not null ,'UserAdvisingKey' int REFERENCES 'User'(Key) not null ,'MasterCoreAreaAreaProgramsKey' int REFERENCES 'MasterCoreArea'(Key) not null);"
       ,"create table 'MasterProgInfo'('Key' integer primary key ,'ProgModules' string not null ,'Praktikum' string ,'Seminar' string ,'Thesis' string ,'AllgGrundlagen' string ,'Anwendungsfach' string ,'MasterProgramProgramInfoKey' int REFERENCES 'MasterProgram'(Key) not null);"
       ,"create table 'UnivisInfo'('Key' integer primary key ,'Code' string not null ,'Term' string not null ,'Year' not null ,'URL' string not null);"
       ,"create table 'Student'('Key' integer primary key ,'Email' string unique not null ,'Name' string not null ,'First' string ,'TAN' string not null ,'LastLogin' string not null);"
       ,"create table 'StudentCourse'('Key' integer primary key ,'SelectDate' string not null ,'StudentStudentCoursesKey' int REFERENCES 'Student'(Key) not null ,'ModInstStudentCourseInstancesKey' int REFERENCES 'ModInst'(Key) not null);"]

--- Saves complete database in storage dir.
saveDB :: IO ()
saveDB = saveDBTo storageDir

--- Saves complete database as term files into an existing directory
--- provided as a parameter.
saveDBTo :: String -> IO ()
saveDBTo dir =
  do Database.CDBI.ER.saveDBTerms prerequisites_CDBI_Description sqliteDBFile
      dir
     Database.CDBI.ER.saveDBTerms categorizing_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.saveDBTerms studyProgram_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.saveDBTerms category_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.saveDBTerms masterCoreArea_CDBI_Description sqliteDBFile
      dir
     Database.CDBI.ER.saveDBTerms user_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.saveDBTerms modData_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.saveDBTerms modDescr_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.saveDBTerms modInst_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.saveDBTerms advisorStudyProgram_CDBI_Description
      sqliteDBFile
      dir
     Database.CDBI.ER.saveDBTerms advisorModule_CDBI_Description sqliteDBFile
      dir
     Database.CDBI.ER.saveDBTerms masterProgram_CDBI_Description sqliteDBFile
      dir
     Database.CDBI.ER.saveDBTerms masterProgInfo_CDBI_Description sqliteDBFile
      dir
     Database.CDBI.ER.saveDBTerms univisInfo_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.saveDBTerms student_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.saveDBTerms studentCourse_CDBI_Description sqliteDBFile
      dir

--- Restores complete database from term files in storage dir.
restoreDB :: IO ()
restoreDB = restoreDBFrom storageDir

--- Restores complete database from term files which are stored
--- in a directory provided as a parameter.
restoreDBFrom :: String -> IO ()
restoreDBFrom dir =
  do Database.CDBI.ER.restoreDBTerms studyProgram_CDBI_Description sqliteDBFile
      dir
     Database.CDBI.ER.restoreDBTerms category_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.restoreDBTerms masterCoreArea_CDBI_Description
      sqliteDBFile
      dir
     Database.CDBI.ER.restoreDBTerms user_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.restoreDBTerms modData_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.restoreDBTerms modDescr_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.restoreDBTerms modInst_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.restoreDBTerms categorizing_CDBI_Description sqliteDBFile
      dir
     Database.CDBI.ER.restoreDBTerms advisorStudyProgram_CDBI_Description
      sqliteDBFile
      dir
     Database.CDBI.ER.restoreDBTerms advisorModule_CDBI_Description sqliteDBFile
      dir
     Database.CDBI.ER.restoreDBTerms masterProgram_CDBI_Description sqliteDBFile
      dir
     Database.CDBI.ER.restoreDBTerms masterProgInfo_CDBI_Description
      sqliteDBFile
      dir
     Database.CDBI.ER.restoreDBTerms univisInfo_CDBI_Description sqliteDBFile
      dir
     Database.CDBI.ER.restoreDBTerms prerequisites_CDBI_Description sqliteDBFile
      dir
     Database.CDBI.ER.restoreDBTerms student_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.restoreDBTerms studentCourse_CDBI_Description sqliteDBFile
      dir

--- Runs a DB action (typically a query).
runQ :: Database.CDBI.Connection.DBAction a -> IO a
runQ = Database.CDBI.ER.runQueryOnDB sqliteDBFile

--- Runs a DB action as a transaction.
runT
  :: Database.CDBI.Connection.DBAction a
  -> IO (Database.CDBI.Connection.SQLResult a)
runT = Database.CDBI.ER.runTransactionOnDB sqliteDBFile

--- Runs a DB action as a transaction. Emits an error in case of failure.
runJustT :: Database.CDBI.Connection.DBAction a -> IO a
runJustT = Database.CDBI.ER.runJustTransactionOnDB sqliteDBFile
