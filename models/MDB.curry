module MDB (
 StudyProgram, Category, MasterCoreArea, User, ModData, ModDescr, ModInst,
 MasterProgram, MasterProgInfo, UnivisInfo, StudyProgramKey, CategoryKey,
 MasterCoreAreaKey, UserKey, ModDataKey, ModDescrKey, ModInstKey,
 MasterProgramKey, MasterProgInfoKey, UnivisInfoKey, Categorizing, studyProgramName,
 setStudyProgramName, studyProgramShortName, setStudyProgramShortName,
 studyProgramProgKey, setStudyProgramProgKey, studyProgramURLKey,
 setStudyProgramURLKey, studyProgramPosition, setStudyProgramPosition,
 categoryName, setCategoryName, categoryShortName, setCategoryShortName,
 categoryCatKey, setCategoryCatKey, categoryPosition, setCategoryPosition,
 categoryStudyProgramProgramCategoriesKey,
 setCategoryStudyProgramProgramCategoriesKey, masterCoreAreaName,
 setMasterCoreAreaName, masterCoreAreaShortName, setMasterCoreAreaShortName,
 masterCoreAreaDescription, setMasterCoreAreaDescription,
 masterCoreAreaAreaKey, setMasterCoreAreaAreaKey, masterCoreAreaPosition,
 setMasterCoreAreaPosition, userLogin, setUserLogin, userName, setUserName,
 userFirst, setUserFirst, userTitle, setUserTitle, userEmail, setUserEmail,
 userUrl, setUserUrl, userPassword, setUserPassword, userLastLogin,
 setUserLastLogin, modDataCode, setModDataCode, modDataNameG, setModDataNameG,
 modDataNameE, setModDataNameE, modDataCycle, setModDataCycle,
 modDataPresence, setModDataPresence, modDataECTS, setModDataECTS,
 modDataWorkload, setModDataWorkload, modDataLength, setModDataLength,
 modDataURL, setModDataURL, modDataVisible, setModDataVisible,
 modDataUserResponsibleKey, setModDataUserResponsibleKey, modDescrLanguage,
 setModDescrLanguage, modDescrShortDesc, setModDescrShortDesc,
 modDescrObjectives, setModDescrObjectives, modDescrContents,
 setModDescrContents, modDescrPrereq, setModDescrPrereq, modDescrExam,
 setModDescrExam, modDescrMethods, setModDescrMethods, modDescrUse,
 setModDescrUse, modDescrLiterature, setModDescrLiterature, modDescrLinks,
 setModDescrLinks, modDescrComments, setModDescrComments,
 modDescrModDataDataDescKey, setModDescrModDataDataDescKey, modInstTerm,
 setModInstTerm, modInstYear, setModInstYear, modInstSemester,
 modInstUserLecturerModsKey,
 setModInstUserLecturerModsKey, modInstModDataModuleInstancesKey,
 setModInstModDataModuleInstancesKey, masterProgramName, setMasterProgramName,
 masterProgramTerm, setMasterProgramTerm, masterProgramYear,
 setMasterProgramYear, masterProgramDesc, setMasterProgramDesc,
 masterProgramPrereq, setMasterProgramPrereq, masterProgramComments,
 setMasterProgramComments, masterProgramVisible, setMasterProgramVisible,
 masterProgramUserAdvisingKey, setMasterProgramUserAdvisingKey,
 masterProgramMasterCoreAreaAreaProgramsKey,
 setMasterProgramMasterCoreAreaAreaProgramsKey, masterProgInfoProgModules,
 setMasterProgInfoProgModules, masterProgInfoPraktikum,
 setMasterProgInfoPraktikum, masterProgInfoSeminar, setMasterProgInfoSeminar,
 masterProgInfoThesis, setMasterProgInfoThesis, masterProgInfoAllgGrundlagen,
 setMasterProgInfoAllgGrundlagen, masterProgInfoAnwendungsfach,
 setMasterProgInfoAnwendungsfach, masterProgInfoMasterProgramProgramInfoKey,
 setMasterProgInfoMasterProgramProgramInfoKey, univisInfoCode,
 setUnivisInfoCode, univisInfoTerm, setUnivisInfoTerm, univisInfoYear,
 setUnivisInfoYear, univisInfoURL, setUnivisInfoURL, studyProgram,
 studyProgramKey, showStudyProgramKey, readStudyProgramKey, newStudyProgram,
 updateStudyProgram, deleteStudyProgram, getStudyProgram,
 queryAllStudyPrograms, queryCondStudyProgram, category, categoryKey,
 showCategoryKey, readCategoryKey,
 newCategoryWithStudyProgramProgramCategoriesKey, updateCategory,
 deleteCategory, getCategory, queryAllCategorys, queryCondCategory,
 masterCoreArea, masterCoreAreaKey, showMasterCoreAreaKey,
 readMasterCoreAreaKey, newMasterCoreArea, updateMasterCoreArea,
 deleteMasterCoreArea, getMasterCoreArea, queryAllMasterCoreAreas,
 queryCondMasterCoreArea, user, userKey, showUserKey, readUserKey, newUser,
 updateUser, deleteUser, getUser, queryAllUsers, queryCondUser, modData,
 modDataKey, showModDataKey, readModDataKey, newModDataWithUserResponsibleKey,
 updateModData, deleteModData, getModData, queryAllModDatas, queryCondModData,
 queryModDataOfUser, queryModDataWithCode,
 modDescr, modDescrKey, showModDescrKey, readModDescrKey,
 newModDescrWithModDataDataDescKey, updateModDescr, deleteModDescr,
 getModDescr, queryAllModDescrs, queryCondModDescr, queryDescriptionOfMod,
 modInst, modInstKey,
 showModInstKey, readModInstKey,
 newModInstWithUserLecturerModsKeyWithModDataModuleInstancesKey,
 updateModInst, deleteModInst, getModInst, queryAllModInsts, queryCondModInst,
 queryInstancesOfMod,
 masterProgram, masterProgramKey, showMasterProgramKey, readMasterProgramKey,
 newMasterProgramWithUserAdvisingKeyWithMasterCoreAreaAreaProgramsKey,
 updateMasterProgram, deleteMasterProgram, getMasterProgram,
 queryAllMasterPrograms, queryCondMasterProgram, queryMasterProgramOfUser,
 getMasterProgramKeysOfModInst,
 masterProgInfo,
 masterProgInfoKey, showMasterProgInfoKey, readMasterProgInfoKey,
 newMasterProgInfoWithMasterProgramProgramInfoKey, updateMasterProgInfo,
 deleteMasterProgInfo, getMasterProgInfo, queryAllMasterProgInfos,
 queryCondMasterProgInfo, queryInfoOfMasterProgram,
 univisInfo, univisInfoKey, showUnivisInfoKey,
 readUnivisInfoKey, newUnivisInfo, updateUnivisInfo, deleteUnivisInfo,
 getUnivisInfo, queryAllUnivisInfos, queryCondUnivisInfo, queryUnivisURL,
 queryHasUnivisEntry, categorizingModDataCategorizingKey,
 categorizingCategoryCategorizingKey, categorizing, newCategorizing,
 deleteCategorizing, getModDataCategorys, getModDataKeyCategorys,
 queryAllCategorizings, queryCondCategorizing, queryModDataKeysOfCategory,
 programInfo, withProgInfo, programInfoOf,
 areaPrograms, withProgram, ofCoreArea, advising, organizes, organizedBy,
 moduleInstances, instOfModule, withModule, lecturerMods, instOfLecturer,
 withLecturer, dataDesc, withDesc, descOf, belongsTo, contains, responsible,
 responsibleFor, managedBy, programCategories, withCategory, ofProgram,
 checkAllData, checkCategorizing, checkStudyProgram, checkCategory,
 checkMasterCoreArea, checkUser, checkModData, checkModDescr, checkModInst,
 checkMasterProgram, checkMasterProgInfo, checkUnivisInfo,
 saveAllData, restoreAllData
 ) where

import ERDGeneric
import KeyDatabase
import Time
import ConfigMDB
import ReadShowTerm

data StudyProgram
 = StudyProgram ERDGeneric.Key String String String String Int 
type StudyProgramTuple = (String,String,String,String,Int)
data Category
 = Category ERDGeneric.Key String String String Int ERDGeneric.Key 
type CategoryTuple = (String,String,String,Int,ERDGeneric.Key)
data MasterCoreArea
 = MasterCoreArea ERDGeneric.Key String String String String Int 
type MasterCoreAreaTuple = (String,String,String,String,Int)
data User
 = User ERDGeneric.Key String String String String String String String
    Time.CalendarTime 
type UserTuple
 = (String,String,String,String,String,String,String,Time.CalendarTime)
data ModData
 = ModData ERDGeneric.Key String String String String String Int String Int
    String Bool ERDGeneric.Key 
type ModDataTuple
 = (String,String,String,String,String,Int,String,Int,String,Bool
   ,ERDGeneric.Key)
data ModDescr
 = ModDescr ERDGeneric.Key String String String String String String String
    String String String String ERDGeneric.Key 
type ModDescrTuple
 = (String,String,String,String,String,String,String,String,String,String
   ,String,ERDGeneric.Key)
data ModInst
 = ModInst ERDGeneric.Key String Int ERDGeneric.Key ERDGeneric.Key 
type ModInstTuple = (String,Int,ERDGeneric.Key,ERDGeneric.Key)
data MasterProgram
 = MasterProgram ERDGeneric.Key String String Int String String String Bool
    ERDGeneric.Key ERDGeneric.Key 
type MasterProgramTuple
 = (String,String,Int,String,String,String,Bool,ERDGeneric.Key,ERDGeneric.Key)
data MasterProgInfo
 = MasterProgInfo ERDGeneric.Key String String String String String String
    ERDGeneric.Key 
type MasterProgInfoTuple
 = (String,String,String,String,String,String,ERDGeneric.Key)
data UnivisInfo = UnivisInfo ERDGeneric.Key String String Int String 
type UnivisInfoTuple = (String,String,Int,String)
data StudyProgramKey = StudyProgramKey ERDGeneric.Key 
data CategoryKey = CategoryKey ERDGeneric.Key 
data MasterCoreAreaKey = MasterCoreAreaKey ERDGeneric.Key 
data UserKey = UserKey ERDGeneric.Key 
data ModDataKey = ModDataKey ERDGeneric.Key 
data ModDescrKey = ModDescrKey ERDGeneric.Key 
data ModInstKey = ModInstKey ERDGeneric.Key 
data MasterProgramKey = MasterProgramKey ERDGeneric.Key 
data MasterProgInfoKey = MasterProgInfoKey ERDGeneric.Key 
data UnivisInfoKey = UnivisInfoKey ERDGeneric.Key 
data Categorizing = Categorizing ERDGeneric.Key ERDGeneric.Key 
type CategorizingTuple = (ERDGeneric.Key,ERDGeneric.Key)

--- Transforms entity StudyProgram into tuple representation.
studyProgram2tuple :: StudyProgram -> StudyProgramTuple
studyProgram2tuple (StudyProgram _ x2 x3 x4 x5 x6) = (x2,x3,x4,x5,x6)

--- Transforms key and tuple into a StudyProgram entity.
keytuple2StudyProgram :: ERDGeneric.Key -> StudyProgramTuple -> StudyProgram
keytuple2StudyProgram x1 (x2 ,x3 ,x4 ,x5 ,x6) = StudyProgram x1 x2 x3 x4 x5 x6

--- Transforms entity Category into tuple representation.
category2tuple :: Category -> CategoryTuple
category2tuple (Category _ x2 x3 x4 x5 x6) = (x2,x3,x4,x5,x6)

--- Transforms key and tuple into a Category entity.
keytuple2Category :: ERDGeneric.Key -> CategoryTuple -> Category
keytuple2Category x1 (x2 ,x3 ,x4 ,x5 ,x6) = Category x1 x2 x3 x4 x5 x6

--- Transforms entity MasterCoreArea into tuple representation.
masterCoreArea2tuple :: MasterCoreArea -> MasterCoreAreaTuple
masterCoreArea2tuple (MasterCoreArea _ x2 x3 x4 x5 x6) = (x2,x3,x4,x5,x6)

--- Transforms key and tuple into a MasterCoreArea entity.
keytuple2MasterCoreArea
 :: ERDGeneric.Key -> MasterCoreAreaTuple -> MasterCoreArea
keytuple2MasterCoreArea x1 (x2 ,x3 ,x4 ,x5 ,x6) =
  MasterCoreArea x1 x2 x3 x4 x5 x6

--- Transforms entity User into tuple representation.
user2tuple :: User -> UserTuple
user2tuple (User _ x2 x3 x4 x5 x6 x7 x8 x9) = (x2,x3,x4,x5,x6,x7,x8,x9)

--- Transforms key and tuple into a User entity.
keytuple2User :: ERDGeneric.Key -> UserTuple -> User
keytuple2User x1 (x2 ,x3 ,x4 ,x5 ,x6 ,x7 ,x8 ,x9) =
  User x1 x2 x3 x4 x5 x6 x7 x8 x9

--- Transforms entity ModData into tuple representation.
modData2tuple :: ModData -> ModDataTuple
modData2tuple (ModData _ x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) =
  (x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12)

--- Transforms key and tuple into a ModData entity.
keytuple2ModData :: ERDGeneric.Key -> ModDataTuple -> ModData
keytuple2ModData x1 (x2 ,x3 ,x4 ,x5 ,x6 ,x7 ,x8 ,x9 ,x10 ,x11 ,x12) =
  ModData x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12

--- Transforms entity ModDescr into tuple representation.
modDescr2tuple :: ModDescr -> ModDescrTuple
modDescr2tuple (ModDescr _ x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) =
  (x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13)

--- Transforms key and tuple into a ModDescr entity.
keytuple2ModDescr :: ERDGeneric.Key -> ModDescrTuple -> ModDescr
keytuple2ModDescr x1 (x2 ,x3 ,x4 ,x5 ,x6 ,x7 ,x8 ,x9 ,x10 ,x11 ,x12 ,x13) =
  ModDescr x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13

--- Transforms entity ModInst into tuple representation.
modInst2tuple :: ModInst -> ModInstTuple
modInst2tuple (ModInst _ x2 x3 x4 x5) = (x2,x3,x4,x5)

--- Transforms key and tuple into a ModInst entity.
keytuple2ModInst :: ERDGeneric.Key -> ModInstTuple -> ModInst
keytuple2ModInst x1 (x2 ,x3 ,x4 ,x5) = ModInst x1 x2 x3 x4 x5

--- Transforms entity MasterProgram into tuple representation.
masterProgram2tuple :: MasterProgram -> MasterProgramTuple
masterProgram2tuple (MasterProgram _ x2 x3 x4 x5 x6 x7 x8 x9 x10) =
  (x2,x3,x4,x5,x6,x7,x8,x9,x10)

--- Transforms key and tuple into a MasterProgram entity.
keytuple2MasterProgram
 :: ERDGeneric.Key -> MasterProgramTuple -> MasterProgram
keytuple2MasterProgram x1 (x2 ,x3 ,x4 ,x5 ,x6 ,x7 ,x8 ,x9 ,x10) =
  MasterProgram x1 x2 x3 x4 x5 x6 x7 x8 x9 x10

--- Transforms entity MasterProgInfo into tuple representation.
masterProgInfo2tuple :: MasterProgInfo -> MasterProgInfoTuple
masterProgInfo2tuple (MasterProgInfo _ x2 x3 x4 x5 x6 x7 x8) =
  (x2,x3,x4,x5,x6,x7,x8)

--- Transforms key and tuple into a MasterProgInfo entity.
keytuple2MasterProgInfo
 :: ERDGeneric.Key -> MasterProgInfoTuple -> MasterProgInfo
keytuple2MasterProgInfo x1 (x2 ,x3 ,x4 ,x5 ,x6 ,x7 ,x8) =
  MasterProgInfo x1 x2 x3 x4 x5 x6 x7 x8

--- Transforms entity UnivisInfo into tuple representation.
univisInfo2tuple :: UnivisInfo -> UnivisInfoTuple
univisInfo2tuple (UnivisInfo _ x2 x3 x4 x5) = (x2,x3,x4,x5)

--- Transforms key and tuple into a UnivisInfo entity.
keytuple2UnivisInfo :: ERDGeneric.Key -> UnivisInfoTuple -> UnivisInfo
keytuple2UnivisInfo x1 (x2 ,x3 ,x4 ,x5) = UnivisInfo x1 x2 x3 x4 x5

--- Transforms relationship entity Categorizing into tuple representation.
categorizing2tuple :: Categorizing -> CategorizingTuple
categorizing2tuple (Categorizing x1 x2) = (x1,x2)

--- Transforms key and tuple into a Categorizing relationship entity.
keytuple2Categorizing :: ERDGeneric.Key -> CategorizingTuple -> Categorizing
keytuple2Categorizing _ (x1 ,x2) = Categorizing x1 x2

--- Sets the value of attribute "ModDataCategorizingKey" in a Categorizing entity.
setCategorizingModDataCategorizingKey
 :: Categorizing -> ModDataKey -> Categorizing
setCategorizingModDataCategorizingKey (Categorizing _ x2) x =
  Categorizing (modDataKeyToKey x) x2

--- Sets the value of attribute "CategoryCategorizingKey" in a Categorizing entity.
setCategorizingCategoryCategorizingKey
 :: Categorizing -> CategoryKey -> Categorizing
setCategorizingCategoryCategorizingKey (Categorizing x1 _) x =
  Categorizing x1 (categoryKeyToKey x)

--- Sets the value of attribute "Key" in a StudyProgram entity.
setStudyProgramKey :: StudyProgram -> ERDGeneric.Key -> StudyProgram
setStudyProgramKey (StudyProgram _ x2 x3 x4 x5 x6) x =
  StudyProgram x x2 x3 x4 x5 x6

--- Gets the value of attribute "Name" of a StudyProgram entity.
studyProgramName :: StudyProgram -> String
studyProgramName (StudyProgram _ x _ _ _ _) = x

--- Sets the value of attribute "Name" in a StudyProgram entity.
setStudyProgramName :: StudyProgram -> String -> StudyProgram
setStudyProgramName (StudyProgram x1 _ x3 x4 x5 x6) x =
  StudyProgram x1 x x3 x4 x5 x6

--- Gets the value of attribute "ShortName" of a StudyProgram entity.
studyProgramShortName :: StudyProgram -> String
studyProgramShortName (StudyProgram _ _ x _ _ _) = x

--- Sets the value of attribute "ShortName" in a StudyProgram entity.
setStudyProgramShortName :: StudyProgram -> String -> StudyProgram
setStudyProgramShortName (StudyProgram x1 x2 _ x4 x5 x6) x =
  StudyProgram x1 x2 x x4 x5 x6

--- Gets the value of attribute "ProgKey" of a StudyProgram entity.
studyProgramProgKey :: StudyProgram -> String
studyProgramProgKey (StudyProgram _ _ _ x _ _) = x

--- Sets the value of attribute "ProgKey" in a StudyProgram entity.
setStudyProgramProgKey :: StudyProgram -> String -> StudyProgram
setStudyProgramProgKey (StudyProgram x1 x2 x3 _ x5 x6) x =
  StudyProgram x1 x2 x3 x x5 x6

--- Gets the value of attribute "URLKey" of a StudyProgram entity.
studyProgramURLKey :: StudyProgram -> String
studyProgramURLKey (StudyProgram _ _ _ _ x _) = x

--- Sets the value of attribute "URLKey" in a StudyProgram entity.
setStudyProgramURLKey :: StudyProgram -> String -> StudyProgram
setStudyProgramURLKey (StudyProgram x1 x2 x3 x4 _ x6) x =
  StudyProgram x1 x2 x3 x4 x x6

--- Gets the value of attribute "Position" of a StudyProgram entity.
studyProgramPosition :: StudyProgram -> Int
studyProgramPosition (StudyProgram _ _ _ _ _ x) = x

--- Sets the value of attribute "Position" in a StudyProgram entity.
setStudyProgramPosition :: StudyProgram -> Int -> StudyProgram
setStudyProgramPosition (StudyProgram x1 x2 x3 x4 x5 _) x =
  StudyProgram x1 x2 x3 x4 x5 x

--- Sets the value of attribute "Key" in a Category entity.
setCategoryKey :: Category -> ERDGeneric.Key -> Category
setCategoryKey (Category _ x2 x3 x4 x5 x6) x = Category x x2 x3 x4 x5 x6

--- Gets the value of attribute "Name" of a Category entity.
categoryName :: Category -> String
categoryName (Category _ x _ _ _ _) = x

--- Sets the value of attribute "Name" in a Category entity.
setCategoryName :: Category -> String -> Category
setCategoryName (Category x1 _ x3 x4 x5 x6) x = Category x1 x x3 x4 x5 x6

--- Gets the value of attribute "ShortName" of a Category entity.
categoryShortName :: Category -> String
categoryShortName (Category _ _ x _ _ _) = x

--- Sets the value of attribute "ShortName" in a Category entity.
setCategoryShortName :: Category -> String -> Category
setCategoryShortName (Category x1 x2 _ x4 x5 x6) x = Category x1 x2 x x4 x5 x6

--- Gets the value of attribute "CatKey" of a Category entity.
categoryCatKey :: Category -> String
categoryCatKey (Category _ _ _ x _ _) = x

--- Sets the value of attribute "CatKey" in a Category entity.
setCategoryCatKey :: Category -> String -> Category
setCategoryCatKey (Category x1 x2 x3 _ x5 x6) x = Category x1 x2 x3 x x5 x6

--- Gets the value of attribute "Position" of a Category entity.
categoryPosition :: Category -> Int
categoryPosition (Category _ _ _ _ x _) = x

--- Sets the value of attribute "Position" in a Category entity.
setCategoryPosition :: Category -> Int -> Category
setCategoryPosition (Category x1 x2 x3 x4 _ x6) x = Category x1 x2 x3 x4 x x6

--- Gets the value of attribute "StudyProgramProgramCategoriesKey" of a Category entity.
categoryStudyProgramProgramCategoriesKey :: Category -> StudyProgramKey
categoryStudyProgramProgramCategoriesKey (Category _ _ _ _ _ x) =
  StudyProgramKey x

--- Sets the value of attribute "StudyProgramProgramCategoriesKey" in a Category entity.
setCategoryStudyProgramProgramCategoriesKey
 :: Category -> StudyProgramKey -> Category
setCategoryStudyProgramProgramCategoriesKey (Category x1 x2 x3 x4 x5 _) x =
  Category x1 x2 x3 x4 x5 (studyProgramKeyToKey x)

--- Sets the value of attribute "Key" in a MasterCoreArea entity.
setMasterCoreAreaKey :: MasterCoreArea -> ERDGeneric.Key -> MasterCoreArea
setMasterCoreAreaKey (MasterCoreArea _ x2 x3 x4 x5 x6) x =
  MasterCoreArea x x2 x3 x4 x5 x6

--- Gets the value of attribute "Name" of a MasterCoreArea entity.
masterCoreAreaName :: MasterCoreArea -> String
masterCoreAreaName (MasterCoreArea _ x _ _ _ _) = x

--- Sets the value of attribute "Name" in a MasterCoreArea entity.
setMasterCoreAreaName :: MasterCoreArea -> String -> MasterCoreArea
setMasterCoreAreaName (MasterCoreArea x1 _ x3 x4 x5 x6) x =
  MasterCoreArea x1 x x3 x4 x5 x6

--- Gets the value of attribute "ShortName" of a MasterCoreArea entity.
masterCoreAreaShortName :: MasterCoreArea -> String
masterCoreAreaShortName (MasterCoreArea _ _ x _ _ _) = x

--- Sets the value of attribute "ShortName" in a MasterCoreArea entity.
setMasterCoreAreaShortName :: MasterCoreArea -> String -> MasterCoreArea
setMasterCoreAreaShortName (MasterCoreArea x1 x2 _ x4 x5 x6) x =
  MasterCoreArea x1 x2 x x4 x5 x6

--- Gets the value of attribute "Description" of a MasterCoreArea entity.
masterCoreAreaDescription :: MasterCoreArea -> String
masterCoreAreaDescription (MasterCoreArea _ _ _ x _ _) = x

--- Sets the value of attribute "Description" in a MasterCoreArea entity.
setMasterCoreAreaDescription :: MasterCoreArea -> String -> MasterCoreArea
setMasterCoreAreaDescription (MasterCoreArea x1 x2 x3 _ x5 x6) x =
  MasterCoreArea x1 x2 x3 x x5 x6

--- Gets the value of attribute "AreaKey" of a MasterCoreArea entity.
masterCoreAreaAreaKey :: MasterCoreArea -> String
masterCoreAreaAreaKey (MasterCoreArea _ _ _ _ x _) = x

--- Sets the value of attribute "AreaKey" in a MasterCoreArea entity.
setMasterCoreAreaAreaKey :: MasterCoreArea -> String -> MasterCoreArea
setMasterCoreAreaAreaKey (MasterCoreArea x1 x2 x3 x4 _ x6) x =
  MasterCoreArea x1 x2 x3 x4 x x6

--- Gets the value of attribute "Position" of a MasterCoreArea entity.
masterCoreAreaPosition :: MasterCoreArea -> Int
masterCoreAreaPosition (MasterCoreArea _ _ _ _ _ x) = x

--- Sets the value of attribute "Position" in a MasterCoreArea entity.
setMasterCoreAreaPosition :: MasterCoreArea -> Int -> MasterCoreArea
setMasterCoreAreaPosition (MasterCoreArea x1 x2 x3 x4 x5 _) x =
  MasterCoreArea x1 x2 x3 x4 x5 x

--- Sets the value of attribute "Key" in a User entity.
setUserKey :: User -> ERDGeneric.Key -> User
setUserKey (User _ x2 x3 x4 x5 x6 x7 x8 x9) x = User x x2 x3 x4 x5 x6 x7 x8 x9

--- Gets the value of attribute "Login" of a User entity.
userLogin :: User -> String
userLogin (User _ x _ _ _ _ _ _ _) = x

--- Sets the value of attribute "Login" in a User entity.
setUserLogin :: User -> String -> User
setUserLogin (User x1 _ x3 x4 x5 x6 x7 x8 x9) x =
  User x1 x x3 x4 x5 x6 x7 x8 x9

--- Gets the value of attribute "Name" of a User entity.
userName :: User -> String
userName (User _ _ x _ _ _ _ _ _) = x

--- Sets the value of attribute "Name" in a User entity.
setUserName :: User -> String -> User
setUserName (User x1 x2 _ x4 x5 x6 x7 x8 x9) x =
  User x1 x2 x x4 x5 x6 x7 x8 x9

--- Gets the value of attribute "First" of a User entity.
userFirst :: User -> String
userFirst (User _ _ _ x _ _ _ _ _) = x

--- Sets the value of attribute "First" in a User entity.
setUserFirst :: User -> String -> User
setUserFirst (User x1 x2 x3 _ x5 x6 x7 x8 x9) x =
  User x1 x2 x3 x x5 x6 x7 x8 x9

--- Gets the value of attribute "Title" of a User entity.
userTitle :: User -> String
userTitle (User _ _ _ _ x _ _ _ _) = x

--- Sets the value of attribute "Title" in a User entity.
setUserTitle :: User -> String -> User
setUserTitle (User x1 x2 x3 x4 _ x6 x7 x8 x9) x =
  User x1 x2 x3 x4 x x6 x7 x8 x9

--- Gets the value of attribute "Email" of a User entity.
userEmail :: User -> String
userEmail (User _ _ _ _ _ x _ _ _) = x

--- Sets the value of attribute "Email" in a User entity.
setUserEmail :: User -> String -> User
setUserEmail (User x1 x2 x3 x4 x5 _ x7 x8 x9) x =
  User x1 x2 x3 x4 x5 x x7 x8 x9

--- Gets the value of attribute "Url" of a User entity.
userUrl :: User -> String
userUrl (User _ _ _ _ _ _ x _ _) = x

--- Sets the value of attribute "Url" in a User entity.
setUserUrl :: User -> String -> User
setUserUrl (User x1 x2 x3 x4 x5 x6 _ x8 x9) x = User x1 x2 x3 x4 x5 x6 x x8 x9

--- Gets the value of attribute "Password" of a User entity.
userPassword :: User -> String
userPassword (User _ _ _ _ _ _ _ x _) = x

--- Sets the value of attribute "Password" in a User entity.
setUserPassword :: User -> String -> User
setUserPassword (User x1 x2 x3 x4 x5 x6 x7 _ x9) x =
  User x1 x2 x3 x4 x5 x6 x7 x x9

--- Gets the value of attribute "LastLogin" of a User entity.
userLastLogin :: User -> Time.CalendarTime
userLastLogin (User _ _ _ _ _ _ _ _ x) = x

--- Sets the value of attribute "LastLogin" in a User entity.
setUserLastLogin :: User -> Time.CalendarTime -> User
setUserLastLogin (User x1 x2 x3 x4 x5 x6 x7 x8 _) x =
  User x1 x2 x3 x4 x5 x6 x7 x8 x

--- Sets the value of attribute "Key" in a ModData entity.
setModDataKey :: ModData -> ERDGeneric.Key -> ModData
setModDataKey (ModData _ x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) x =
  ModData x x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12

--- Gets the value of attribute "Code" of a ModData entity.
modDataCode :: ModData -> String
modDataCode (ModData _ x _ _ _ _ _ _ _ _ _ _) = x

--- Sets the value of attribute "Code" in a ModData entity.
setModDataCode :: ModData -> String -> ModData
setModDataCode (ModData x1 _ x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) x =
  ModData x1 x x3 x4 x5 x6 x7 x8 x9 x10 x11 x12

--- Gets the value of attribute "NameG" of a ModData entity.
modDataNameG :: ModData -> String
modDataNameG (ModData _ _ x _ _ _ _ _ _ _ _ _) = x

--- Sets the value of attribute "NameG" in a ModData entity.
setModDataNameG :: ModData -> String -> ModData
setModDataNameG (ModData x1 x2 _ x4 x5 x6 x7 x8 x9 x10 x11 x12) x =
  ModData x1 x2 x x4 x5 x6 x7 x8 x9 x10 x11 x12

--- Gets the value of attribute "NameE" of a ModData entity.
modDataNameE :: ModData -> String
modDataNameE (ModData _ _ _ x _ _ _ _ _ _ _ _) = x

--- Sets the value of attribute "NameE" in a ModData entity.
setModDataNameE :: ModData -> String -> ModData
setModDataNameE (ModData x1 x2 x3 _ x5 x6 x7 x8 x9 x10 x11 x12) x =
  ModData x1 x2 x3 x x5 x6 x7 x8 x9 x10 x11 x12

--- Gets the value of attribute "Cycle" of a ModData entity.
modDataCycle :: ModData -> String
modDataCycle (ModData _ _ _ _ x _ _ _ _ _ _ _) = x

--- Sets the value of attribute "Cycle" in a ModData entity.
setModDataCycle :: ModData -> String -> ModData
setModDataCycle (ModData x1 x2 x3 x4 _ x6 x7 x8 x9 x10 x11 x12) x =
  ModData x1 x2 x3 x4 x x6 x7 x8 x9 x10 x11 x12

--- Gets the value of attribute "Presence" of a ModData entity.
modDataPresence :: ModData -> String
modDataPresence (ModData _ _ _ _ _ x _ _ _ _ _ _) = x

--- Sets the value of attribute "Presence" in a ModData entity.
setModDataPresence :: ModData -> String -> ModData
setModDataPresence (ModData x1 x2 x3 x4 x5 _ x7 x8 x9 x10 x11 x12) x =
  ModData x1 x2 x3 x4 x5 x x7 x8 x9 x10 x11 x12

--- Gets the value of attribute "ECTS" of a ModData entity.
modDataECTS :: ModData -> Int
modDataECTS (ModData _ _ _ _ _ _ x _ _ _ _ _) = x

--- Sets the value of attribute "ECTS" in a ModData entity.
setModDataECTS :: ModData -> Int -> ModData
setModDataECTS (ModData x1 x2 x3 x4 x5 x6 _ x8 x9 x10 x11 x12) x =
  ModData x1 x2 x3 x4 x5 x6 x x8 x9 x10 x11 x12

--- Gets the value of attribute "Workload" of a ModData entity.
modDataWorkload :: ModData -> String
modDataWorkload (ModData _ _ _ _ _ _ _ x _ _ _ _) = x

--- Sets the value of attribute "Workload" in a ModData entity.
setModDataWorkload :: ModData -> String -> ModData
setModDataWorkload (ModData x1 x2 x3 x4 x5 x6 x7 _ x9 x10 x11 x12) x =
  ModData x1 x2 x3 x4 x5 x6 x7 x x9 x10 x11 x12

--- Gets the value of attribute "Length" of a ModData entity.
modDataLength :: ModData -> Int
modDataLength (ModData _ _ _ _ _ _ _ _ x _ _ _) = x

--- Sets the value of attribute "Length" in a ModData entity.
setModDataLength :: ModData -> Int -> ModData
setModDataLength (ModData x1 x2 x3 x4 x5 x6 x7 x8 _ x10 x11 x12) x =
  ModData x1 x2 x3 x4 x5 x6 x7 x8 x x10 x11 x12

--- Gets the value of attribute "URL" of a ModData entity.
modDataURL :: ModData -> String
modDataURL (ModData _ _ _ _ _ _ _ _ _ x _ _) = x

--- Sets the value of attribute "URL" in a ModData entity.
setModDataURL :: ModData -> String -> ModData
setModDataURL (ModData x1 x2 x3 x4 x5 x6 x7 x8 x9 _ x11 x12) x =
  ModData x1 x2 x3 x4 x5 x6 x7 x8 x9 x x11 x12

--- Gets the value of attribute "Visible" of a ModData entity.
modDataVisible :: ModData -> Bool
modDataVisible (ModData _ _ _ _ _ _ _ _ _ _ x _) = x

--- Sets the value of attribute "Visible" in a ModData entity.
setModDataVisible :: ModData -> Bool -> ModData
setModDataVisible (ModData x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 _ x12) x =
  ModData x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x x12

--- Gets the value of attribute "UserResponsibleKey" of a ModData entity.
modDataUserResponsibleKey :: ModData -> UserKey
modDataUserResponsibleKey (ModData _ _ _ _ _ _ _ _ _ _ _ x) = UserKey x

--- Sets the value of attribute "UserResponsibleKey" in a ModData entity.
setModDataUserResponsibleKey :: ModData -> UserKey -> ModData
setModDataUserResponsibleKey (ModData x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 _)
                             x =
  ModData x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 (userKeyToKey x)

--- Sets the value of attribute "Key" in a ModDescr entity.
setModDescrKey :: ModDescr -> ERDGeneric.Key -> ModDescr
setModDescrKey (ModDescr _ x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) x =
  ModDescr x x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13

--- Gets the value of attribute "Language" of a ModDescr entity.
modDescrLanguage :: ModDescr -> String
modDescrLanguage (ModDescr _ x _ _ _ _ _ _ _ _ _ _ _) = x

--- Sets the value of attribute "Language" in a ModDescr entity.
setModDescrLanguage :: ModDescr -> String -> ModDescr
setModDescrLanguage (ModDescr x1 _ x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) x =
  ModDescr x1 x x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13

--- Gets the value of attribute "ShortDesc" of a ModDescr entity.
modDescrShortDesc :: ModDescr -> String
modDescrShortDesc (ModDescr _ _ x _ _ _ _ _ _ _ _ _ _) = x

--- Sets the value of attribute "ShortDesc" in a ModDescr entity.
setModDescrShortDesc :: ModDescr -> String -> ModDescr
setModDescrShortDesc (ModDescr x1 x2 _ x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) x =
  ModDescr x1 x2 x x4 x5 x6 x7 x8 x9 x10 x11 x12 x13

--- Gets the value of attribute "Objectives" of a ModDescr entity.
modDescrObjectives :: ModDescr -> String
modDescrObjectives (ModDescr _ _ _ x _ _ _ _ _ _ _ _ _) = x

--- Sets the value of attribute "Objectives" in a ModDescr entity.
setModDescrObjectives :: ModDescr -> String -> ModDescr
setModDescrObjectives (ModDescr x1 x2 x3 _ x5 x6 x7 x8 x9 x10 x11 x12 x13) x =
  ModDescr x1 x2 x3 x x5 x6 x7 x8 x9 x10 x11 x12 x13

--- Gets the value of attribute "Contents" of a ModDescr entity.
modDescrContents :: ModDescr -> String
modDescrContents (ModDescr _ _ _ _ x _ _ _ _ _ _ _ _) = x

--- Sets the value of attribute "Contents" in a ModDescr entity.
setModDescrContents :: ModDescr -> String -> ModDescr
setModDescrContents (ModDescr x1 x2 x3 x4 _ x6 x7 x8 x9 x10 x11 x12 x13) x =
  ModDescr x1 x2 x3 x4 x x6 x7 x8 x9 x10 x11 x12 x13

--- Gets the value of attribute "Prereq" of a ModDescr entity.
modDescrPrereq :: ModDescr -> String
modDescrPrereq (ModDescr _ _ _ _ _ x _ _ _ _ _ _ _) = x

--- Sets the value of attribute "Prereq" in a ModDescr entity.
setModDescrPrereq :: ModDescr -> String -> ModDescr
setModDescrPrereq (ModDescr x1 x2 x3 x4 x5 _ x7 x8 x9 x10 x11 x12 x13) x =
  ModDescr x1 x2 x3 x4 x5 x x7 x8 x9 x10 x11 x12 x13

--- Gets the value of attribute "Exam" of a ModDescr entity.
modDescrExam :: ModDescr -> String
modDescrExam (ModDescr _ _ _ _ _ _ x _ _ _ _ _ _) = x

--- Sets the value of attribute "Exam" in a ModDescr entity.
setModDescrExam :: ModDescr -> String -> ModDescr
setModDescrExam (ModDescr x1 x2 x3 x4 x5 x6 _ x8 x9 x10 x11 x12 x13) x =
  ModDescr x1 x2 x3 x4 x5 x6 x x8 x9 x10 x11 x12 x13

--- Gets the value of attribute "Methods" of a ModDescr entity.
modDescrMethods :: ModDescr -> String
modDescrMethods (ModDescr _ _ _ _ _ _ _ x _ _ _ _ _) = x

--- Sets the value of attribute "Methods" in a ModDescr entity.
setModDescrMethods :: ModDescr -> String -> ModDescr
setModDescrMethods (ModDescr x1 x2 x3 x4 x5 x6 x7 _ x9 x10 x11 x12 x13) x =
  ModDescr x1 x2 x3 x4 x5 x6 x7 x x9 x10 x11 x12 x13

--- Gets the value of attribute "Use" of a ModDescr entity.
modDescrUse :: ModDescr -> String
modDescrUse (ModDescr _ _ _ _ _ _ _ _ x _ _ _ _) = x

--- Sets the value of attribute "Use" in a ModDescr entity.
setModDescrUse :: ModDescr -> String -> ModDescr
setModDescrUse (ModDescr x1 x2 x3 x4 x5 x6 x7 x8 _ x10 x11 x12 x13) x =
  ModDescr x1 x2 x3 x4 x5 x6 x7 x8 x x10 x11 x12 x13

--- Gets the value of attribute "Literature" of a ModDescr entity.
modDescrLiterature :: ModDescr -> String
modDescrLiterature (ModDescr _ _ _ _ _ _ _ _ _ x _ _ _) = x

--- Sets the value of attribute "Literature" in a ModDescr entity.
setModDescrLiterature :: ModDescr -> String -> ModDescr
setModDescrLiterature (ModDescr x1 x2 x3 x4 x5 x6 x7 x8 x9 _ x11 x12 x13) x =
  ModDescr x1 x2 x3 x4 x5 x6 x7 x8 x9 x x11 x12 x13

--- Gets the value of attribute "Links" of a ModDescr entity.
modDescrLinks :: ModDescr -> String
modDescrLinks (ModDescr _ _ _ _ _ _ _ _ _ _ x _ _) = x

--- Sets the value of attribute "Links" in a ModDescr entity.
setModDescrLinks :: ModDescr -> String -> ModDescr
setModDescrLinks (ModDescr x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 _ x12 x13) x =
  ModDescr x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x x12 x13

--- Gets the value of attribute "Comments" of a ModDescr entity.
modDescrComments :: ModDescr -> String
modDescrComments (ModDescr _ _ _ _ _ _ _ _ _ _ _ x _) = x

--- Sets the value of attribute "Comments" in a ModDescr entity.
setModDescrComments :: ModDescr -> String -> ModDescr
setModDescrComments (ModDescr x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 _ x13) x =
  ModDescr x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x x13

--- Gets the value of attribute "ModDataDataDescKey" of a ModDescr entity.
modDescrModDataDataDescKey :: ModDescr -> ModDataKey
modDescrModDataDataDescKey (ModDescr _ _ _ _ _ _ _ _ _ _ _ _ x) = ModDataKey x

--- Sets the value of attribute "ModDataDataDescKey" in a ModDescr entity.
setModDescrModDataDataDescKey :: ModDescr -> ModDataKey -> ModDescr
setModDescrModDataDataDescKey (ModDescr x1
                                        x2
                                        x3
                                        x4
                                        x5
                                        x6
                                        x7
                                        x8
                                        x9
                                        x10
                                        x11
                                        x12
                                        _) x =
  ModDescr x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 (modDataKeyToKey x)

--- Sets the value of attribute "Key" in a ModInst entity.
setModInstKey :: ModInst -> ERDGeneric.Key -> ModInst
setModInstKey (ModInst _ x2 x3 x4 x5) x = ModInst x x2 x3 x4 x5

--- Gets the value of attribute "Term" of a ModInst entity.
modInstTerm :: ModInst -> String
modInstTerm (ModInst _ x _ _ _) = x

--- Sets the value of attribute "Term" in a ModInst entity.
setModInstTerm :: ModInst -> String -> ModInst
setModInstTerm (ModInst x1 _ x3 x4 x5) x = ModInst x1 x x3 x4 x5

--- Gets the value of attribute "Year" of a ModInst entity.
modInstYear :: ModInst -> Int
modInstYear (ModInst _ _ x _ _) = x

--- Sets the value of attribute "Year" in a ModInst entity.
setModInstYear :: ModInst -> Int -> ModInst
setModInstYear (ModInst x1 x2 _ x4 x5) x = ModInst x1 x2 x x4 x5

--- Gets the pair of term and year of a ModInst entity.
modInstSemester :: ModInst -> (String,Int)
modInstSemester (ModInst _ t y _ _) = (t,y)

--- Gets the value of attribute "UserLecturerModsKey" of a ModInst entity.
modInstUserLecturerModsKey :: ModInst -> UserKey
modInstUserLecturerModsKey (ModInst _ _ _ x _) = UserKey x

--- Sets the value of attribute "UserLecturerModsKey" in a ModInst entity.
setModInstUserLecturerModsKey :: ModInst -> UserKey -> ModInst
setModInstUserLecturerModsKey (ModInst x1 x2 x3 _ x5) x =
  ModInst x1 x2 x3 (userKeyToKey x) x5

--- Gets the value of attribute "ModDataModuleInstancesKey" of a ModInst entity.
modInstModDataModuleInstancesKey :: ModInst -> ModDataKey
modInstModDataModuleInstancesKey (ModInst _ _ _ _ x) = ModDataKey x

--- Sets the value of attribute "ModDataModuleInstancesKey" in a ModInst entity.
setModInstModDataModuleInstancesKey :: ModInst -> ModDataKey -> ModInst
setModInstModDataModuleInstancesKey (ModInst x1 x2 x3 x4 _) x =
  ModInst x1 x2 x3 x4 (modDataKeyToKey x)

--- Sets the value of attribute "Key" in a MasterProgram entity.
setMasterProgramKey :: MasterProgram -> ERDGeneric.Key -> MasterProgram
setMasterProgramKey (MasterProgram _ x2 x3 x4 x5 x6 x7 x8 x9 x10) x =
  MasterProgram x x2 x3 x4 x5 x6 x7 x8 x9 x10

--- Gets the value of attribute "Name" of a MasterProgram entity.
masterProgramName :: MasterProgram -> String
masterProgramName (MasterProgram _ x _ _ _ _ _ _ _ _) = x

--- Sets the value of attribute "Name" in a MasterProgram entity.
setMasterProgramName :: MasterProgram -> String -> MasterProgram
setMasterProgramName (MasterProgram x1 _ x3 x4 x5 x6 x7 x8 x9 x10) x =
  MasterProgram x1 x x3 x4 x5 x6 x7 x8 x9 x10

--- Gets the value of attribute "Term" of a MasterProgram entity.
masterProgramTerm :: MasterProgram -> String
masterProgramTerm (MasterProgram _ _ x _ _ _ _ _ _ _) = x

--- Sets the value of attribute "Term" in a MasterProgram entity.
setMasterProgramTerm :: MasterProgram -> String -> MasterProgram
setMasterProgramTerm (MasterProgram x1 x2 _ x4 x5 x6 x7 x8 x9 x10) x =
  MasterProgram x1 x2 x x4 x5 x6 x7 x8 x9 x10

--- Gets the value of attribute "Year" of a MasterProgram entity.
masterProgramYear :: MasterProgram -> Int
masterProgramYear (MasterProgram _ _ _ x _ _ _ _ _ _) = x

--- Sets the value of attribute "Year" in a MasterProgram entity.
setMasterProgramYear :: MasterProgram -> Int -> MasterProgram
setMasterProgramYear (MasterProgram x1 x2 x3 _ x5 x6 x7 x8 x9 x10) x =
  MasterProgram x1 x2 x3 x x5 x6 x7 x8 x9 x10

--- Gets the value of attribute "Desc" of a MasterProgram entity.
masterProgramDesc :: MasterProgram -> String
masterProgramDesc (MasterProgram _ _ _ _ x _ _ _ _ _) = x

--- Sets the value of attribute "Desc" in a MasterProgram entity.
setMasterProgramDesc :: MasterProgram -> String -> MasterProgram
setMasterProgramDesc (MasterProgram x1 x2 x3 x4 _ x6 x7 x8 x9 x10) x =
  MasterProgram x1 x2 x3 x4 x x6 x7 x8 x9 x10

--- Gets the value of attribute "Prereq" of a MasterProgram entity.
masterProgramPrereq :: MasterProgram -> String
masterProgramPrereq (MasterProgram _ _ _ _ _ x _ _ _ _) = x

--- Sets the value of attribute "Prereq" in a MasterProgram entity.
setMasterProgramPrereq :: MasterProgram -> String -> MasterProgram
setMasterProgramPrereq (MasterProgram x1 x2 x3 x4 x5 _ x7 x8 x9 x10) x =
  MasterProgram x1 x2 x3 x4 x5 x x7 x8 x9 x10

--- Gets the value of attribute "Comments" of a MasterProgram entity.
masterProgramComments :: MasterProgram -> String
masterProgramComments (MasterProgram _ _ _ _ _ _ x _ _ _) = x

--- Sets the value of attribute "Comments" in a MasterProgram entity.
setMasterProgramComments :: MasterProgram -> String -> MasterProgram
setMasterProgramComments (MasterProgram x1 x2 x3 x4 x5 x6 _ x8 x9 x10) x =
  MasterProgram x1 x2 x3 x4 x5 x6 x x8 x9 x10

--- Gets the value of attribute "Visible" of a MasterProgram entity.
masterProgramVisible :: MasterProgram -> Bool
masterProgramVisible (MasterProgram _ _ _ _ _ _ _ x _ _) = x

--- Sets the value of attribute "Visible" in a MasterProgram entity.
setMasterProgramVisible :: MasterProgram -> Bool -> MasterProgram
setMasterProgramVisible (MasterProgram x1 x2 x3 x4 x5 x6 x7 _ x9 x10) x =
  MasterProgram x1 x2 x3 x4 x5 x6 x7 x x9 x10

--- Gets the value of attribute "UserAdvisingKey" of a MasterProgram entity.
masterProgramUserAdvisingKey :: MasterProgram -> UserKey
masterProgramUserAdvisingKey (MasterProgram _ _ _ _ _ _ _ _ x _) = UserKey x

--- Sets the value of attribute "UserAdvisingKey" in a MasterProgram entity.
setMasterProgramUserAdvisingKey :: MasterProgram -> UserKey -> MasterProgram
setMasterProgramUserAdvisingKey (MasterProgram x1 x2 x3 x4 x5 x6 x7 x8 _ x10)
                                x =
  MasterProgram x1 x2 x3 x4 x5 x6 x7 x8 (userKeyToKey x) x10

--- Gets the value of attribute "MasterCoreAreaAreaProgramsKey" of a MasterProgram entity.
masterProgramMasterCoreAreaAreaProgramsKey
 :: MasterProgram -> MasterCoreAreaKey
masterProgramMasterCoreAreaAreaProgramsKey (MasterProgram _
                                                          _
                                                          _
                                                          _
                                                          _
                                                          _
                                                          _
                                                          _
                                                          _
                                                          x) =
  MasterCoreAreaKey x

--- Sets the value of attribute "MasterCoreAreaAreaProgramsKey" in a MasterProgram entity.
setMasterProgramMasterCoreAreaAreaProgramsKey
 :: MasterProgram -> MasterCoreAreaKey -> MasterProgram
setMasterProgramMasterCoreAreaAreaProgramsKey (MasterProgram x1
                                                             x2
                                                             x3
                                                             x4
                                                             x5
                                                             x6
                                                             x7
                                                             x8
                                                             x9
                                                             _) x =
  MasterProgram x1 x2 x3 x4 x5 x6 x7 x8 x9 (masterCoreAreaKeyToKey x)

--- Sets the value of attribute "Key" in a MasterProgInfo entity.
setMasterProgInfoKey :: MasterProgInfo -> ERDGeneric.Key -> MasterProgInfo
setMasterProgInfoKey (MasterProgInfo _ x2 x3 x4 x5 x6 x7 x8) x =
  MasterProgInfo x x2 x3 x4 x5 x6 x7 x8

--- Gets the value of attribute "ProgModules" of a MasterProgInfo entity.
masterProgInfoProgModules :: MasterProgInfo -> String
masterProgInfoProgModules (MasterProgInfo _ x _ _ _ _ _ _) = x

--- Sets the value of attribute "ProgModules" in a MasterProgInfo entity.
setMasterProgInfoProgModules :: MasterProgInfo -> String -> MasterProgInfo
setMasterProgInfoProgModules (MasterProgInfo x1 _ x3 x4 x5 x6 x7 x8) x =
  MasterProgInfo x1 x x3 x4 x5 x6 x7 x8

--- Gets the value of attribute "Praktikum" of a MasterProgInfo entity.
masterProgInfoPraktikum :: MasterProgInfo -> String
masterProgInfoPraktikum (MasterProgInfo _ _ x _ _ _ _ _) = x

--- Sets the value of attribute "Praktikum" in a MasterProgInfo entity.
setMasterProgInfoPraktikum :: MasterProgInfo -> String -> MasterProgInfo
setMasterProgInfoPraktikum (MasterProgInfo x1 x2 _ x4 x5 x6 x7 x8) x =
  MasterProgInfo x1 x2 x x4 x5 x6 x7 x8

--- Gets the value of attribute "Seminar" of a MasterProgInfo entity.
masterProgInfoSeminar :: MasterProgInfo -> String
masterProgInfoSeminar (MasterProgInfo _ _ _ x _ _ _ _) = x

--- Sets the value of attribute "Seminar" in a MasterProgInfo entity.
setMasterProgInfoSeminar :: MasterProgInfo -> String -> MasterProgInfo
setMasterProgInfoSeminar (MasterProgInfo x1 x2 x3 _ x5 x6 x7 x8) x =
  MasterProgInfo x1 x2 x3 x x5 x6 x7 x8

--- Gets the value of attribute "Thesis" of a MasterProgInfo entity.
masterProgInfoThesis :: MasterProgInfo -> String
masterProgInfoThesis (MasterProgInfo _ _ _ _ x _ _ _) = x

--- Sets the value of attribute "Thesis" in a MasterProgInfo entity.
setMasterProgInfoThesis :: MasterProgInfo -> String -> MasterProgInfo
setMasterProgInfoThesis (MasterProgInfo x1 x2 x3 x4 _ x6 x7 x8) x =
  MasterProgInfo x1 x2 x3 x4 x x6 x7 x8

--- Gets the value of attribute "AllgGrundlagen" of a MasterProgInfo entity.
masterProgInfoAllgGrundlagen :: MasterProgInfo -> String
masterProgInfoAllgGrundlagen (MasterProgInfo _ _ _ _ _ x _ _) = x

--- Sets the value of attribute "AllgGrundlagen" in a MasterProgInfo entity.
setMasterProgInfoAllgGrundlagen :: MasterProgInfo -> String -> MasterProgInfo
setMasterProgInfoAllgGrundlagen (MasterProgInfo x1 x2 x3 x4 x5 _ x7 x8) x =
  MasterProgInfo x1 x2 x3 x4 x5 x x7 x8

--- Gets the value of attribute "Anwendungsfach" of a MasterProgInfo entity.
masterProgInfoAnwendungsfach :: MasterProgInfo -> String
masterProgInfoAnwendungsfach (MasterProgInfo _ _ _ _ _ _ x _) = x

--- Sets the value of attribute "Anwendungsfach" in a MasterProgInfo entity.
setMasterProgInfoAnwendungsfach :: MasterProgInfo -> String -> MasterProgInfo
setMasterProgInfoAnwendungsfach (MasterProgInfo x1 x2 x3 x4 x5 x6 _ x8) x =
  MasterProgInfo x1 x2 x3 x4 x5 x6 x x8

--- Gets the value of attribute "MasterProgramProgramInfoKey" of a MasterProgInfo entity.
masterProgInfoMasterProgramProgramInfoKey
 :: MasterProgInfo -> MasterProgramKey
masterProgInfoMasterProgramProgramInfoKey (MasterProgInfo _ _ _ _ _ _ _ x) =
  MasterProgramKey x

--- Sets the value of attribute "MasterProgramProgramInfoKey" in a MasterProgInfo entity.
setMasterProgInfoMasterProgramProgramInfoKey
 :: MasterProgInfo -> MasterProgramKey -> MasterProgInfo
setMasterProgInfoMasterProgramProgramInfoKey (MasterProgInfo x1
                                                             x2
                                                             x3
                                                             x4
                                                             x5
                                                             x6
                                                             x7
                                                             _) x =
  MasterProgInfo x1 x2 x3 x4 x5 x6 x7 (masterProgramKeyToKey x)

--- Sets the value of attribute "Key" in a UnivisInfo entity.
setUnivisInfoKey :: UnivisInfo -> ERDGeneric.Key -> UnivisInfo
setUnivisInfoKey (UnivisInfo _ x2 x3 x4 x5) x = UnivisInfo x x2 x3 x4 x5

--- Gets the value of attribute "Code" of a UnivisInfo entity.
univisInfoCode :: UnivisInfo -> String
univisInfoCode (UnivisInfo _ x _ _ _) = x

--- Sets the value of attribute "Code" in a UnivisInfo entity.
setUnivisInfoCode :: UnivisInfo -> String -> UnivisInfo
setUnivisInfoCode (UnivisInfo x1 _ x3 x4 x5) x = UnivisInfo x1 x x3 x4 x5

--- Gets the value of attribute "Term" of a UnivisInfo entity.
univisInfoTerm :: UnivisInfo -> String
univisInfoTerm (UnivisInfo _ _ x _ _) = x

--- Sets the value of attribute "Term" in a UnivisInfo entity.
setUnivisInfoTerm :: UnivisInfo -> String -> UnivisInfo
setUnivisInfoTerm (UnivisInfo x1 x2 _ x4 x5) x = UnivisInfo x1 x2 x x4 x5

--- Gets the value of attribute "Year" of a UnivisInfo entity.
univisInfoYear :: UnivisInfo -> Int
univisInfoYear (UnivisInfo _ _ _ x _) = x

--- Sets the value of attribute "Year" in a UnivisInfo entity.
setUnivisInfoYear :: UnivisInfo -> Int -> UnivisInfo
setUnivisInfoYear (UnivisInfo x1 x2 x3 _ x5) x = UnivisInfo x1 x2 x3 x x5

--- Gets the value of attribute "URL" of a UnivisInfo entity.
univisInfoURL :: UnivisInfo -> String
univisInfoURL (UnivisInfo _ _ _ _ x) = x

--- Sets the value of attribute "URL" in a UnivisInfo entity.
setUnivisInfoURL :: UnivisInfo -> String -> UnivisInfo
setUnivisInfoURL (UnivisInfo x1 x2 x3 x4 _) x = UnivisInfo x1 x2 x3 x4 x

--- Database predicate representing the relation between keys and StudyProgram tuple entities.
studyProgramEntry
 :: ERDGeneric.Key -> StudyProgramTuple -> KeyDatabase.Dynamic
studyProgramEntry =
  KeyDatabase.persistentSQLite mdbFile "StudyProgram"
   ["Name","ShortName","ProgKey","URLKey","Position"]

--- Dynamic predicate representing the relation
--- between keys and StudyProgram entities.
studyProgram :: StudyProgramKey -> StudyProgram -> KeyDatabase.Dynamic
studyProgram key obj
  | key =:= studyProgramKey obj = studyProgramEntry (studyProgramKeyToKey key)
                                   (studyProgram2tuple obj)

--- Gets the key of a StudyProgram entity.
studyProgramKey :: StudyProgram -> StudyProgramKey
studyProgramKey (StudyProgram x _ _ _ _ _) = StudyProgramKey x

--- Shows the key of a StudyProgram entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showStudyProgramKey :: StudyProgram -> String
showStudyProgramKey obj =
  ERDGeneric.showDatabaseKey "StudyProgram" studyProgramKeyToKey
   (studyProgramKey obj)

--- Transforms a string into a key of a StudyProgram entity.
--- Nothing is returned if the string does not represent a reasonable key.
readStudyProgramKey :: String -> Maybe StudyProgramKey
readStudyProgramKey s =
  ERDGeneric.readDatabaseKey "StudyProgram" StudyProgramKey s

studyProgramKeyToKey :: StudyProgramKey -> ERDGeneric.Key
studyProgramKeyToKey (StudyProgramKey k) = k

maybeStudyProgramKeyToKey :: Maybe StudyProgramKey -> Maybe ERDGeneric.Key
maybeStudyProgramKeyToKey Nothing = Nothing
maybeStudyProgramKeyToKey (Just (StudyProgramKey k)) = Just k

--- Inserts a new StudyProgram entity.
newStudyProgram
 :: String -> String -> String -> String -> Int
  -> KeyDatabase.Transaction StudyProgram
newStudyProgram name_p shortName_p progKey_p uRLKey_p position_p =
  ERDGeneric.unique "MDB" studyProgramEntry keytuple2StudyProgram
   studyProgramShortName shortName_p |>>
   (ERDGeneric.unique "MDB" studyProgramEntry keytuple2StudyProgram
     studyProgramProgKey progKey_p |>>
    (ERDGeneric.unique "MDB" studyProgramEntry keytuple2StudyProgram
      studyProgramURLKey uRLKey_p |>>
     ERDGeneric.newEntry studyProgramEntry keytuple2StudyProgram
      (name_p,shortName_p,progKey_p,uRLKey_p,position_p)))

--- Updates an existing StudyProgram entity.
updateStudyProgram :: StudyProgram -> KeyDatabase.Transaction ()
updateStudyProgram studyProgram_p =
  ERDGeneric.uniqueUpdate "MDB" studyProgramEntry keytuple2StudyProgram
   (studyProgramKeyToKey . studyProgramKey) studyProgramShortName
   studyProgram_p |>>
   (ERDGeneric.uniqueUpdate "MDB" studyProgramEntry keytuple2StudyProgram
     (studyProgramKeyToKey . studyProgramKey) studyProgramProgKey
     studyProgram_p |>>
    (ERDGeneric.uniqueUpdate "MDB" studyProgramEntry keytuple2StudyProgram
      (studyProgramKeyToKey . studyProgramKey) studyProgramURLKey
      studyProgram_p |>>
     KeyDatabase.updateDBEntry studyProgramEntry
      (studyProgramKeyToKey (studyProgramKey studyProgram_p))
      (studyProgram2tuple studyProgram_p)))

--- Deletes an existing StudyProgram entity.
deleteStudyProgram :: StudyProgram -> KeyDatabase.Transaction ()
deleteStudyProgram studyProgram_p =
  ERDGeneric.requiredForeignDBKey "Category" categoryEntry keytuple2Category
   categoryStudyProgramProgramCategoriesKey (studyProgramKey studyProgram_p)
   |>>
   KeyDatabase.deleteDBEntry studyProgramEntry
    (studyProgramKeyToKey (studyProgramKey studyProgram_p))

--- Gets a StudyProgram entity stored in the database with the given key.
getStudyProgram :: StudyProgramKey -> KeyDatabase.Transaction StudyProgram
getStudyProgram key =
  ERDGeneric.getEntry studyProgramEntry keytuple2StudyProgram
   (studyProgramKeyToKey key)

--- Gets all StudyProgram entities stored in the database.
queryAllStudyPrograms :: KeyDatabase.Query [StudyProgram]
queryAllStudyPrograms =
  KeyDatabase.transformQ (map (uncurry keytuple2StudyProgram))
   (KeyDatabase.allDBKeyInfos studyProgramEntry)

--- Gets all StudyProgram entities satisfying a given condition.
queryCondStudyProgram
 :: (StudyProgram -> Bool) -> KeyDatabase.Query [StudyProgram]
queryCondStudyProgram econd =
  KeyDatabase.transformQ (filter econd) queryAllStudyPrograms

--- Database predicate representing the relation between keys and Category tuple entities.
categoryEntry :: ERDGeneric.Key -> CategoryTuple -> KeyDatabase.Dynamic
categoryEntry =
  KeyDatabase.persistentSQLite mdbFile "Category"
   ["Name","ShortName","CatKey","Position","StudyProgramProgramCategoriesKey"]

--- Dynamic predicate representing the relation
--- between keys and Category entities.
category :: CategoryKey -> Category -> KeyDatabase.Dynamic
category key obj
  | key =:= categoryKey obj = categoryEntry (categoryKeyToKey key)
                               (category2tuple obj)

--- Gets the key of a Category entity.
categoryKey :: Category -> CategoryKey
categoryKey (Category x _ _ _ _ _) = CategoryKey x

--- Shows the key of a Category entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showCategoryKey :: Category -> String
showCategoryKey obj =
  ERDGeneric.showDatabaseKey "Category" categoryKeyToKey (categoryKey obj)

--- Transforms a string into a key of a Category entity.
--- Nothing is returned if the string does not represent a reasonable key.
readCategoryKey :: String -> Maybe CategoryKey
readCategoryKey s = ERDGeneric.readDatabaseKey "Category" CategoryKey s

categoryKeyToKey :: CategoryKey -> ERDGeneric.Key
categoryKeyToKey (CategoryKey k) = k

maybeCategoryKeyToKey :: Maybe CategoryKey -> Maybe ERDGeneric.Key
maybeCategoryKeyToKey Nothing = Nothing
maybeCategoryKeyToKey (Just (CategoryKey k)) = Just k

--- Inserts a new Category entity.
newCategoryWithStudyProgramProgramCategoriesKey
 :: String -> String -> String -> Int -> StudyProgramKey
  -> KeyDatabase.Transaction Category
newCategoryWithStudyProgramProgramCategoriesKey name_p shortName_p catKey_p
                                                position_p
                                                studyProgramProgramCategoriesKey_p =
  ERDGeneric.unique "MDB" categoryEntry keytuple2Category categoryCatKey
   catKey_p |>>
   (ERDGeneric.existsEntryWithDBKey "StudyProgram" studyProgramEntry
     (studyProgramKeyToKey studyProgramProgramCategoriesKey_p) |>>
    ERDGeneric.newEntry categoryEntry keytuple2Category
     (name_p,shortName_p,catKey_p,position_p
     ,studyProgramKeyToKey studyProgramProgramCategoriesKey_p))

--- Updates an existing Category entity.
updateCategory :: Category -> KeyDatabase.Transaction ()
updateCategory category_p =
  ERDGeneric.uniqueUpdate "MDB" categoryEntry keytuple2Category
   (categoryKeyToKey . categoryKey) categoryCatKey category_p |>>
   (ERDGeneric.existsEntryWithDBKey "StudyProgram" studyProgramEntry
     (studyProgramKeyToKey
       (categoryStudyProgramProgramCategoriesKey category_p)) |>>
    KeyDatabase.updateDBEntry categoryEntry
     (categoryKeyToKey (categoryKey category_p)) (category2tuple category_p))

--- Deletes an existing Category entity.
deleteCategory :: Category -> KeyDatabase.Transaction ()
deleteCategory category_p =
  ERDGeneric.requiredForeignDBKey "Categorizing" categorizingEntry
   keytuple2Categorizing categorizingCategoryCategorizingKey
   (categoryKey category_p) |>>
   KeyDatabase.deleteDBEntry categoryEntry
    (categoryKeyToKey (categoryKey category_p))

--- Gets a Category entity stored in the database with the given key.
getCategory :: CategoryKey -> KeyDatabase.Transaction Category
getCategory key =
  ERDGeneric.getEntry categoryEntry keytuple2Category (categoryKeyToKey key)

--- Gets all Category entities stored in the database.
queryAllCategorys :: KeyDatabase.Query [Category]
queryAllCategorys =
  KeyDatabase.transformQ (map (uncurry keytuple2Category))
   (KeyDatabase.allDBKeyInfos categoryEntry)

--- Gets all Category entities satisfying a given condition.
queryCondCategory :: (Category -> Bool) -> KeyDatabase.Query [Category]
queryCondCategory econd =
  KeyDatabase.transformQ (filter econd) queryAllCategorys

--- Database predicate representing the relation between keys and MasterCoreArea tuple entities.
masterCoreAreaEntry
 :: ERDGeneric.Key -> MasterCoreAreaTuple -> KeyDatabase.Dynamic
masterCoreAreaEntry =
  KeyDatabase.persistentSQLite mdbFile "MasterCoreArea"
   ["Name","ShortName","Description","AreaKey","Position"]

--- Dynamic predicate representing the relation
--- between keys and MasterCoreArea entities.
masterCoreArea :: MasterCoreAreaKey -> MasterCoreArea -> KeyDatabase.Dynamic
masterCoreArea key obj
  | key =:= masterCoreAreaKey obj = masterCoreAreaEntry
                                     (masterCoreAreaKeyToKey key)
                                     (masterCoreArea2tuple obj)

--- Gets the key of a MasterCoreArea entity.
masterCoreAreaKey :: MasterCoreArea -> MasterCoreAreaKey
masterCoreAreaKey (MasterCoreArea x _ _ _ _ _) = MasterCoreAreaKey x

--- Shows the key of a MasterCoreArea entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showMasterCoreAreaKey :: MasterCoreArea -> String
showMasterCoreAreaKey obj =
  ERDGeneric.showDatabaseKey "MasterCoreArea" masterCoreAreaKeyToKey
   (masterCoreAreaKey obj)

--- Transforms a string into a key of a MasterCoreArea entity.
--- Nothing is returned if the string does not represent a reasonable key.
readMasterCoreAreaKey :: String -> Maybe MasterCoreAreaKey
readMasterCoreAreaKey s =
  ERDGeneric.readDatabaseKey "MasterCoreArea" MasterCoreAreaKey s

masterCoreAreaKeyToKey :: MasterCoreAreaKey -> ERDGeneric.Key
masterCoreAreaKeyToKey (MasterCoreAreaKey k) = k

maybeMasterCoreAreaKeyToKey :: Maybe MasterCoreAreaKey -> Maybe ERDGeneric.Key
maybeMasterCoreAreaKeyToKey Nothing = Nothing
maybeMasterCoreAreaKeyToKey (Just (MasterCoreAreaKey k)) = Just k

--- Inserts a new MasterCoreArea entity.
newMasterCoreArea
 :: String -> String -> String -> String -> Maybe Int
  -> KeyDatabase.Transaction MasterCoreArea
newMasterCoreArea name_p shortName_p description_p areaKey_p position_p =
  ERDGeneric.unique "MDB" masterCoreAreaEntry keytuple2MasterCoreArea
   masterCoreAreaAreaKey areaKey_p |>>
   ERDGeneric.newEntry masterCoreAreaEntry keytuple2MasterCoreArea
    (name_p,shortName_p,description_p,areaKey_p,maybe 1 id position_p)

--- Updates an existing MasterCoreArea entity.
updateMasterCoreArea :: MasterCoreArea -> KeyDatabase.Transaction ()
updateMasterCoreArea masterCoreArea_p =
  ERDGeneric.uniqueUpdate "MDB" masterCoreAreaEntry keytuple2MasterCoreArea
   (masterCoreAreaKeyToKey . masterCoreAreaKey) masterCoreAreaAreaKey
   masterCoreArea_p |>>
   KeyDatabase.updateDBEntry masterCoreAreaEntry
    (masterCoreAreaKeyToKey (masterCoreAreaKey masterCoreArea_p))
    (masterCoreArea2tuple masterCoreArea_p)

--- Deletes an existing MasterCoreArea entity.
deleteMasterCoreArea :: MasterCoreArea -> KeyDatabase.Transaction ()
deleteMasterCoreArea masterCoreArea_p =
  ERDGeneric.requiredForeignDBKey "MasterProgram" masterProgramEntry
   keytuple2MasterProgram masterProgramMasterCoreAreaAreaProgramsKey
   (masterCoreAreaKey masterCoreArea_p) |>>
   KeyDatabase.deleteDBEntry masterCoreAreaEntry
    (masterCoreAreaKeyToKey (masterCoreAreaKey masterCoreArea_p))

--- Gets a MasterCoreArea entity stored in the database with the given key.
getMasterCoreArea
 :: MasterCoreAreaKey -> KeyDatabase.Transaction MasterCoreArea
getMasterCoreArea key =
  ERDGeneric.getEntry masterCoreAreaEntry keytuple2MasterCoreArea
   (masterCoreAreaKeyToKey key)

--- Gets all MasterCoreArea entities stored in the database.
queryAllMasterCoreAreas :: KeyDatabase.Query [MasterCoreArea]
queryAllMasterCoreAreas =
  KeyDatabase.transformQ (map (uncurry keytuple2MasterCoreArea))
   (KeyDatabase.allDBKeyInfos masterCoreAreaEntry)

--- Gets all MasterCoreArea entities satisfying a given condition.
queryCondMasterCoreArea
 :: (MasterCoreArea -> Bool) -> KeyDatabase.Query [MasterCoreArea]
queryCondMasterCoreArea econd =
  KeyDatabase.transformQ (filter econd) queryAllMasterCoreAreas

--- Database predicate representing the relation between keys and User tuple entities.
userEntry :: ERDGeneric.Key -> UserTuple -> KeyDatabase.Dynamic
userEntry =
  KeyDatabase.persistentSQLite mdbFile "User"
   ["Login","Name","First","Title","Email","Url","Password","LastLogin"]

--- Dynamic predicate representing the relation
--- between keys and User entities.
user :: UserKey -> User -> KeyDatabase.Dynamic
user key obj
  | key =:= userKey obj = userEntry (userKeyToKey key) (user2tuple obj)

--- Gets the key of a User entity.
userKey :: User -> UserKey
userKey (User x _ _ _ _ _ _ _ _) = UserKey x

--- Shows the key of a User entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showUserKey :: User -> String
showUserKey obj = ERDGeneric.showDatabaseKey "User" userKeyToKey (userKey obj)

--- Transforms a string into a key of a User entity.
--- Nothing is returned if the string does not represent a reasonable key.
readUserKey :: String -> Maybe UserKey
readUserKey s = ERDGeneric.readDatabaseKey "User" UserKey s

userKeyToKey :: UserKey -> ERDGeneric.Key
userKeyToKey (UserKey k) = k

maybeUserKeyToKey :: Maybe UserKey -> Maybe ERDGeneric.Key
maybeUserKeyToKey Nothing = Nothing
maybeUserKeyToKey (Just (UserKey k)) = Just k

--- Inserts a new User entity.
newUser
 :: String -> String -> String -> String -> String -> String -> String
  -> Time.CalendarTime -> KeyDatabase.Transaction User
newUser login_p name_p first_p title_p email_p url_p password_p lastLogin_p =
  ERDGeneric.unique "MDB" userEntry keytuple2User userLogin login_p |>>
   ERDGeneric.newEntry userEntry keytuple2User
    (login_p,name_p,first_p,title_p,email_p,url_p,password_p,lastLogin_p)

--- Updates an existing User entity.
updateUser :: User -> KeyDatabase.Transaction ()
updateUser user_p =
  ERDGeneric.uniqueUpdate "MDB" userEntry keytuple2User
   (userKeyToKey . userKey) userLogin user_p |>>
   KeyDatabase.updateDBEntry userEntry (userKeyToKey (userKey user_p))
    (user2tuple user_p)

--- Deletes an existing User entity.
deleteUser :: User -> KeyDatabase.Transaction ()
deleteUser user_p =
  ERDGeneric.requiredForeignDBKey "ModData" modDataEntry keytuple2ModData
   modDataUserResponsibleKey (userKey user_p) |>>
   (ERDGeneric.requiredForeignDBKey "ModInst" modInstEntry keytuple2ModInst
     modInstUserLecturerModsKey (userKey user_p) |>>
    (ERDGeneric.requiredForeignDBKey "MasterProgram" masterProgramEntry
      keytuple2MasterProgram masterProgramUserAdvisingKey (userKey user_p) |>>
     KeyDatabase.deleteDBEntry userEntry (userKeyToKey (userKey user_p))))

--- Gets a User entity stored in the database with the given key.
getUser :: UserKey -> KeyDatabase.Transaction User
getUser key = ERDGeneric.getEntry userEntry keytuple2User (userKeyToKey key)

--- Gets all User entities stored in the database.
queryAllUsers :: KeyDatabase.Query [User]
queryAllUsers =
  KeyDatabase.transformQ (map (uncurry keytuple2User))
   (KeyDatabase.allDBKeyInfos userEntry)

--- Gets all User entities satisfying a given condition.
queryCondUser :: (User -> Bool) -> KeyDatabase.Query [User]
queryCondUser econd = KeyDatabase.transformQ (filter econd) queryAllUsers

--- Database predicate representing the relation between keys and ModData tuple entities.
modDataEntry :: ERDGeneric.Key -> ModDataTuple -> KeyDatabase.Dynamic
modDataEntry =
  KeyDatabase.persistentSQLite mdbFile "ModData"
   ["Code","NameG","NameE","Cycle","Presence","ECTS","Workload","Length","URL"
   ,"Visible","UserResponsibleKey"]

--- Dynamic predicate representing the relation
--- between keys and ModData entities.
modData :: ModDataKey -> ModData -> KeyDatabase.Dynamic
modData key obj
  | key =:= modDataKey obj = modDataEntry (modDataKeyToKey key)
                              (modData2tuple obj)

--- Gets the key of a ModData entity.
modDataKey :: ModData -> ModDataKey
modDataKey (ModData x _ _ _ _ _ _ _ _ _ _ _) = ModDataKey x

--- Shows the key of a ModData entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showModDataKey :: ModData -> String
showModDataKey obj =
  ERDGeneric.showDatabaseKey "ModData" modDataKeyToKey (modDataKey obj)

--- Transforms a string into a key of a ModData entity.
--- Nothing is returned if the string does not represent a reasonable key.
readModDataKey :: String -> Maybe ModDataKey
readModDataKey s = ERDGeneric.readDatabaseKey "ModData" ModDataKey s

modDataKeyToKey :: ModDataKey -> ERDGeneric.Key
modDataKeyToKey (ModDataKey k) = k

maybeModDataKeyToKey :: Maybe ModDataKey -> Maybe ERDGeneric.Key
maybeModDataKeyToKey Nothing = Nothing
maybeModDataKeyToKey (Just (ModDataKey k)) = Just k

--- Inserts a new ModData entity.
newModDataWithUserResponsibleKey
 :: String -> String -> String -> String -> String -> Maybe Int -> String
  -> Maybe Int -> String -> Bool -> UserKey -> KeyDatabase.Transaction ModData
newModDataWithUserResponsibleKey code_p nameG_p nameE_p cycle_p presence_p
                                 eCTS_p workload_p length_p uRL_p visible_p
                                 userResponsibleKey_p =
  ERDGeneric.unique "MDB" modDataEntry keytuple2ModData modDataCode code_p |>>
   (ERDGeneric.existsEntryWithDBKey "User" userEntry
     (userKeyToKey userResponsibleKey_p) |>>
    ERDGeneric.newEntry modDataEntry keytuple2ModData
     (code_p,nameG_p,nameE_p,cycle_p,presence_p,maybe 8 id eCTS_p,workload_p
     ,maybe 1 id length_p,uRL_p,visible_p,userKeyToKey userResponsibleKey_p))

--- Updates an existing ModData entity.
updateModData :: ModData -> KeyDatabase.Transaction ()
updateModData modData_p =
  ERDGeneric.uniqueUpdate "MDB" modDataEntry keytuple2ModData
   (modDataKeyToKey . modDataKey) modDataCode modData_p |>>
   (ERDGeneric.existsEntryWithDBKey "User" userEntry
     (userKeyToKey (modDataUserResponsibleKey modData_p)) |>>
    KeyDatabase.updateDBEntry modDataEntry
     (modDataKeyToKey (modDataKey modData_p)) (modData2tuple modData_p))

--- Deletes an existing ModData entity.
deleteModData :: ModData -> KeyDatabase.Transaction ()
deleteModData modData_p =
  ERDGeneric.requiredForeignDBKey "Categorizing" categorizingEntry
   keytuple2Categorizing categorizingModDataCategorizingKey
   (modDataKey modData_p) |>>
   (ERDGeneric.requiredForeignDBKey "ModDescr" modDescrEntry keytuple2ModDescr
     modDescrModDataDataDescKey (modDataKey modData_p) |>>
    (ERDGeneric.requiredForeignDBKey "ModInst" modInstEntry keytuple2ModInst
      modInstModDataModuleInstancesKey (modDataKey modData_p) |>>
     KeyDatabase.deleteDBEntry modDataEntry
      (modDataKeyToKey (modDataKey modData_p))))

--- Gets a ModData entity stored in the database with the given key.
getModData :: ModDataKey -> KeyDatabase.Transaction ModData
getModData key =
  ERDGeneric.getEntry modDataEntry keytuple2ModData (modDataKeyToKey key)

--- Gets all ModData entities stored in the database.
queryAllModDatas :: KeyDatabase.Query [ModData]
queryAllModDatas =
  KeyDatabase.transformQ (map (uncurry keytuple2ModData))
   (KeyDatabase.allDBKeyInfos modDataEntry)

--- Gets all ModData entities satisfying a given condition.
queryCondModData :: (ModData -> Bool) -> KeyDatabase.Query [ModData]
queryCondModData econd =
  KeyDatabase.transformQ (filter econd) queryAllModDatas

--- Gets all ModData entities of a user.
queryModDataOfUser :: UserKey -> KeyDatabase.Query [ModData]
queryModDataOfUser ukey =
  KeyDatabase.transformQ
   (map (uncurry keytuple2ModData))
   (KeyDatabase.someDBKeyInfos modDataEntry [10 @= userKeyToKey ukey])

--- Gets all ModData entities with a given module code.
queryModDataWithCode :: String -> KeyDatabase.Query [ModData]
queryModDataWithCode mcode =
  KeyDatabase.transformQ
   (map (uncurry keytuple2ModData))
   (KeyDatabase.someDBKeyInfos modDataEntry [0 @= mcode])

--- Database predicate representing the relation between keys and ModDescr tuple entities.
modDescrEntry :: ERDGeneric.Key -> ModDescrTuple -> KeyDatabase.Dynamic
modDescrEntry =
  KeyDatabase.persistentSQLite mdbFile "ModDescr"
   ["Language","ShortDesc","Objectives","Contents","Prereq","Exam","Methods"
   ,"Use","Literature","Links","Comments","ModDataDataDescKey"]

--- Dynamic predicate representing the relation
--- between keys and ModDescr entities.
modDescr :: ModDescrKey -> ModDescr -> KeyDatabase.Dynamic
modDescr key obj
  | key =:= modDescrKey obj = modDescrEntry (modDescrKeyToKey key)
                               (modDescr2tuple obj)

--- Gets the key of a ModDescr entity.
modDescrKey :: ModDescr -> ModDescrKey
modDescrKey (ModDescr x _ _ _ _ _ _ _ _ _ _ _ _) = ModDescrKey x

--- Shows the key of a ModDescr entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showModDescrKey :: ModDescr -> String
showModDescrKey obj =
  ERDGeneric.showDatabaseKey "ModDescr" modDescrKeyToKey (modDescrKey obj)

--- Transforms a string into a key of a ModDescr entity.
--- Nothing is returned if the string does not represent a reasonable key.
readModDescrKey :: String -> Maybe ModDescrKey
readModDescrKey s = ERDGeneric.readDatabaseKey "ModDescr" ModDescrKey s

modDescrKeyToKey :: ModDescrKey -> ERDGeneric.Key
modDescrKeyToKey (ModDescrKey k) = k

maybeModDescrKeyToKey :: Maybe ModDescrKey -> Maybe ERDGeneric.Key
maybeModDescrKeyToKey Nothing = Nothing
maybeModDescrKeyToKey (Just (ModDescrKey k)) = Just k

--- Inserts a new ModDescr entity.
newModDescrWithModDataDataDescKey
 :: String -> String -> String -> String -> String -> String -> String
  -> String -> String -> String -> String -> ModDataKey
  -> KeyDatabase.Transaction ModDescr
newModDescrWithModDataDataDescKey language_p shortDesc_p objectives_p
                                  contents_p prereq_p exam_p methods_p use_p
                                  literature_p links_p comments_p
                                  modDataDataDescKey_p =
  ERDGeneric.unique "MDB" modDescrEntry keytuple2ModDescr
   modDescrModDataDataDescKey modDataDataDescKey_p |>>
   (ERDGeneric.existsEntryWithDBKey "ModData" modDataEntry
     (modDataKeyToKey modDataDataDescKey_p) |>>
    ERDGeneric.newEntry modDescrEntry keytuple2ModDescr
     (language_p,shortDesc_p,objectives_p,contents_p,prereq_p,exam_p,methods_p
     ,use_p,literature_p,links_p,comments_p
     ,modDataKeyToKey modDataDataDescKey_p))

--- Updates an existing ModDescr entity.
updateModDescr :: ModDescr -> KeyDatabase.Transaction ()
updateModDescr modDescr_p =
  ERDGeneric.uniqueUpdate "MDB" modDescrEntry keytuple2ModDescr
   (modDescrKeyToKey . modDescrKey) modDescrModDataDataDescKey modDescr_p |>>
   (ERDGeneric.existsEntryWithDBKey "ModData" modDataEntry
     (modDataKeyToKey (modDescrModDataDataDescKey modDescr_p)) |>>
    KeyDatabase.updateDBEntry modDescrEntry
     (modDescrKeyToKey (modDescrKey modDescr_p)) (modDescr2tuple modDescr_p))

--- Deletes an existing ModDescr entity.
deleteModDescr :: ModDescr -> KeyDatabase.Transaction ()
deleteModDescr modDescr_p =
  KeyDatabase.deleteDBEntry modDescrEntry
   (modDescrKeyToKey (modDescrKey modDescr_p))

--- Gets a ModDescr entity stored in the database with the given key.
getModDescr :: ModDescrKey -> KeyDatabase.Transaction ModDescr
getModDescr key =
  ERDGeneric.getEntry modDescrEntry keytuple2ModDescr (modDescrKeyToKey key)

--- Gets all ModDescr entities stored in the database.
queryAllModDescrs :: KeyDatabase.Query [ModDescr]
queryAllModDescrs =
  KeyDatabase.transformQ (map (uncurry keytuple2ModDescr))
   (KeyDatabase.allDBKeyInfos modDescrEntry)

--- Gets all ModDescr entities satisfying a given condition.
queryCondModDescr :: (ModDescr -> Bool) -> KeyDatabase.Query [ModDescr]
queryCondModDescr econd =
  KeyDatabase.transformQ (filter econd) queryAllModDescrs

--- Gets all RecipeDescription entities stored in the database.
queryDescriptionOfMod :: ModDataKey -> KeyDatabase.Query (Maybe ModDescr)
queryDescriptionOfMod mdk =
  KeyDatabase.transformQ
   ((\l -> if null l then Nothing else Just (head l))
      . map (uncurry keytuple2ModDescr))
   (KeyDatabase.someDBKeyInfos modDescrEntry [11 @= modDataKeyToKey mdk])

--- Database predicate representing the relation between keys and ModInst tuple entities.
modInstEntry :: ERDGeneric.Key -> ModInstTuple -> KeyDatabase.Dynamic
modInstEntry =
  KeyDatabase.persistentSQLite mdbFile "ModInst"
   ["Term","Year","UserLecturerModsKey","ModDataModuleInstancesKey"]

--- Dynamic predicate representing the relation
--- between keys and ModInst entities.
modInst :: ModInstKey -> ModInst -> KeyDatabase.Dynamic
modInst key obj
  | key =:= modInstKey obj = modInstEntry (modInstKeyToKey key)
                              (modInst2tuple obj)

--- Gets the key of a ModInst entity.
modInstKey :: ModInst -> ModInstKey
modInstKey (ModInst x _ _ _ _) = ModInstKey x

--- Shows the key of a ModInst entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showModInstKey :: ModInst -> String
showModInstKey obj =
  ERDGeneric.showDatabaseKey "ModInst" modInstKeyToKey (modInstKey obj)

--- Transforms a string into a key of a ModInst entity.
--- Nothing is returned if the string does not represent a reasonable key.
readModInstKey :: String -> Maybe ModInstKey
readModInstKey s = ERDGeneric.readDatabaseKey "ModInst" ModInstKey s

modInstKeyToKey :: ModInstKey -> ERDGeneric.Key
modInstKeyToKey (ModInstKey k) = k

maybeModInstKeyToKey :: Maybe ModInstKey -> Maybe ERDGeneric.Key
maybeModInstKeyToKey Nothing = Nothing
maybeModInstKeyToKey (Just (ModInstKey k)) = Just k

--- Inserts a new ModInst entity.
newModInstWithUserLecturerModsKeyWithModDataModuleInstancesKey
 :: String -> Maybe Int -> UserKey -> ModDataKey
  -> KeyDatabase.Transaction ModInst
newModInstWithUserLecturerModsKeyWithModDataModuleInstancesKey term_p year_p
                                                               userLecturerModsKey_p
                                                               modDataModuleInstancesKey_p =
  ERDGeneric.existsEntryWithDBKey "User" userEntry
   (userKeyToKey userLecturerModsKey_p) |>>
   (ERDGeneric.existsEntryWithDBKey "ModData" modDataEntry
     (modDataKeyToKey modDataModuleInstancesKey_p) |>>
    ERDGeneric.newEntry modInstEntry keytuple2ModInst
     (term_p,maybe 2011 id year_p,userKeyToKey userLecturerModsKey_p
     ,modDataKeyToKey modDataModuleInstancesKey_p))

--- Updates an existing ModInst entity.
updateModInst :: ModInst -> KeyDatabase.Transaction ()
updateModInst modInst_p =
  ERDGeneric.existsEntryWithDBKey "User" userEntry
   (userKeyToKey (modInstUserLecturerModsKey modInst_p)) |>>
   (ERDGeneric.existsEntryWithDBKey "ModData" modDataEntry
     (modDataKeyToKey (modInstModDataModuleInstancesKey modInst_p)) |>>
    KeyDatabase.updateDBEntry modInstEntry
     (modInstKeyToKey (modInstKey modInst_p)) (modInst2tuple modInst_p))

--- Deletes an existing ModInst entity.
deleteModInst :: ModInst -> KeyDatabase.Transaction ()
deleteModInst modInst_p =
  KeyDatabase.deleteDBEntry modInstEntry
   (modInstKeyToKey (modInstKey modInst_p))

--- Gets a ModInst entity stored in the database with the given key.
getModInst :: ModInstKey -> KeyDatabase.Transaction ModInst
getModInst key =
  ERDGeneric.getEntry modInstEntry keytuple2ModInst (modInstKeyToKey key)

--- Gets all ModInst entities stored in the database.
queryAllModInsts :: KeyDatabase.Query [ModInst]
queryAllModInsts =
  KeyDatabase.transformQ (map (uncurry keytuple2ModInst))
   (KeyDatabase.allDBKeyInfos modInstEntry)

--- Gets all ModInst entities satisfying a given condition.
queryCondModInst :: (ModInst -> Bool) -> KeyDatabase.Query [ModInst]
queryCondModInst econd =
  KeyDatabase.transformQ (filter econd) queryAllModInsts

--- Gets all module instances for a given module (key).
queryInstancesOfMod :: ModDataKey -> KeyDatabase.Query [ModInst]
queryInstancesOfMod mdk =
  KeyDatabase.transformQ
   (map (uncurry keytuple2ModInst))
   (KeyDatabase.someDBKeyInfos modInstEntry [3 @= modDataKeyToKey mdk])

--- Database predicate representing the relation between keys and MasterProgram tuple entities.
masterProgramEntry
 :: ERDGeneric.Key -> MasterProgramTuple -> KeyDatabase.Dynamic
masterProgramEntry =
  KeyDatabase.persistentSQLite mdbFile "MasterProgram"
   ["Name","Term","Year","Desc","Prereq","Comments","Visible"
   ,"UserAdvisingKey","MasterCoreAreaAreaProgramsKey"]

--- Dynamic predicate representing the relation
--- between keys and MasterProgram entities.
masterProgram :: MasterProgramKey -> MasterProgram -> KeyDatabase.Dynamic
masterProgram key obj
  | key =:= masterProgramKey obj = masterProgramEntry
                                    (masterProgramKeyToKey key)
                                    (masterProgram2tuple obj)

--- Gets the key of a MasterProgram entity.
masterProgramKey :: MasterProgram -> MasterProgramKey
masterProgramKey (MasterProgram x _ _ _ _ _ _ _ _ _) = MasterProgramKey x

--- Shows the key of a MasterProgram entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showMasterProgramKey :: MasterProgram -> String
showMasterProgramKey obj =
  ERDGeneric.showDatabaseKey "MasterProgram" masterProgramKeyToKey
   (masterProgramKey obj)

--- Transforms a string into a key of a MasterProgram entity.
--- Nothing is returned if the string does not represent a reasonable key.
readMasterProgramKey :: String -> Maybe MasterProgramKey
readMasterProgramKey s =
  ERDGeneric.readDatabaseKey "MasterProgram" MasterProgramKey s

masterProgramKeyToKey :: MasterProgramKey -> ERDGeneric.Key
masterProgramKeyToKey (MasterProgramKey k) = k

maybeMasterProgramKeyToKey :: Maybe MasterProgramKey -> Maybe ERDGeneric.Key
maybeMasterProgramKeyToKey Nothing = Nothing
maybeMasterProgramKeyToKey (Just (MasterProgramKey k)) = Just k

--- Inserts a new MasterProgram entity.
newMasterProgramWithUserAdvisingKeyWithMasterCoreAreaAreaProgramsKey
 :: String -> String -> Maybe Int -> String -> String -> String -> Bool
  -> UserKey -> MasterCoreAreaKey -> KeyDatabase.Transaction MasterProgram
newMasterProgramWithUserAdvisingKeyWithMasterCoreAreaAreaProgramsKey name_p
                                                                     term_p
                                                                     year_p
                                                                     desc_p
                                                                     prereq_p
                                                                     comments_p
                                                                     visible_p
                                                                     userAdvisingKey_p
                                                                     masterCoreAreaAreaProgramsKey_p =
  ERDGeneric.existsEntryWithDBKey "User" userEntry
   (userKeyToKey userAdvisingKey_p) |>>
   (ERDGeneric.existsEntryWithDBKey "MasterCoreArea" masterCoreAreaEntry
     (masterCoreAreaKeyToKey masterCoreAreaAreaProgramsKey_p) |>>
    ERDGeneric.newEntry masterProgramEntry keytuple2MasterProgram
     (name_p,term_p,maybe 2011 id year_p,desc_p,prereq_p,comments_p,visible_p
     ,userKeyToKey userAdvisingKey_p
     ,masterCoreAreaKeyToKey masterCoreAreaAreaProgramsKey_p))

--- Updates an existing MasterProgram entity.
updateMasterProgram :: MasterProgram -> KeyDatabase.Transaction ()
updateMasterProgram masterProgram_p =
  ERDGeneric.existsEntryWithDBKey "User" userEntry
   (userKeyToKey (masterProgramUserAdvisingKey masterProgram_p)) |>>
   (ERDGeneric.existsEntryWithDBKey "MasterCoreArea" masterCoreAreaEntry
     (masterCoreAreaKeyToKey
       (masterProgramMasterCoreAreaAreaProgramsKey masterProgram_p)) |>>
    KeyDatabase.updateDBEntry masterProgramEntry
     (masterProgramKeyToKey (masterProgramKey masterProgram_p))
     (masterProgram2tuple masterProgram_p))

--- Deletes an existing MasterProgram entity.
deleteMasterProgram :: MasterProgram -> KeyDatabase.Transaction ()
deleteMasterProgram masterProgram_p =
  ERDGeneric.requiredForeignDBKey "MasterProgInfo" masterProgInfoEntry
   keytuple2MasterProgInfo masterProgInfoMasterProgramProgramInfoKey
   (masterProgramKey masterProgram_p) |>>
   KeyDatabase.deleteDBEntry masterProgramEntry
    (masterProgramKeyToKey (masterProgramKey masterProgram_p))

--- Gets a MasterProgram entity stored in the database with the given key.
getMasterProgram :: MasterProgramKey -> KeyDatabase.Transaction MasterProgram
getMasterProgram key =
  ERDGeneric.getEntry masterProgramEntry keytuple2MasterProgram
   (masterProgramKeyToKey key)

--- Gets all MasterProgram entities stored in the database.
queryAllMasterPrograms :: KeyDatabase.Query [MasterProgram]
queryAllMasterPrograms =
  KeyDatabase.transformQ (map (uncurry keytuple2MasterProgram))
   (KeyDatabase.allDBKeyInfos masterProgramEntry)

--- Gets all MasterProgram entities satisfying a given condition.
queryCondMasterProgram
 :: (MasterProgram -> Bool) -> KeyDatabase.Query [MasterProgram]
queryCondMasterProgram econd =
  KeyDatabase.transformQ (filter econd) queryAllMasterPrograms

--- Gets all MasterProgram entities belonging to a user.
queryMasterProgramOfUser :: UserKey -> KeyDatabase.Query [MasterProgram]
queryMasterProgramOfUser uk =
  KeyDatabase.transformQ
   (map (uncurry keytuple2MasterProgram))
   (KeyDatabase.someDBKeyInfos masterProgramEntry
                               [7 @= userKeyToKey uk])

--- Gets all MasterProgram (keys) for each ModInst of a given ModInst list.
getMasterProgramKeysOfModInst :: [ModInst] -> Query [[MasterProgramKey]]
getMasterProgramKeysOfModInst mis =
  transformQ
   (\mpis ->
     map (\mi -> let mdk = modInstModDataModuleInstancesKey mi in
           map masterProgInfoMasterProgramProgramInfoKey
            (filter (\mpi ->
                       any (\ (_,_,smpk,trm,yr) ->
                               readModDataKey smpk == Just mdk &&
                               modInstTerm mi == trm && modInstYear mi == yr)
                           (readQTerm (masterProgInfoProgModules mpi)))
                    mpis))
         mis)
   queryAllMasterProgInfos

--- Database predicate representing the relation between keys and MasterProgInfo tuple entities.
masterProgInfoEntry
 :: ERDGeneric.Key -> MasterProgInfoTuple -> KeyDatabase.Dynamic
masterProgInfoEntry =
  KeyDatabase.persistentSQLite mdbFile "MasterProgInfo"
   ["ProgModules","Praktikum","Seminar","Thesis","AllgGrundlagen"
   ,"Anwendungsfach","MasterProgramProgramInfoKey"]

--- Dynamic predicate representing the relation
--- between keys and MasterProgInfo entities.
masterProgInfo :: MasterProgInfoKey -> MasterProgInfo -> KeyDatabase.Dynamic
masterProgInfo key obj
  | key =:= masterProgInfoKey obj = masterProgInfoEntry
                                     (masterProgInfoKeyToKey key)
                                     (masterProgInfo2tuple obj)

--- Gets the key of a MasterProgInfo entity.
masterProgInfoKey :: MasterProgInfo -> MasterProgInfoKey
masterProgInfoKey (MasterProgInfo x _ _ _ _ _ _ _) = MasterProgInfoKey x

--- Shows the key of a MasterProgInfo entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showMasterProgInfoKey :: MasterProgInfo -> String
showMasterProgInfoKey obj =
  ERDGeneric.showDatabaseKey "MasterProgInfo" masterProgInfoKeyToKey
   (masterProgInfoKey obj)

--- Transforms a string into a key of a MasterProgInfo entity.
--- Nothing is returned if the string does not represent a reasonable key.
readMasterProgInfoKey :: String -> Maybe MasterProgInfoKey
readMasterProgInfoKey s =
  ERDGeneric.readDatabaseKey "MasterProgInfo" MasterProgInfoKey s

masterProgInfoKeyToKey :: MasterProgInfoKey -> ERDGeneric.Key
masterProgInfoKeyToKey (MasterProgInfoKey k) = k

maybeMasterProgInfoKeyToKey :: Maybe MasterProgInfoKey -> Maybe ERDGeneric.Key
maybeMasterProgInfoKeyToKey Nothing = Nothing
maybeMasterProgInfoKeyToKey (Just (MasterProgInfoKey k)) = Just k

--- Inserts a new MasterProgInfo entity.
newMasterProgInfoWithMasterProgramProgramInfoKey
 :: String -> String -> String -> String -> String -> String
  -> MasterProgramKey -> KeyDatabase.Transaction MasterProgInfo
newMasterProgInfoWithMasterProgramProgramInfoKey progModules_p praktikum_p
                                                 seminar_p thesis_p
                                                 allgGrundlagen_p
                                                 anwendungsfach_p
                                                 masterProgramProgramInfoKey_p =
  ERDGeneric.existsEntryWithDBKey "MasterProgram" masterProgramEntry
   (masterProgramKeyToKey masterProgramProgramInfoKey_p) |>>
   ERDGeneric.newEntry masterProgInfoEntry keytuple2MasterProgInfo
    (progModules_p,praktikum_p,seminar_p,thesis_p,allgGrundlagen_p
    ,anwendungsfach_p,masterProgramKeyToKey masterProgramProgramInfoKey_p)

--- Updates an existing MasterProgInfo entity.
updateMasterProgInfo :: MasterProgInfo -> KeyDatabase.Transaction ()
updateMasterProgInfo masterProgInfo_p =
  ERDGeneric.existsEntryWithDBKey "MasterProgram" masterProgramEntry
   (masterProgramKeyToKey
     (masterProgInfoMasterProgramProgramInfoKey masterProgInfo_p)) |>>
   KeyDatabase.updateDBEntry masterProgInfoEntry
    (masterProgInfoKeyToKey (masterProgInfoKey masterProgInfo_p))
    (masterProgInfo2tuple masterProgInfo_p)

--- Deletes an existing MasterProgInfo entity.
deleteMasterProgInfo :: MasterProgInfo -> KeyDatabase.Transaction ()
deleteMasterProgInfo masterProgInfo_p =
  KeyDatabase.deleteDBEntry masterProgInfoEntry
   (masterProgInfoKeyToKey (masterProgInfoKey masterProgInfo_p))

--- Gets a MasterProgInfo entity stored in the database with the given key.
getMasterProgInfo
 :: MasterProgInfoKey -> KeyDatabase.Transaction MasterProgInfo
getMasterProgInfo key =
  ERDGeneric.getEntry masterProgInfoEntry keytuple2MasterProgInfo
   (masterProgInfoKeyToKey key)

--- Gets all MasterProgInfo entities stored in the database.
queryAllMasterProgInfos :: KeyDatabase.Query [MasterProgInfo]
queryAllMasterProgInfos =
  KeyDatabase.transformQ (map (uncurry keytuple2MasterProgInfo))
   (KeyDatabase.allDBKeyInfos masterProgInfoEntry)

--- Gets all MasterProgInfo entities satisfying a given condition.
queryCondMasterProgInfo
 :: (MasterProgInfo -> Bool) -> KeyDatabase.Query [MasterProgInfo]
queryCondMasterProgInfo econd =
  KeyDatabase.transformQ (filter econd) queryAllMasterProgInfos

--- Gets all RecipeDescription entities stored in the database.
queryInfoOfMasterProgram :: MasterProgramKey
                         -> KeyDatabase.Query (Maybe MasterProgInfo)
queryInfoOfMasterProgram mpk =
  KeyDatabase.transformQ
   ((\l -> if null l then Nothing else Just (head l))
      . map (uncurry keytuple2MasterProgInfo))
   (KeyDatabase.someDBKeyInfos masterProgInfoEntry
                               [6 @= masterProgramKeyToKey mpk])

--- Database predicate representing the relation between keys and UnivisInfo tuple entities.
univisInfoEntry :: ERDGeneric.Key -> UnivisInfoTuple -> KeyDatabase.Dynamic
univisInfoEntry =
  KeyDatabase.persistentSQLite mdbFile "UnivisInfo"
   ["Code","Term","Year","URL"]

--- Dynamic predicate representing the relation
--- between keys and UnivisInfo entities.
univisInfo :: UnivisInfoKey -> UnivisInfo -> KeyDatabase.Dynamic
univisInfo key obj
  | key =:= univisInfoKey obj = univisInfoEntry (univisInfoKeyToKey key)
                                 (univisInfo2tuple obj)

--- Gets the key of a UnivisInfo entity.
univisInfoKey :: UnivisInfo -> UnivisInfoKey
univisInfoKey (UnivisInfo x _ _ _ _) = UnivisInfoKey x

--- Shows the key of a UnivisInfo entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showUnivisInfoKey :: UnivisInfo -> String
showUnivisInfoKey obj =
  ERDGeneric.showDatabaseKey "UnivisInfo" univisInfoKeyToKey
   (univisInfoKey obj)

--- Transforms a string into a key of a UnivisInfo entity.
--- Nothing is returned if the string does not represent a reasonable key.
readUnivisInfoKey :: String -> Maybe UnivisInfoKey
readUnivisInfoKey s = ERDGeneric.readDatabaseKey "UnivisInfo" UnivisInfoKey s

univisInfoKeyToKey :: UnivisInfoKey -> ERDGeneric.Key
univisInfoKeyToKey (UnivisInfoKey k) = k

maybeUnivisInfoKeyToKey :: Maybe UnivisInfoKey -> Maybe ERDGeneric.Key
maybeUnivisInfoKeyToKey Nothing = Nothing
maybeUnivisInfoKeyToKey (Just (UnivisInfoKey k)) = Just k

--- Inserts a new UnivisInfo entity.
newUnivisInfo
 :: String -> String -> Int -> String -> KeyDatabase.Transaction UnivisInfo
newUnivisInfo code_p term_p year_p uRL_p =
  ERDGeneric.newEntry univisInfoEntry keytuple2UnivisInfo
   (code_p,term_p,year_p,uRL_p)

--- Updates an existing UnivisInfo entity.
updateUnivisInfo :: UnivisInfo -> KeyDatabase.Transaction ()
updateUnivisInfo univisInfo_p =
  KeyDatabase.updateDBEntry univisInfoEntry
   (univisInfoKeyToKey (univisInfoKey univisInfo_p))
   (univisInfo2tuple univisInfo_p)

--- Deletes an existing UnivisInfo entity.
deleteUnivisInfo :: UnivisInfo -> KeyDatabase.Transaction ()
deleteUnivisInfo univisInfo_p =
  KeyDatabase.deleteDBEntry univisInfoEntry
   (univisInfoKeyToKey (univisInfoKey univisInfo_p))

--- Gets a UnivisInfo entity stored in the database with the given key.
getUnivisInfo :: UnivisInfoKey -> KeyDatabase.Transaction UnivisInfo
getUnivisInfo key =
  ERDGeneric.getEntry univisInfoEntry keytuple2UnivisInfo
   (univisInfoKeyToKey key)

--- Gets all UnivisInfo entities stored in the database.
queryAllUnivisInfos :: KeyDatabase.Query [UnivisInfo]
queryAllUnivisInfos =
  KeyDatabase.transformQ (map (uncurry keytuple2UnivisInfo))
   (KeyDatabase.allDBKeyInfos univisInfoEntry)

--- Gets all UnivisInfo entities satisfying a given condition.
queryCondUnivisInfo :: (UnivisInfo -> Bool) -> KeyDatabase.Query [UnivisInfo]
queryCondUnivisInfo econd =
  KeyDatabase.transformQ (filter econd) queryAllUnivisInfos

--- query the univis URLs for a module in a semester:
queryUnivisURL :: String -> (String,Int) -> Query [String]
queryUnivisURL mcode (term,year) = 
  KeyDatabase.transformQ
   (map (univisInfoURL . uncurry keytuple2UnivisInfo))
   (KeyDatabase.someDBKeyInfos univisInfoEntry [0@=mcode, 1@=term, 2@=year])

--- query whether a module has a UnivIS instance in a semester:
queryHasUnivisEntry :: String -> (String,Int) -> Query Bool
queryHasUnivisEntry mcode (term,year) = 
  KeyDatabase.transformQ
   (not . null)
   (KeyDatabase.someDBKeyInfos univisInfoEntry [0@=mcode, 1@=term, 2@=year])

--- Database predicate representing the relation between keys and Categorizing tuple entities.
categorizingEntry
 :: ERDGeneric.Key -> CategorizingTuple -> KeyDatabase.Dynamic
categorizingEntry =
  KeyDatabase.persistentSQLite mdbFile "Categorizing"
   ["ModDataCategorizingKey","CategoryCategorizingKey"]

categorizingModDataCategorizingKey :: Categorizing -> ModDataKey
categorizingModDataCategorizingKey (Categorizing x _) = ModDataKey x

categorizingCategoryCategorizingKey :: Categorizing -> CategoryKey
categorizingCategoryCategorizingKey (Categorizing _ x) = CategoryKey x

--- Dynamic predicate representing the Categorizing relation between ModData entities and Category entities
categorizing :: ModDataKey -> CategoryKey -> KeyDatabase.Dynamic
categorizing (ModDataKey key1) (CategoryKey key2) =
  categorizingEntry unknown (key1,key2)

--- Inserts a new Categorizing relation between a ModData entity and a Category entity
newCategorizing :: ModDataKey -> CategoryKey -> KeyDatabase.Transaction ()
newCategorizing key1 key2 =
  ERDGeneric.existsEntryWithDBKey "ModData" modDataEntry
   (modDataKeyToKey key1) |>>
   (ERDGeneric.existsEntryWithDBKey "Category" categoryEntry
     (categoryKeyToKey key2) |>>
    (ERDGeneric.unique2 categorizingEntry (modDataKeyToKey key1)
      (categoryKeyToKey key2) |>>
     ERDGeneric.newEntryR categorizingEntry (modDataKeyToKey key1)
      (categoryKeyToKey key2)))

--- Deletes an existing Categorizing relation between a ModData entity and a Category entity
deleteCategorizing :: ModDataKey -> CategoryKey -> KeyDatabase.Transaction ()
deleteCategorizing key1 key2 =
  ERDGeneric.minTestDelete "Categorizing" categorizingEntry
   keytuple2Categorizing categorizingModDataCategorizingKey 1 key1 |>>
   ERDGeneric.deleteEntryR categorizingEntry (modDataKeyToKey key1)
    (categoryKeyToKey key2)

--- Gets the associated Category entities for a given ModData entity
getModDataCategorys :: ModData -> KeyDatabase.Transaction [Category]
getModDataCategorys e = getModDataKeyCategorys (modDataKey e)

--- Gets the associated Category entities for a given ModDataKey
getModDataKeyCategorys :: ModDataKey -> KeyDatabase.Transaction [Category]
getModDataKeyCategorys mdk =
  KeyDatabase.getDB (queryModDataKeyCategorys mdk) |>>=
  KeyDatabase.mapT getCategory . map categorizingCategoryCategorizingKey

--- Gets all Categorizing relationship entities stored in the database.
queryAllCategorizings :: KeyDatabase.Query [Categorizing]
queryAllCategorizings =
  KeyDatabase.transformQ (map (uncurry keytuple2Categorizing))
   (KeyDatabase.allDBKeyInfos categorizingEntry)

--- Gets all Categorizing relationship entities satisfying a given condition.
queryCondCategorizing
 :: (Categorizing -> Bool) -> KeyDatabase.Query [Categorizing]
queryCondCategorizing econd =
  KeyDatabase.transformQ (filter econd) queryAllCategorizings

--- Gets all categories for a given module (key).
queryModDataKeyCategorys :: ModDataKey -> KeyDatabase.Query [Categorizing]
queryModDataKeyCategorys mdk =
  KeyDatabase.transformQ
   (map (uncurry keytuple2Categorizing))
   (KeyDatabase.someDBKeyInfos categorizingEntry [0 @= modDataKeyToKey mdk])

--- Gets all categories for a given module (key).
queryModDataKeysOfCategory :: CategoryKey -> KeyDatabase.Query [ModDataKey]
queryModDataKeysOfCategory ck =
  KeyDatabase.transformQ
   (map (categorizingModDataCategorizingKey . uncurry keytuple2Categorizing))
   (KeyDatabase.someDBKeyInfos categorizingEntry [1 @= categoryKeyToKey ck])

--- Dynamic predicate representing the ProgramInfo relation
--- between MasterProgram entities and MasterProgInfo entities.
programInfo :: MasterProgramKey -> MasterProgInfoKey -> KeyDatabase.Dynamic
programInfo key1 key2
  | masterProgInfoMasterProgramProgramInfoKey en =:=
     key1 = masterProgInfoEntry (masterProgInfoKeyToKey key2)
             (masterProgInfo2tuple en)
  where en free

--- Dynamic predicate representing role "withProgInfo".
withProgInfo :: MasterProgramKey -> MasterProgInfoKey -> KeyDatabase.Dynamic
withProgInfo = programInfo

--- Dynamic predicate representing role "withProgInfo".
programInfoOf :: MasterProgInfoKey -> MasterProgramKey -> KeyDatabase.Dynamic
programInfoOf = flip withProgInfo

--- Dynamic predicate representing the AreaPrograms relation
--- between MasterCoreArea entities and MasterProgram entities.
areaPrograms :: MasterCoreAreaKey -> MasterProgramKey -> KeyDatabase.Dynamic
areaPrograms key1 key2
  | masterProgramMasterCoreAreaAreaProgramsKey en =:=
     key1 = masterProgramEntry (masterProgramKeyToKey key2)
             (masterProgram2tuple en)
  where en free

--- Dynamic predicate representing role "withProgram".
withProgram :: MasterCoreAreaKey -> MasterProgramKey -> KeyDatabase.Dynamic
withProgram = areaPrograms

--- Dynamic predicate representing role "withProgram".
ofCoreArea :: MasterProgramKey -> MasterCoreAreaKey -> KeyDatabase.Dynamic
ofCoreArea = flip withProgram

--- Dynamic predicate representing the Advising relation
--- between User entities and MasterProgram entities.
advising :: UserKey -> MasterProgramKey -> KeyDatabase.Dynamic
advising key1 key2
  | masterProgramUserAdvisingKey en =:= key1 = masterProgramEntry
                                                (masterProgramKeyToKey key2)
                                                (masterProgram2tuple en)
  where en free

--- Dynamic predicate representing role "organizes".
organizes :: UserKey -> MasterProgramKey -> KeyDatabase.Dynamic
organizes = advising

--- Dynamic predicate representing role "organizes".
organizedBy :: MasterProgramKey -> UserKey -> KeyDatabase.Dynamic
organizedBy = flip organizes

--- Dynamic predicate representing the ModuleInstances relation
--- between ModData entities and ModInst entities.
moduleInstances :: ModDataKey -> ModInstKey -> KeyDatabase.Dynamic
moduleInstances key1 key2
  | modInstModDataModuleInstancesKey en =:= key1 = modInstEntry
                                                    (modInstKeyToKey key2)
                                                    (modInst2tuple en)
  where en free

--- Dynamic predicate representing role "instOfModule".
instOfModule :: ModDataKey -> ModInstKey -> KeyDatabase.Dynamic
instOfModule = moduleInstances

--- Dynamic predicate representing role "instOfModule".
withModule :: ModInstKey -> ModDataKey -> KeyDatabase.Dynamic
withModule = flip instOfModule

--- Dynamic predicate representing the LecturerMods relation
--- between User entities and ModInst entities.
lecturerMods :: UserKey -> ModInstKey -> KeyDatabase.Dynamic
lecturerMods key1 key2
  | modInstUserLecturerModsKey en =:= key1 = modInstEntry
                                              (modInstKeyToKey key2)
                                              (modInst2tuple en)
  where en free

--- Dynamic predicate representing role "instOfLecturer".
instOfLecturer :: UserKey -> ModInstKey -> KeyDatabase.Dynamic
instOfLecturer = lecturerMods

--- Dynamic predicate representing role "instOfLecturer".
withLecturer :: ModInstKey -> UserKey -> KeyDatabase.Dynamic
withLecturer = flip instOfLecturer

--- Dynamic predicate representing the DataDesc relation
--- between ModData entities and ModDescr entities.
dataDesc :: ModDataKey -> ModDescrKey -> KeyDatabase.Dynamic
dataDesc key1 key2
  | modDescrModDataDataDescKey en =:= key1 = modDescrEntry
                                              (modDescrKeyToKey key2)
                                              (modDescr2tuple en)
  where en free

--- Dynamic predicate representing role "withDesc".
withDesc :: ModDataKey -> ModDescrKey -> KeyDatabase.Dynamic
withDesc = dataDesc

--- Dynamic predicate representing role "withDesc".
descOf :: ModDescrKey -> ModDataKey -> KeyDatabase.Dynamic
descOf = flip withDesc

--- Dynamic predicate representing role "belongsTo".
belongsTo :: ModDataKey -> CategoryKey -> KeyDatabase.Dynamic
belongsTo = categorizing

--- Dynamic predicate representing role "contains".
contains :: CategoryKey -> ModDataKey -> KeyDatabase.Dynamic
contains = flip categorizing

--- Dynamic predicate representing the Responsible relation
--- between User entities and ModData entities.
responsible :: UserKey -> ModDataKey -> KeyDatabase.Dynamic
responsible key1 key2
  | modDataUserResponsibleKey en =:= key1 = modDataEntry
                                             (modDataKeyToKey key2)
                                             (modData2tuple en)
  where en free

--- Dynamic predicate representing role "responsibleFor".
responsibleFor :: UserKey -> ModDataKey -> KeyDatabase.Dynamic
responsibleFor = responsible

--- Dynamic predicate representing role "responsibleFor".
managedBy :: ModDataKey -> UserKey -> KeyDatabase.Dynamic
managedBy = flip responsibleFor

--- Dynamic predicate representing the ProgramCategories relation
--- between StudyProgram entities and Category entities.
programCategories :: StudyProgramKey -> CategoryKey -> KeyDatabase.Dynamic
programCategories key1 key2
  | categoryStudyProgramProgramCategoriesKey en =:= key1 = categoryEntry
                                                            (categoryKeyToKey
                                                              key2)
                                                           
                                                            (category2tuple
                                                              en)
  where en free

--- Dynamic predicate representing role "withCategory".
withCategory :: StudyProgramKey -> CategoryKey -> KeyDatabase.Dynamic
withCategory = programCategories

--- Dynamic predicate representing role "withCategory".
ofProgram :: CategoryKey -> StudyProgramKey -> KeyDatabase.Dynamic
ofProgram = flip withCategory

--- Checks the consistency of the complete database.
checkAllData :: KeyDatabase.Transaction ()
checkAllData =
  checkCategorizing |>>
   (checkStudyProgram |>>
    (checkCategory |>>
     (checkMasterCoreArea |>>
      (checkUser |>>
       (checkModData |>>
        (checkModDescr |>>
         (checkModInst |>>
          (checkMasterProgram |>>
           (checkMasterProgInfo |>> checkUnivisInfo)))))))))

--- Checks the consistency of the database for Categorizing entities.
checkCategorizing :: KeyDatabase.Transaction ()
checkCategorizing =
  KeyDatabase.getDB (KeyDatabase.allDBKeyInfos categorizingEntry) |>>=
   (KeyDatabase.mapT_ checkCategorizingEntry .
    map (uncurry keytuple2Categorizing))

--- Checks the consistency of the database for StudyProgram entities.
checkStudyProgram :: KeyDatabase.Transaction ()
checkStudyProgram =
  KeyDatabase.getDB (KeyDatabase.allDBKeyInfos studyProgramEntry) |>>=
   (KeyDatabase.mapT_ checkStudyProgramEntry .
    map (uncurry keytuple2StudyProgram))

--- Checks the consistency of the database for Category entities.
checkCategory :: KeyDatabase.Transaction ()
checkCategory =
  KeyDatabase.getDB (KeyDatabase.allDBKeyInfos categoryEntry) |>>=
   (KeyDatabase.mapT_ checkCategoryEntry . map (uncurry keytuple2Category))

--- Checks the consistency of the database for MasterCoreArea entities.
checkMasterCoreArea :: KeyDatabase.Transaction ()
checkMasterCoreArea =
  KeyDatabase.getDB (KeyDatabase.allDBKeyInfos masterCoreAreaEntry) |>>=
   (KeyDatabase.mapT_ checkMasterCoreAreaEntry .
    map (uncurry keytuple2MasterCoreArea))

--- Checks the consistency of the database for User entities.
checkUser :: KeyDatabase.Transaction ()
checkUser =
  KeyDatabase.getDB (KeyDatabase.allDBKeyInfos userEntry) |>>=
   (KeyDatabase.mapT_ checkUserEntry . map (uncurry keytuple2User))

--- Checks the consistency of the database for ModData entities.
checkModData :: KeyDatabase.Transaction ()
checkModData =
  KeyDatabase.getDB (KeyDatabase.allDBKeyInfos modDataEntry) |>>=
   (KeyDatabase.mapT_ checkModDataEntry . map (uncurry keytuple2ModData))

--- Checks the consistency of the database for ModDescr entities.
checkModDescr :: KeyDatabase.Transaction ()
checkModDescr =
  KeyDatabase.getDB (KeyDatabase.allDBKeyInfos modDescrEntry) |>>=
   (KeyDatabase.mapT_ checkModDescrEntry . map (uncurry keytuple2ModDescr))

--- Checks the consistency of the database for ModInst entities.
checkModInst :: KeyDatabase.Transaction ()
checkModInst =
  KeyDatabase.getDB (KeyDatabase.allDBKeyInfos modInstEntry) |>>=
   (KeyDatabase.mapT_ checkModInstEntry . map (uncurry keytuple2ModInst))

--- Checks the consistency of the database for MasterProgram entities.
checkMasterProgram :: KeyDatabase.Transaction ()
checkMasterProgram =
  KeyDatabase.getDB (KeyDatabase.allDBKeyInfos masterProgramEntry) |>>=
   (KeyDatabase.mapT_ checkMasterProgramEntry .
    map (uncurry keytuple2MasterProgram))

--- Checks the consistency of the database for MasterProgInfo entities.
checkMasterProgInfo :: KeyDatabase.Transaction ()
checkMasterProgInfo =
  KeyDatabase.getDB (KeyDatabase.allDBKeyInfos masterProgInfoEntry) |>>=
   (KeyDatabase.mapT_ checkMasterProgInfoEntry .
    map (uncurry keytuple2MasterProgInfo))

--- Checks the consistency of the database for UnivisInfo entities.
checkUnivisInfo :: KeyDatabase.Transaction ()
checkUnivisInfo =
  KeyDatabase.getDB (KeyDatabase.allDBKeyInfos univisInfoEntry) |>>=
   (KeyDatabase.mapT_ checkUnivisInfoEntry .
    map (uncurry keytuple2UnivisInfo))

checkCategorizingEntry :: Categorizing -> KeyDatabase.Transaction ()
checkCategorizingEntry categorizing_p =
  ERDGeneric.existsEntryWithDBKey "ModData" modDataEntry
   (modDataKeyToKey (categorizingModDataCategorizingKey categorizing_p)) |>>
   (ERDGeneric.existsEntryWithDBKey "Category" categoryEntry
     (categoryKeyToKey (categorizingCategoryCategorizingKey categorizing_p))
    |>>
    ERDGeneric.unique2C categorizingEntry
     (modDataKeyToKey (categorizingModDataCategorizingKey categorizing_p))
     (categoryKeyToKey (categorizingCategoryCategorizingKey categorizing_p)))

checkStudyProgramEntry :: StudyProgram -> KeyDatabase.Transaction ()
checkStudyProgramEntry studyProgram_p =
  ERDGeneric.duplicateKeyTest studyProgramEntry |>>
   (ERDGeneric.uniqueC "MDB" studyProgramEntry keytuple2StudyProgram
     studyProgramShortName studyProgram_p |>>
    (ERDGeneric.uniqueC "MDB" studyProgramEntry keytuple2StudyProgram
      studyProgramProgKey studyProgram_p |>>
     ERDGeneric.uniqueC "MDB" studyProgramEntry keytuple2StudyProgram
      studyProgramURLKey studyProgram_p))

checkCategoryEntry :: Category -> KeyDatabase.Transaction ()
checkCategoryEntry category_p =
  ERDGeneric.duplicateKeyTest categoryEntry |>>
   (ERDGeneric.uniqueC "MDB" categoryEntry keytuple2Category categoryCatKey
     category_p |>>
    ERDGeneric.existsEntryWithDBKey "StudyProgram" studyProgramEntry
     (studyProgramKeyToKey
       (categoryStudyProgramProgramCategoriesKey category_p)))

checkMasterCoreAreaEntry :: MasterCoreArea -> KeyDatabase.Transaction ()
checkMasterCoreAreaEntry masterCoreArea_p =
  ERDGeneric.duplicateKeyTest masterCoreAreaEntry |>>
   ERDGeneric.uniqueC "MDB" masterCoreAreaEntry keytuple2MasterCoreArea
    masterCoreAreaAreaKey masterCoreArea_p

checkUserEntry :: User -> KeyDatabase.Transaction ()
checkUserEntry user_p =
  ERDGeneric.duplicateKeyTest userEntry |>>
   ERDGeneric.uniqueC "MDB" userEntry keytuple2User userLogin user_p

checkModDataEntry :: ModData -> KeyDatabase.Transaction ()
checkModDataEntry modData_p =
  ERDGeneric.duplicateKeyTest modDataEntry |>>
   (ERDGeneric.uniqueC "MDB" modDataEntry keytuple2ModData modDataCode
     modData_p |>>
    (ERDGeneric.existsEntryWithDBKey "User" userEntry
      (userKeyToKey (modDataUserResponsibleKey modData_p)) |>>
     ERDGeneric.minTestC "Categorizing" categorizingEntry
      keytuple2Categorizing categorizingModDataCategorizingKey 1
      (modDataKey modData_p)))

checkModDescrEntry :: ModDescr -> KeyDatabase.Transaction ()
checkModDescrEntry modDescr_p =
  ERDGeneric.duplicateKeyTest modDescrEntry |>>
   (ERDGeneric.uniqueC "MDB" modDescrEntry keytuple2ModDescr
     modDescrModDataDataDescKey modDescr_p |>>
    ERDGeneric.existsEntryWithDBKey "ModData" modDataEntry
     (modDataKeyToKey (modDescrModDataDataDescKey modDescr_p)))

checkModInstEntry :: ModInst -> KeyDatabase.Transaction ()
checkModInstEntry modInst_p =
  ERDGeneric.duplicateKeyTest modInstEntry |>>
   (ERDGeneric.existsEntryWithDBKey "User" userEntry
     (userKeyToKey (modInstUserLecturerModsKey modInst_p)) |>>
    ERDGeneric.existsEntryWithDBKey "ModData" modDataEntry
     (modDataKeyToKey (modInstModDataModuleInstancesKey modInst_p)))

checkMasterProgramEntry :: MasterProgram -> KeyDatabase.Transaction ()
checkMasterProgramEntry masterProgram_p =
  ERDGeneric.duplicateKeyTest masterProgramEntry |>>
   (ERDGeneric.existsEntryWithDBKey "User" userEntry
     (userKeyToKey (masterProgramUserAdvisingKey masterProgram_p)) |>>
    ERDGeneric.existsEntryWithDBKey "MasterCoreArea" masterCoreAreaEntry
     (masterCoreAreaKeyToKey
       (masterProgramMasterCoreAreaAreaProgramsKey masterProgram_p)))

checkMasterProgInfoEntry :: MasterProgInfo -> KeyDatabase.Transaction ()
checkMasterProgInfoEntry masterProgInfo_p =
  ERDGeneric.duplicateKeyTest masterProgInfoEntry |>>
   ERDGeneric.existsEntryWithDBKey "MasterProgram" masterProgramEntry
    (masterProgramKeyToKey
      (masterProgInfoMasterProgramProgramInfoKey masterProgInfo_p))

checkUnivisInfoEntry :: UnivisInfo -> KeyDatabase.Transaction ()
checkUnivisInfoEntry _ = ERDGeneric.duplicateKeyTest univisInfoEntry

--- Saves the complete database as Curry terms.
--- The first argument is the directory where the term files should be stored.
saveAllData :: String -> IO ()
saveAllData path =
  do ERDGeneric.saveDBTerms path "StudyProgram" studyProgramEntry
      keytuple2StudyProgram
     ERDGeneric.saveDBTerms path "Category" categoryEntry keytuple2Category
     ERDGeneric.saveDBTerms path "MasterCoreArea" masterCoreAreaEntry
      keytuple2MasterCoreArea
     ERDGeneric.saveDBTerms path "User" userEntry keytuple2User
     ERDGeneric.saveDBTerms path "ModData" modDataEntry keytuple2ModData
     ERDGeneric.saveDBTerms path "ModDescr" modDescrEntry keytuple2ModDescr
     ERDGeneric.saveDBTerms path "ModInst" modInstEntry keytuple2ModInst
     ERDGeneric.saveDBTerms path "MasterProgram" masterProgramEntry
      keytuple2MasterProgram
     ERDGeneric.saveDBTerms path "MasterProgInfo" masterProgInfoEntry
      keytuple2MasterProgInfo
     ERDGeneric.saveDBTerms path "UnivisInfo" univisInfoEntry
      keytuple2UnivisInfo
     ERDGeneric.saveDBTerms path "Categorizing" categorizingEntry
      keytuple2Categorizing

--- Restore the complete database from files containing Curry terms.
--- The first argument is the directory where the term files are stored.
restoreAllData :: String -> IO ()
restoreAllData path =
  do ERDGeneric.restoreDBTerms path "StudyProgram" studyProgramEntry
      (studyProgramKeyToKey . studyProgramKey) studyProgram2tuple
     ERDGeneric.restoreDBTerms path "Category" categoryEntry
      (categoryKeyToKey . categoryKey) category2tuple
     ERDGeneric.restoreDBTerms path "MasterCoreArea" masterCoreAreaEntry
      (masterCoreAreaKeyToKey . masterCoreAreaKey) masterCoreArea2tuple
     ERDGeneric.restoreDBTerms path "User" userEntry (userKeyToKey . userKey)
      user2tuple
     ERDGeneric.restoreDBTerms path "ModData" modDataEntry
      (modDataKeyToKey . modDataKey) modData2tuple
     ERDGeneric.restoreDBTerms path "ModDescr" modDescrEntry
      (modDescrKeyToKey . modDescrKey) modDescr2tuple
     ERDGeneric.restoreDBTerms path "ModInst" modInstEntry
      (modInstKeyToKey . modInstKey) modInst2tuple
     ERDGeneric.restoreDBTerms path "MasterProgram" masterProgramEntry
      (masterProgramKeyToKey . masterProgramKey) masterProgram2tuple
     ERDGeneric.restoreDBTerms path "MasterProgInfo" masterProgInfoEntry
      (masterProgInfoKeyToKey . masterProgInfoKey) masterProgInfo2tuple
     ERDGeneric.restoreDBTerms path "UnivisInfo" univisInfoEntry
      (univisInfoKeyToKey . univisInfoKey) univisInfo2tuple
     ERDGeneric.restoreDBRelTerms path "Categorizing" categorizingEntry
      categorizing2tuple
