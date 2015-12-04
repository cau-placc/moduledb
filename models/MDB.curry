module MDB
  ( StudyProgram, Category, MasterCoreArea, User, ModData, ModDescr, ModInst
  , AdvisorStudyProgram, AdvisorModule, MasterProgram, MasterProgInfo
  , UnivisInfo, StudyProgramKey, CategoryKey, MasterCoreAreaKey, UserKey
  , ModDataKey, ModDescrKey, ModInstKey, AdvisorStudyProgramKey
  , AdvisorModuleKey, MasterProgramKey, MasterProgInfoKey, UnivisInfoKey
  , Categorizing, studyProgramName, setStudyProgramName, studyProgramNameE
  , setStudyProgramNameE, studyProgramShortName, setStudyProgramShortName
  , studyProgramProgKey, setStudyProgramProgKey, studyProgramPosition
  , setStudyProgramPosition, categoryName, setCategoryName, categoryNameE
  , setCategoryNameE, categoryShortName, setCategoryShortName, categoryComment
  , setCategoryComment, categoryMinECTS, setCategoryMinECTS, categoryMaxECTS
  , setCategoryMaxECTS, categoryPosition, setCategoryPosition
  , categoryStudyProgramProgramCategoriesKey
  , setCategoryStudyProgramProgramCategoriesKey, masterCoreAreaName
  , setMasterCoreAreaName, masterCoreAreaShortName, setMasterCoreAreaShortName
  , masterCoreAreaDescription, setMasterCoreAreaDescription
  , masterCoreAreaAreaKey, setMasterCoreAreaAreaKey, masterCoreAreaPosition
  , setMasterCoreAreaPosition, userLogin, setUserLogin, userName, setUserName
  , userFirst, setUserFirst, userTitle, setUserTitle, userEmail, setUserEmail
  , userUrl, setUserUrl, userPassword, setUserPassword, userLastLogin
  , setUserLastLogin, modDataCode, setModDataCode, modDataNameG
  , setModDataNameG, modDataNameE, setModDataNameE, modDataCycle
  , setModDataCycle, modDataPresence, setModDataPresence, modDataECTS
  , setModDataECTS, modDataWorkload, setModDataWorkload, modDataLength
  , setModDataLength, modDataURL, setModDataURL, modDataVisible
  , setModDataVisible, modDataUserResponsibleKey, setModDataUserResponsibleKey
  , modDescrLanguage, setModDescrLanguage, modDescrShortDesc
  , setModDescrShortDesc, modDescrObjectives, setModDescrObjectives
  , modDescrContents, setModDescrContents, modDescrPrereq, setModDescrPrereq
  , modDescrExam, setModDescrExam, modDescrMethods, setModDescrMethods
  , modDescrUse, setModDescrUse, modDescrLiterature, setModDescrLiterature
  , modDescrLinks, setModDescrLinks, modDescrComments, setModDescrComments
  , modDescrModDataDataDescKey, setModDescrModDataDataDescKey, modInstTerm
  , setModInstTerm, modInstYear, setModInstYear, modInstUserLecturerModsKey
  , setModInstUserLecturerModsKey, modInstModDataModuleInstancesKey
  , setModInstModDataModuleInstancesKey, advisorStudyProgramName
  , setAdvisorStudyProgramName, advisorStudyProgramTerm
  , setAdvisorStudyProgramTerm, advisorStudyProgramYear
  , setAdvisorStudyProgramYear, advisorStudyProgramDesc
  , setAdvisorStudyProgramDesc, advisorStudyProgramPrereq
  , setAdvisorStudyProgramPrereq, advisorStudyProgramComments
  , setAdvisorStudyProgramComments, advisorStudyProgramVisible
  , setAdvisorStudyProgramVisible, advisorStudyProgramUserStudyAdvisingKey
  , setAdvisorStudyProgramUserStudyAdvisingKey
  , advisorStudyProgramStudyProgramStudyProgramsAdvisedKey
  , setAdvisorStudyProgramStudyProgramStudyProgramsAdvisedKey
  , advisorModuleMandatory, setAdvisorModuleMandatory
  , advisorModuleAdvisorStudyProgramAdvisorProgramModulesKey
  , setAdvisorModuleAdvisorStudyProgramAdvisorProgramModulesKey
  , advisorModuleCategoryAdvisorCategorizingKey
  , setAdvisorModuleCategoryAdvisorCategorizingKey
  , advisorModuleModInstAdvisedProgramModuleInstancesKey
  , setAdvisorModuleModInstAdvisedProgramModuleInstancesKey, masterProgramName
  , setMasterProgramName, masterProgramTerm, setMasterProgramTerm
  , masterProgramYear, setMasterProgramYear, masterProgramDesc
  , setMasterProgramDesc, masterProgramPrereq, setMasterProgramPrereq
  , masterProgramComments, setMasterProgramComments, masterProgramVisible
  , setMasterProgramVisible, masterProgramUserAdvisingKey
  , setMasterProgramUserAdvisingKey
  , masterProgramMasterCoreAreaAreaProgramsKey
  , setMasterProgramMasterCoreAreaAreaProgramsKey, masterProgInfoProgModules
  , setMasterProgInfoProgModules, masterProgInfoPraktikum
  , setMasterProgInfoPraktikum, masterProgInfoSeminar
  , setMasterProgInfoSeminar, masterProgInfoThesis, setMasterProgInfoThesis
  , masterProgInfoAllgGrundlagen, setMasterProgInfoAllgGrundlagen
  , masterProgInfoAnwendungsfach, setMasterProgInfoAnwendungsfach
  , masterProgInfoMasterProgramProgramInfoKey
  , setMasterProgInfoMasterProgramProgramInfoKey, univisInfoCode
  , setUnivisInfoCode, univisInfoTerm, setUnivisInfoTerm, univisInfoYear
  , setUnivisInfoYear, univisInfoURL, setUnivisInfoURL, studyProgram
  , studyProgramKey, showStudyProgramKey, readStudyProgramKey, newStudyProgram
  , updateStudyProgram, deleteStudyProgram, getStudyProgram
  , queryAllStudyPrograms, queryCondStudyProgram, category, categoryKey
  , showCategoryKey, readCategoryKey
  , newCategoryWithStudyProgramProgramCategoriesKey, updateCategory
  , deleteCategory, getCategory, queryAllCategorys, queryCondCategory
  , masterCoreArea, masterCoreAreaKey, showMasterCoreAreaKey
  , readMasterCoreAreaKey, newMasterCoreArea, updateMasterCoreArea
  , deleteMasterCoreArea, getMasterCoreArea, queryAllMasterCoreAreas
  , queryCondMasterCoreArea, user, userKey, showUserKey, readUserKey, newUser
  , updateUser, deleteUser, getUser, queryAllUsers, queryCondUser, modData
  , modDataKey, showModDataKey, readModDataKey
  , newModDataWithUserResponsibleKey, updateModData, deleteModData, getModData
  , queryAllModDatas, queryCondModData, modDescr, modDescrKey, showModDescrKey
  , readModDescrKey, newModDescrWithModDataDataDescKey, updateModDescr
  , deleteModDescr, getModDescr, queryAllModDescrs, queryCondModDescr, modInst
  , modInstKey, showModInstKey, readModInstKey
  , newModInstWithUserLecturerModsKeyWithModDataModuleInstancesKey
  , updateModInst, deleteModInst, getModInst, queryAllModInsts
  , queryCondModInst, advisorStudyProgram, advisorStudyProgramKey
  , showAdvisorStudyProgramKey, readAdvisorStudyProgramKey
  , newAdvisorStudyProgramWithUserStudyAdvisingKeyWithStudyProgramStudyProgramsAdvisedKey
  , updateAdvisorStudyProgram, deleteAdvisorStudyProgram
  , getAdvisorStudyProgram, queryAllAdvisorStudyPrograms
  , queryCondAdvisorStudyProgram, advisorModule, advisorModuleKey
  , showAdvisorModuleKey, readAdvisorModuleKey
  , newAdvisorModuleWithAdvisorStudyProgramAdvisorProgramModulesKeyWithCategoryAdvisorCategorizingKeyWithModInstAdvisedProgramModuleInstancesKey
  , updateAdvisorModule, deleteAdvisorModule, getAdvisorModule
  , queryAllAdvisorModules, queryCondAdvisorModule, masterProgram
  , masterProgramKey, showMasterProgramKey, readMasterProgramKey
  , newMasterProgramWithUserAdvisingKeyWithMasterCoreAreaAreaProgramsKey
  , updateMasterProgram, deleteMasterProgram, getMasterProgram
  , queryAllMasterPrograms, queryCondMasterProgram, masterProgInfo
  , masterProgInfoKey, showMasterProgInfoKey, readMasterProgInfoKey
  , newMasterProgInfoWithMasterProgramProgramInfoKey, updateMasterProgInfo
  , deleteMasterProgInfo, getMasterProgInfo, queryAllMasterProgInfos
  , queryCondMasterProgInfo, univisInfo, univisInfoKey, showUnivisInfoKey
  , readUnivisInfoKey, newUnivisInfo, updateUnivisInfo, deleteUnivisInfo
  , getUnivisInfo, queryAllUnivisInfos, queryCondUnivisInfo
  , categorizingModDataCategorizingKey, categorizingCategoryCategorizingKey
  , categorizing, newCategorizing, deleteCategorizing, getModDataCategorys
  , queryAllCategorizings, queryCondCategorizing, programInfo, withProgInfo
  , programInfoOf, areaPrograms, withProgram, ofCoreArea, advising, organizes
  , organizedBy, advisedProgramModuleInstances, advisorUseofModInst
  , withModInst, advisorCategorizing, containsAdvisorMods, advisedBelongsTo
  , advisorProgramModules, moduleOfAdvisorProgram, belongsToAdvisedProgram
  , studyProgramsAdvised, advisedProgram, instanceOf, studyAdvising
  , advisesProgram, advisedBy, moduleInstances, instOfModule, withModule
  , lecturerMods, instOfLecturer, withLecturer, dataDesc, withDesc, descOf
  , belongsTo, contains, responsible, responsibleFor, managedBy
  , programCategories, withCategory, ofProgram, checkAllData
  , checkCategorizing, checkStudyProgram, checkCategory, checkMasterCoreArea
  , checkUser, checkModData, checkModDescr, checkModInst
  , checkAdvisorStudyProgram, checkAdvisorModule, checkMasterProgram
  , checkMasterProgInfo, checkUnivisInfo, saveAllData, restoreAllData
  , modInstSemester
  , queryModDataOfUser, queryModDataWithCode, queryModDataCodeName
  , queryInstancesOfMod, getMasterProgramKeysOfModInst
  , getModDataCategories, destroyCategorizing, queryDescriptionOfMod
  , queryModDataKeysOfCategory, getModDataKeyCategorys
  , queryHasUnivisEntry, masterProgramKeyToString
  , queryMasterProgramOfUser, queryInfoOfMasterProgram
  , queryMasterProgramMainInfos
  , queryModKeysOfSem, queryExamOfMod, queryUnivisURL
  , readTermDB, storeTermDB
  ) where

import ConfigMDB
import ERDGeneric
import KeyDatabase
import KeyDatabaseQuery
import ReadShowTerm
import Time

data StudyProgram = StudyProgram Key String String String String Int

type StudyProgramTuple = (String,String,String,String,Int)

data Category = Category Key String String String String Int Int Int Key

type CategoryTuple = (String,String,String,String,Int,Int,Int,Key)

data MasterCoreArea = MasterCoreArea Key String String String String Int

type MasterCoreAreaTuple = (String,String,String,String,Int)

data User = User Key String String String String String String String CalendarTime

type UserTuple = (String
                 ,String
                 ,String
                 ,String
                 ,String
                 ,String
                 ,String
                 ,CalendarTime)

data ModData = ModData Key String String String String String Int String Int String Bool Key

type ModDataTuple = (String
                    ,String
                    ,String
                    ,String
                    ,String
                    ,Int
                    ,String
                    ,Int
                    ,String
                    ,Bool
                    ,Key)

data ModDescr = ModDescr Key String String String String String String String String String String String Key

type ModDescrTuple = (String
                     ,String
                     ,String
                     ,String
                     ,String
                     ,String
                     ,String
                     ,String
                     ,String
                     ,String
                     ,String
                     ,Key)

data ModInst = ModInst Key String Int Key Key

type ModInstTuple = (String,Int,Key,Key)

data AdvisorStudyProgram = AdvisorStudyProgram Key String String Int String String String Bool Key Key

type AdvisorStudyProgramTuple = (String
                                ,String
                                ,Int
                                ,String
                                ,String
                                ,String
                                ,Bool
                                ,Key
                                ,Key)

data AdvisorModule = AdvisorModule Key Bool Key Key Key

type AdvisorModuleTuple = (Bool,Key,Key,Key)

data MasterProgram = MasterProgram Key String String Int String String String Bool Key Key

type MasterProgramTuple = (String
                          ,String
                          ,Int
                          ,String
                          ,String
                          ,String
                          ,Bool
                          ,Key
                          ,Key)

data MasterProgInfo = MasterProgInfo Key String String String String String String Key

type MasterProgInfoTuple = (String,String,String,String,String,String,Key)

data UnivisInfo = UnivisInfo Key String String Int String

type UnivisInfoTuple = (String,String,Int,String)

data StudyProgramKey = StudyProgramKey Key

data CategoryKey = CategoryKey Key

data MasterCoreAreaKey = MasterCoreAreaKey Key

data UserKey = UserKey Key

data ModDataKey = ModDataKey Key

data ModDescrKey = ModDescrKey Key

data ModInstKey = ModInstKey Key

data AdvisorStudyProgramKey = AdvisorStudyProgramKey Key

data AdvisorModuleKey = AdvisorModuleKey Key

data MasterProgramKey = MasterProgramKey Key

data MasterProgInfoKey = MasterProgInfoKey Key

data UnivisInfoKey = UnivisInfoKey Key

data Categorizing = Categorizing Key Key

type CategorizingTuple = (Key,Key)

dbFile :: String
dbFile = storageDir++"MDB.db"

--- Transforms entity StudyProgram into tuple representation.
studyProgram2tuple :: StudyProgram -> StudyProgramTuple
studyProgram2tuple (StudyProgram _ x2 x3 x4 x5 x6) = (x2,x3,x4,x5,x6)

--- Transforms key and tuple into a StudyProgram entity.
keytuple2StudyProgram :: Key -> StudyProgramTuple -> StudyProgram
keytuple2StudyProgram x1 (x2,x3,x4,x5,x6) = StudyProgram x1 x2 x3 x4 x5 x6

--- Transforms entity Category into tuple representation.
category2tuple :: Category -> CategoryTuple
category2tuple (Category _ x2 x3 x4 x5 x6 x7 x8 x9) =
  (x2,x3,x4,x5,x6,x7,x8,x9)

--- Transforms key and tuple into a Category entity.
keytuple2Category :: Key -> CategoryTuple -> Category
keytuple2Category x1 (x2,x3,x4,x5,x6,x7,x8,x9) =
  Category x1 x2 x3 x4 x5 x6 x7 x8 x9

--- Transforms entity MasterCoreArea into tuple representation.
masterCoreArea2tuple :: MasterCoreArea -> MasterCoreAreaTuple
masterCoreArea2tuple (MasterCoreArea _ x2 x3 x4 x5 x6) = (x2,x3,x4,x5,x6)

--- Transforms key and tuple into a MasterCoreArea entity.
keytuple2MasterCoreArea :: Key -> MasterCoreAreaTuple -> MasterCoreArea
keytuple2MasterCoreArea x1 (x2,x3,x4,x5,x6) = MasterCoreArea x1 x2 x3 x4 x5 x6

--- Transforms entity User into tuple representation.
user2tuple :: User -> UserTuple
user2tuple (User _ x2 x3 x4 x5 x6 x7 x8 x9) = (x2,x3,x4,x5,x6,x7,x8,x9)

--- Transforms key and tuple into a User entity.
keytuple2User :: Key -> UserTuple -> User
keytuple2User x1 (x2,x3,x4,x5,x6,x7,x8,x9) = User x1 x2 x3 x4 x5 x6 x7 x8 x9

--- Transforms entity ModData into tuple representation.
modData2tuple :: ModData -> ModDataTuple
modData2tuple (ModData _ x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) =
  (x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12)

--- Transforms key and tuple into a ModData entity.
keytuple2ModData :: Key -> ModDataTuple -> ModData
keytuple2ModData x1 (x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12) =
  ModData x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12

--- Transforms entity ModDescr into tuple representation.
modDescr2tuple :: ModDescr -> ModDescrTuple
modDescr2tuple (ModDescr _ x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) =
  (x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13)

--- Transforms key and tuple into a ModDescr entity.
keytuple2ModDescr :: Key -> ModDescrTuple -> ModDescr
keytuple2ModDescr x1 (x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13) =
  ModDescr x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13

--- Transforms entity ModInst into tuple representation.
modInst2tuple :: ModInst -> ModInstTuple
modInst2tuple (ModInst _ x2 x3 x4 x5) = (x2,x3,x4,x5)

--- Transforms key and tuple into a ModInst entity.
keytuple2ModInst :: Key -> ModInstTuple -> ModInst
keytuple2ModInst x1 (x2,x3,x4,x5) = ModInst x1 x2 x3 x4 x5

--- Transforms entity AdvisorStudyProgram into tuple representation.
advisorStudyProgram2tuple :: AdvisorStudyProgram -> AdvisorStudyProgramTuple
advisorStudyProgram2tuple
    (AdvisorStudyProgram _ x2 x3 x4 x5 x6 x7 x8 x9 x10) =
  (x2,x3,x4,x5,x6,x7,x8,x9,x10)

--- Transforms key and tuple into a AdvisorStudyProgram entity.
keytuple2AdvisorStudyProgram
  :: Key -> AdvisorStudyProgramTuple -> AdvisorStudyProgram
keytuple2AdvisorStudyProgram x1 (x2,x3,x4,x5,x6,x7,x8,x9,x10) =
  AdvisorStudyProgram x1 x2 x3 x4 x5 x6 x7 x8 x9 x10

--- Transforms entity AdvisorModule into tuple representation.
advisorModule2tuple :: AdvisorModule -> AdvisorModuleTuple
advisorModule2tuple (AdvisorModule _ x2 x3 x4 x5) = (x2,x3,x4,x5)

--- Transforms key and tuple into a AdvisorModule entity.
keytuple2AdvisorModule :: Key -> AdvisorModuleTuple -> AdvisorModule
keytuple2AdvisorModule x1 (x2,x3,x4,x5) = AdvisorModule x1 x2 x3 x4 x5

--- Transforms entity MasterProgram into tuple representation.
masterProgram2tuple :: MasterProgram -> MasterProgramTuple
masterProgram2tuple (MasterProgram _ x2 x3 x4 x5 x6 x7 x8 x9 x10) =
  (x2,x3,x4,x5,x6,x7,x8,x9,x10)

--- Transforms key and tuple into a MasterProgram entity.
keytuple2MasterProgram :: Key -> MasterProgramTuple -> MasterProgram
keytuple2MasterProgram x1 (x2,x3,x4,x5,x6,x7,x8,x9,x10) =
  MasterProgram x1 x2 x3 x4 x5 x6 x7 x8 x9 x10

--- Transforms entity MasterProgInfo into tuple representation.
masterProgInfo2tuple :: MasterProgInfo -> MasterProgInfoTuple
masterProgInfo2tuple (MasterProgInfo _ x2 x3 x4 x5 x6 x7 x8) =
  (x2,x3,x4,x5,x6,x7,x8)

--- Transforms key and tuple into a MasterProgInfo entity.
keytuple2MasterProgInfo :: Key -> MasterProgInfoTuple -> MasterProgInfo
keytuple2MasterProgInfo x1 (x2,x3,x4,x5,x6,x7,x8) =
  MasterProgInfo x1 x2 x3 x4 x5 x6 x7 x8

--- Transforms entity UnivisInfo into tuple representation.
univisInfo2tuple :: UnivisInfo -> UnivisInfoTuple
univisInfo2tuple (UnivisInfo _ x2 x3 x4 x5) = (x2,x3,x4,x5)

--- Transforms key and tuple into a UnivisInfo entity.
keytuple2UnivisInfo :: Key -> UnivisInfoTuple -> UnivisInfo
keytuple2UnivisInfo x1 (x2,x3,x4,x5) = UnivisInfo x1 x2 x3 x4 x5

--- Transforms relationship entity Categorizing into tuple representation.
categorizing2tuple :: Categorizing -> CategorizingTuple
categorizing2tuple (Categorizing x1 x2) = (x1,x2)

--- Transforms key and tuple into a Categorizing relationship entity.
keytuple2Categorizing :: Key -> CategorizingTuple -> Categorizing
keytuple2Categorizing _ (x1,x2) = Categorizing x1 x2

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
setStudyProgramKey :: StudyProgram -> Key -> StudyProgram
setStudyProgramKey (StudyProgram _ x2 x3 x4 x5 x6) x =
  StudyProgram x x2 x3 x4 x5 x6

--- Gets the value of attribute "Name" of a StudyProgram entity.
studyProgramName :: StudyProgram -> String
studyProgramName (StudyProgram _ x _ _ _ _) = x

--- Sets the value of attribute "Name" in a StudyProgram entity.
setStudyProgramName :: StudyProgram -> String -> StudyProgram
setStudyProgramName (StudyProgram x1 _ x3 x4 x5 x6) x =
  StudyProgram x1 x x3 x4 x5 x6

--- Gets the value of attribute "NameE" of a StudyProgram entity.
studyProgramNameE :: StudyProgram -> String
studyProgramNameE (StudyProgram _ _ x _ _ _) = x

--- Sets the value of attribute "NameE" in a StudyProgram entity.
setStudyProgramNameE :: StudyProgram -> String -> StudyProgram
setStudyProgramNameE (StudyProgram x1 x2 _ x4 x5 x6) x =
  StudyProgram x1 x2 x x4 x5 x6

--- Gets the value of attribute "ShortName" of a StudyProgram entity.
studyProgramShortName :: StudyProgram -> String
studyProgramShortName (StudyProgram _ _ _ x _ _) = x

--- Sets the value of attribute "ShortName" in a StudyProgram entity.
setStudyProgramShortName :: StudyProgram -> String -> StudyProgram
setStudyProgramShortName (StudyProgram x1 x2 x3 _ x5 x6) x =
  StudyProgram x1 x2 x3 x x5 x6

--- Gets the value of attribute "ProgKey" of a StudyProgram entity.
studyProgramProgKey :: StudyProgram -> String
studyProgramProgKey (StudyProgram _ _ _ _ x _) = x

--- Sets the value of attribute "ProgKey" in a StudyProgram entity.
setStudyProgramProgKey :: StudyProgram -> String -> StudyProgram
setStudyProgramProgKey (StudyProgram x1 x2 x3 x4 _ x6) x =
  StudyProgram x1 x2 x3 x4 x x6

--- Gets the value of attribute "Position" of a StudyProgram entity.
studyProgramPosition :: StudyProgram -> Int
studyProgramPosition (StudyProgram _ _ _ _ _ x) = x

--- Sets the value of attribute "Position" in a StudyProgram entity.
setStudyProgramPosition :: StudyProgram -> Int -> StudyProgram
setStudyProgramPosition (StudyProgram x1 x2 x3 x4 x5 _) x =
  StudyProgram x1 x2 x3 x4 x5 x

--- Sets the value of attribute "Key" in a Category entity.
setCategoryKey :: Category -> Key -> Category
setCategoryKey (Category _ x2 x3 x4 x5 x6 x7 x8 x9) x =
  Category x x2 x3 x4 x5 x6 x7 x8 x9

--- Gets the value of attribute "Name" of a Category entity.
categoryName :: Category -> String
categoryName (Category _ x _ _ _ _ _ _ _) = x

--- Sets the value of attribute "Name" in a Category entity.
setCategoryName :: Category -> String -> Category
setCategoryName (Category x1 _ x3 x4 x5 x6 x7 x8 x9) x =
  Category x1 x x3 x4 x5 x6 x7 x8 x9

--- Gets the value of attribute "NameE" of a Category entity.
categoryNameE :: Category -> String
categoryNameE (Category _ _ x _ _ _ _ _ _) = x

--- Sets the value of attribute "NameE" in a Category entity.
setCategoryNameE :: Category -> String -> Category
setCategoryNameE (Category x1 x2 _ x4 x5 x6 x7 x8 x9) x =
  Category x1 x2 x x4 x5 x6 x7 x8 x9

--- Gets the value of attribute "ShortName" of a Category entity.
categoryShortName :: Category -> String
categoryShortName (Category _ _ _ x _ _ _ _ _) = x

--- Sets the value of attribute "ShortName" in a Category entity.
setCategoryShortName :: Category -> String -> Category
setCategoryShortName (Category x1 x2 x3 _ x5 x6 x7 x8 x9) x =
  Category x1 x2 x3 x x5 x6 x7 x8 x9

--- Gets the value of attribute "Comment" of a Category entity.
categoryComment :: Category -> String
categoryComment (Category _ _ _ _ x _ _ _ _) = x

--- Sets the value of attribute "Comment" in a Category entity.
setCategoryComment :: Category -> String -> Category
setCategoryComment (Category x1 x2 x3 x4 _ x6 x7 x8 x9) x =
  Category x1 x2 x3 x4 x x6 x7 x8 x9

--- Gets the value of attribute "MinECTS" of a Category entity.
categoryMinECTS :: Category -> Int
categoryMinECTS (Category _ _ _ _ _ x _ _ _) = x

--- Sets the value of attribute "MinECTS" in a Category entity.
setCategoryMinECTS :: Category -> Int -> Category
setCategoryMinECTS (Category x1 x2 x3 x4 x5 _ x7 x8 x9) x =
  Category x1 x2 x3 x4 x5 x x7 x8 x9

--- Gets the value of attribute "MaxECTS" of a Category entity.
categoryMaxECTS :: Category -> Int
categoryMaxECTS (Category _ _ _ _ _ _ x _ _) = x

--- Sets the value of attribute "MaxECTS" in a Category entity.
setCategoryMaxECTS :: Category -> Int -> Category
setCategoryMaxECTS (Category x1 x2 x3 x4 x5 x6 _ x8 x9) x =
  Category x1 x2 x3 x4 x5 x6 x x8 x9

--- Gets the value of attribute "Position" of a Category entity.
categoryPosition :: Category -> Int
categoryPosition (Category _ _ _ _ _ _ _ x _) = x

--- Sets the value of attribute "Position" in a Category entity.
setCategoryPosition :: Category -> Int -> Category
setCategoryPosition (Category x1 x2 x3 x4 x5 x6 x7 _ x9) x =
  Category x1 x2 x3 x4 x5 x6 x7 x x9

--- Gets the value of attribute "StudyProgramProgramCategoriesKey" of a Category entity.
categoryStudyProgramProgramCategoriesKey :: Category -> StudyProgramKey
categoryStudyProgramProgramCategoriesKey (Category _ _ _ _ _ _ _ _ x) =
  StudyProgramKey x

--- Sets the value of attribute "StudyProgramProgramCategoriesKey" in a Category entity.
setCategoryStudyProgramProgramCategoriesKey
  :: Category -> StudyProgramKey -> Category
setCategoryStudyProgramProgramCategoriesKey
    (Category x1 x2 x3 x4 x5 x6 x7 x8 _) x =
  Category x1 x2 x3 x4 x5 x6 x7 x8 (studyProgramKeyToKey x)

--- Sets the value of attribute "Key" in a MasterCoreArea entity.
setMasterCoreAreaKey :: MasterCoreArea -> Key -> MasterCoreArea
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
setUserKey :: User -> Key -> User
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
userLastLogin :: User -> CalendarTime
userLastLogin (User _ _ _ _ _ _ _ _ x) = x

--- Sets the value of attribute "LastLogin" in a User entity.
setUserLastLogin :: User -> CalendarTime -> User
setUserLastLogin (User x1 x2 x3 x4 x5 x6 x7 x8 _) x =
  User x1 x2 x3 x4 x5 x6 x7 x8 x

--- Sets the value of attribute "Key" in a ModData entity.
setModDataKey :: ModData -> Key -> ModData
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
setModDataUserResponsibleKey
    (ModData x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 _) x =
  ModData x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 (userKeyToKey x)

--- Sets the value of attribute "Key" in a ModDescr entity.
setModDescrKey :: ModDescr -> Key -> ModDescr
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
setModDescrModDataDataDescKey
    (ModDescr x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 _) x =
  ModDescr x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 (modDataKeyToKey x)

--- Sets the value of attribute "Key" in a ModInst entity.
setModInstKey :: ModInst -> Key -> ModInst
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

--- Sets the value of attribute "Key" in a AdvisorStudyProgram entity.
setAdvisorStudyProgramKey :: AdvisorStudyProgram -> Key -> AdvisorStudyProgram
setAdvisorStudyProgramKey
    (AdvisorStudyProgram _ x2 x3 x4 x5 x6 x7 x8 x9 x10) x =
  AdvisorStudyProgram x x2 x3 x4 x5 x6 x7 x8 x9 x10

--- Gets the value of attribute "Name" of a AdvisorStudyProgram entity.
advisorStudyProgramName :: AdvisorStudyProgram -> String
advisorStudyProgramName (AdvisorStudyProgram _ x _ _ _ _ _ _ _ _) = x

--- Sets the value of attribute "Name" in a AdvisorStudyProgram entity.
setAdvisorStudyProgramName
  :: AdvisorStudyProgram -> String -> AdvisorStudyProgram
setAdvisorStudyProgramName
    (AdvisorStudyProgram x1 _ x3 x4 x5 x6 x7 x8 x9 x10) x =
  AdvisorStudyProgram x1 x x3 x4 x5 x6 x7 x8 x9 x10

--- Gets the value of attribute "Term" of a AdvisorStudyProgram entity.
advisorStudyProgramTerm :: AdvisorStudyProgram -> String
advisorStudyProgramTerm (AdvisorStudyProgram _ _ x _ _ _ _ _ _ _) = x

--- Sets the value of attribute "Term" in a AdvisorStudyProgram entity.
setAdvisorStudyProgramTerm
  :: AdvisorStudyProgram -> String -> AdvisorStudyProgram
setAdvisorStudyProgramTerm
    (AdvisorStudyProgram x1 x2 _ x4 x5 x6 x7 x8 x9 x10) x =
  AdvisorStudyProgram x1 x2 x x4 x5 x6 x7 x8 x9 x10

--- Gets the value of attribute "Year" of a AdvisorStudyProgram entity.
advisorStudyProgramYear :: AdvisorStudyProgram -> Int
advisorStudyProgramYear (AdvisorStudyProgram _ _ _ x _ _ _ _ _ _) = x

--- Sets the value of attribute "Year" in a AdvisorStudyProgram entity.
setAdvisorStudyProgramYear
  :: AdvisorStudyProgram -> Int -> AdvisorStudyProgram
setAdvisorStudyProgramYear
    (AdvisorStudyProgram x1 x2 x3 _ x5 x6 x7 x8 x9 x10) x =
  AdvisorStudyProgram x1 x2 x3 x x5 x6 x7 x8 x9 x10

--- Gets the value of attribute "Desc" of a AdvisorStudyProgram entity.
advisorStudyProgramDesc :: AdvisorStudyProgram -> String
advisorStudyProgramDesc (AdvisorStudyProgram _ _ _ _ x _ _ _ _ _) = x

--- Sets the value of attribute "Desc" in a AdvisorStudyProgram entity.
setAdvisorStudyProgramDesc
  :: AdvisorStudyProgram -> String -> AdvisorStudyProgram
setAdvisorStudyProgramDesc
    (AdvisorStudyProgram x1 x2 x3 x4 _ x6 x7 x8 x9 x10) x =
  AdvisorStudyProgram x1 x2 x3 x4 x x6 x7 x8 x9 x10

--- Gets the value of attribute "Prereq" of a AdvisorStudyProgram entity.
advisorStudyProgramPrereq :: AdvisorStudyProgram -> String
advisorStudyProgramPrereq (AdvisorStudyProgram _ _ _ _ _ x _ _ _ _) = x

--- Sets the value of attribute "Prereq" in a AdvisorStudyProgram entity.
setAdvisorStudyProgramPrereq
  :: AdvisorStudyProgram -> String -> AdvisorStudyProgram
setAdvisorStudyProgramPrereq
    (AdvisorStudyProgram x1 x2 x3 x4 x5 _ x7 x8 x9 x10) x =
  AdvisorStudyProgram x1 x2 x3 x4 x5 x x7 x8 x9 x10

--- Gets the value of attribute "Comments" of a AdvisorStudyProgram entity.
advisorStudyProgramComments :: AdvisorStudyProgram -> String
advisorStudyProgramComments (AdvisorStudyProgram _ _ _ _ _ _ x _ _ _) = x

--- Sets the value of attribute "Comments" in a AdvisorStudyProgram entity.
setAdvisorStudyProgramComments
  :: AdvisorStudyProgram -> String -> AdvisorStudyProgram
setAdvisorStudyProgramComments
    (AdvisorStudyProgram x1 x2 x3 x4 x5 x6 _ x8 x9 x10) x =
  AdvisorStudyProgram x1 x2 x3 x4 x5 x6 x x8 x9 x10

--- Gets the value of attribute "Visible" of a AdvisorStudyProgram entity.
advisorStudyProgramVisible :: AdvisorStudyProgram -> Bool
advisorStudyProgramVisible (AdvisorStudyProgram _ _ _ _ _ _ _ x _ _) = x

--- Sets the value of attribute "Visible" in a AdvisorStudyProgram entity.
setAdvisorStudyProgramVisible
  :: AdvisorStudyProgram -> Bool -> AdvisorStudyProgram
setAdvisorStudyProgramVisible
    (AdvisorStudyProgram x1 x2 x3 x4 x5 x6 x7 _ x9 x10) x =
  AdvisorStudyProgram x1 x2 x3 x4 x5 x6 x7 x x9 x10

--- Gets the value of attribute "UserStudyAdvisingKey" of a AdvisorStudyProgram entity.
advisorStudyProgramUserStudyAdvisingKey :: AdvisorStudyProgram -> UserKey
advisorStudyProgramUserStudyAdvisingKey
    (AdvisorStudyProgram _ _ _ _ _ _ _ _ x _) =
  UserKey x

--- Sets the value of attribute "UserStudyAdvisingKey" in a AdvisorStudyProgram entity.
setAdvisorStudyProgramUserStudyAdvisingKey
  :: AdvisorStudyProgram -> UserKey -> AdvisorStudyProgram
setAdvisorStudyProgramUserStudyAdvisingKey
    (AdvisorStudyProgram x1 x2 x3 x4 x5 x6 x7 x8 _ x10) x =
  AdvisorStudyProgram x1 x2 x3 x4 x5 x6 x7 x8 (userKeyToKey x) x10

--- Gets the value of attribute "StudyProgramStudyProgramsAdvisedKey" of a AdvisorStudyProgram entity.
advisorStudyProgramStudyProgramStudyProgramsAdvisedKey
  :: AdvisorStudyProgram -> StudyProgramKey
advisorStudyProgramStudyProgramStudyProgramsAdvisedKey
    (AdvisorStudyProgram _ _ _ _ _ _ _ _ _ x) =
  StudyProgramKey x

--- Sets the value of attribute "StudyProgramStudyProgramsAdvisedKey" in a AdvisorStudyProgram entity.
setAdvisorStudyProgramStudyProgramStudyProgramsAdvisedKey
  :: AdvisorStudyProgram -> StudyProgramKey -> AdvisorStudyProgram
setAdvisorStudyProgramStudyProgramStudyProgramsAdvisedKey
    (AdvisorStudyProgram x1 x2 x3 x4 x5 x6 x7 x8 x9 _) x =
  AdvisorStudyProgram x1 x2 x3 x4 x5 x6 x7 x8 x9 (studyProgramKeyToKey x)

--- Sets the value of attribute "Key" in a AdvisorModule entity.
setAdvisorModuleKey :: AdvisorModule -> Key -> AdvisorModule
setAdvisorModuleKey (AdvisorModule _ x2 x3 x4 x5) x =
  AdvisorModule x x2 x3 x4 x5

--- Gets the value of attribute "Mandatory" of a AdvisorModule entity.
advisorModuleMandatory :: AdvisorModule -> Bool
advisorModuleMandatory (AdvisorModule _ x _ _ _) = x

--- Sets the value of attribute "Mandatory" in a AdvisorModule entity.
setAdvisorModuleMandatory :: AdvisorModule -> Bool -> AdvisorModule
setAdvisorModuleMandatory (AdvisorModule x1 _ x3 x4 x5) x =
  AdvisorModule x1 x x3 x4 x5

--- Gets the value of attribute "AdvisorStudyProgramAdvisorProgramModulesKey" of a AdvisorModule entity.
advisorModuleAdvisorStudyProgramAdvisorProgramModulesKey
  :: AdvisorModule -> AdvisorStudyProgramKey
advisorModuleAdvisorStudyProgramAdvisorProgramModulesKey
    (AdvisorModule _ _ x _ _) =
  AdvisorStudyProgramKey x

--- Sets the value of attribute "AdvisorStudyProgramAdvisorProgramModulesKey" in a AdvisorModule entity.
setAdvisorModuleAdvisorStudyProgramAdvisorProgramModulesKey
  :: AdvisorModule -> AdvisorStudyProgramKey -> AdvisorModule
setAdvisorModuleAdvisorStudyProgramAdvisorProgramModulesKey
    (AdvisorModule x1 x2 _ x4 x5) x =
  AdvisorModule x1 x2 (advisorStudyProgramKeyToKey x) x4 x5

--- Gets the value of attribute "CategoryAdvisorCategorizingKey" of a AdvisorModule entity.
advisorModuleCategoryAdvisorCategorizingKey :: AdvisorModule -> CategoryKey
advisorModuleCategoryAdvisorCategorizingKey (AdvisorModule _ _ _ x _) =
  CategoryKey x

--- Sets the value of attribute "CategoryAdvisorCategorizingKey" in a AdvisorModule entity.
setAdvisorModuleCategoryAdvisorCategorizingKey
  :: AdvisorModule -> CategoryKey -> AdvisorModule
setAdvisorModuleCategoryAdvisorCategorizingKey
    (AdvisorModule x1 x2 x3 _ x5) x =
  AdvisorModule x1 x2 x3 (categoryKeyToKey x) x5

--- Gets the value of attribute "ModInstAdvisedProgramModuleInstancesKey" of a AdvisorModule entity.
advisorModuleModInstAdvisedProgramModuleInstancesKey
  :: AdvisorModule -> ModInstKey
advisorModuleModInstAdvisedProgramModuleInstancesKey
    (AdvisorModule _ _ _ _ x) =
  ModInstKey x

--- Sets the value of attribute "ModInstAdvisedProgramModuleInstancesKey" in a AdvisorModule entity.
setAdvisorModuleModInstAdvisedProgramModuleInstancesKey
  :: AdvisorModule -> ModInstKey -> AdvisorModule
setAdvisorModuleModInstAdvisedProgramModuleInstancesKey
    (AdvisorModule x1 x2 x3 x4 _) x =
  AdvisorModule x1 x2 x3 x4 (modInstKeyToKey x)

--- Sets the value of attribute "Key" in a MasterProgram entity.
setMasterProgramKey :: MasterProgram -> Key -> MasterProgram
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
setMasterProgramUserAdvisingKey
    (MasterProgram x1 x2 x3 x4 x5 x6 x7 x8 _ x10) x =
  MasterProgram x1 x2 x3 x4 x5 x6 x7 x8 (userKeyToKey x) x10

--- Gets the value of attribute "MasterCoreAreaAreaProgramsKey" of a MasterProgram entity.
masterProgramMasterCoreAreaAreaProgramsKey
  :: MasterProgram -> MasterCoreAreaKey
masterProgramMasterCoreAreaAreaProgramsKey
    (MasterProgram _ _ _ _ _ _ _ _ _ x) =
  MasterCoreAreaKey x

--- Sets the value of attribute "MasterCoreAreaAreaProgramsKey" in a MasterProgram entity.
setMasterProgramMasterCoreAreaAreaProgramsKey
  :: MasterProgram -> MasterCoreAreaKey -> MasterProgram
setMasterProgramMasterCoreAreaAreaProgramsKey
    (MasterProgram x1 x2 x3 x4 x5 x6 x7 x8 x9 _) x =
  MasterProgram x1 x2 x3 x4 x5 x6 x7 x8 x9 (masterCoreAreaKeyToKey x)

--- Sets the value of attribute "Key" in a MasterProgInfo entity.
setMasterProgInfoKey :: MasterProgInfo -> Key -> MasterProgInfo
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
setMasterProgInfoMasterProgramProgramInfoKey
    (MasterProgInfo x1 x2 x3 x4 x5 x6 x7 _) x =
  MasterProgInfo x1 x2 x3 x4 x5 x6 x7 (masterProgramKeyToKey x)

--- Sets the value of attribute "Key" in a UnivisInfo entity.
setUnivisInfoKey :: UnivisInfo -> Key -> UnivisInfo
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
studyProgramEntry :: Key -> StudyProgramTuple -> Dynamic
studyProgramEntry =
  persistentSQLite dbFile "StudyProgram"
   ["Name","NameE","ShortName","ProgKey","Position"]

--- Dynamic predicate representing the relation
--- between keys and StudyProgram entities.
studyProgram :: StudyProgramKey -> StudyProgram -> Dynamic
studyProgram key obj
  | key =:= studyProgramKey obj
  = studyProgramEntry (studyProgramKeyToKey key) (studyProgram2tuple obj)

--- Gets the key of a StudyProgram entity.
studyProgramKey :: StudyProgram -> StudyProgramKey
studyProgramKey (StudyProgram x _ _ _ _ _) = StudyProgramKey x

--- Shows the key of a StudyProgram entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showStudyProgramKey :: StudyProgram -> String
showStudyProgramKey obj =
  showDatabaseKey "StudyProgram" studyProgramKeyToKey (studyProgramKey obj)

--- Transforms a string into a key of a StudyProgram entity.
--- Nothing is returned if the string does not represent a reasonable key.
readStudyProgramKey :: String -> Maybe StudyProgramKey
readStudyProgramKey s = readDatabaseKey "StudyProgram" StudyProgramKey s

studyProgramKeyToKey :: StudyProgramKey -> Key
studyProgramKeyToKey (StudyProgramKey k) = k

maybeStudyProgramKeyToKey :: Maybe StudyProgramKey -> Maybe Key
maybeStudyProgramKeyToKey Nothing = Nothing
maybeStudyProgramKeyToKey (Just (StudyProgramKey k)) = Just k

--- Inserts a new StudyProgram entity.
newStudyProgram
  :: String -> String -> String -> String -> Int -> Transaction StudyProgram
newStudyProgram name_p nameE_p shortName_p progKey_p position_p =
  unique "MDB" studyProgramEntry keytuple2StudyProgram studyProgramShortName
   shortName_p
   |>> (unique "MDB" studyProgramEntry keytuple2StudyProgram
         studyProgramProgKey
         progKey_p
         |>> newEntry studyProgramEntry keytuple2StudyProgram
              (name_p,nameE_p,shortName_p,progKey_p,position_p))

--- Updates an existing StudyProgram entity.
updateStudyProgram :: StudyProgram -> Transaction ()
updateStudyProgram studyProgram_p =
  uniqueUpdate "MDB" studyProgramEntry keytuple2StudyProgram
   (studyProgramKeyToKey . studyProgramKey)
   studyProgramShortName
   studyProgram_p
   |>> (uniqueUpdate "MDB" studyProgramEntry keytuple2StudyProgram
         (studyProgramKeyToKey . studyProgramKey)
         studyProgramProgKey
         studyProgram_p
         |>> updateDBEntry studyProgramEntry
              (studyProgramKeyToKey (studyProgramKey studyProgram_p))
              (studyProgram2tuple studyProgram_p))

--- Deletes an existing StudyProgram entity.
deleteStudyProgram :: StudyProgram -> Transaction ()
deleteStudyProgram studyProgram_p =
  requiredForeignDBKey "Category" categoryEntry keytuple2Category
   categoryStudyProgramProgramCategoriesKey
   (studyProgramKey studyProgram_p)
   |>> (requiredForeignDBKey "AdvisorStudyProgram" advisorStudyProgramEntry
         keytuple2AdvisorStudyProgram
         advisorStudyProgramStudyProgramStudyProgramsAdvisedKey
         (studyProgramKey studyProgram_p)
         |>> deleteDBEntry studyProgramEntry
              (studyProgramKeyToKey (studyProgramKey studyProgram_p)))

--- Gets a StudyProgram entity stored in the database with the given key.
getStudyProgram :: StudyProgramKey -> Transaction StudyProgram
getStudyProgram key =
  getEntry studyProgramEntry keytuple2StudyProgram (studyProgramKeyToKey key)

--- Gets all StudyProgram entities stored in the database.
queryAllStudyPrograms :: Query [StudyProgram]
queryAllStudyPrograms =
  transformQ (map (uncurry keytuple2StudyProgram))
   (allDBKeyInfos studyProgramEntry)

--- Gets all StudyProgram entities satisfying a given condition.
queryCondStudyProgram :: (StudyProgram -> Bool) -> Query [StudyProgram]
queryCondStudyProgram econd = transformQ (filter econd) queryAllStudyPrograms

--- Database predicate representing the relation between keys and Category tuple entities.
categoryEntry :: Key -> CategoryTuple -> Dynamic
categoryEntry =
  persistentSQLite dbFile "Category"
   ["Name"
   ,"NameE"
   ,"ShortName"
   ,"Comment"
   ,"MinECTS"
   ,"MaxECTS"
   ,"Position"
   ,"StudyProgramProgramCategoriesKey"]

--- Dynamic predicate representing the relation
--- between keys and Category entities.
category :: CategoryKey -> Category -> Dynamic
category key obj
  | key =:= categoryKey obj
  = categoryEntry (categoryKeyToKey key) (category2tuple obj)

--- Gets the key of a Category entity.
categoryKey :: Category -> CategoryKey
categoryKey (Category x _ _ _ _ _ _ _ _) = CategoryKey x

--- Shows the key of a Category entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showCategoryKey :: Category -> String
showCategoryKey obj =
  showDatabaseKey "Category" categoryKeyToKey (categoryKey obj)

--- Transforms a string into a key of a Category entity.
--- Nothing is returned if the string does not represent a reasonable key.
readCategoryKey :: String -> Maybe CategoryKey
readCategoryKey s = readDatabaseKey "Category" CategoryKey s

categoryKeyToKey :: CategoryKey -> Key
categoryKeyToKey (CategoryKey k) = k

maybeCategoryKeyToKey :: Maybe CategoryKey -> Maybe Key
maybeCategoryKeyToKey Nothing = Nothing
maybeCategoryKeyToKey (Just (CategoryKey k)) = Just k

--- Inserts a new Category entity.
newCategoryWithStudyProgramProgramCategoriesKey
  :: String
  -> String
  -> String
  -> String
  -> Maybe Int -> Maybe Int -> Int -> StudyProgramKey -> Transaction Category
newCategoryWithStudyProgramProgramCategoriesKey
    name_p
    nameE_p
    shortName_p
    comment_p
    minECTS_p
    maxECTS_p
    position_p
    studyProgramProgramCategoriesKey_p =
  existsEntryWithDBKey "StudyProgram" studyProgramEntry
   (studyProgramKeyToKey studyProgramProgramCategoriesKey_p)
   |>> newEntry categoryEntry keytuple2Category
        (name_p
        ,nameE_p
        ,shortName_p
        ,comment_p
        ,maybe 0 id minECTS_p
        ,maybe 180 id maxECTS_p
        ,position_p
        ,studyProgramKeyToKey studyProgramProgramCategoriesKey_p)

--- Updates an existing Category entity.
updateCategory :: Category -> Transaction ()
updateCategory category_p =
  existsEntryWithDBKey "StudyProgram" studyProgramEntry
   (studyProgramKeyToKey
     (categoryStudyProgramProgramCategoriesKey category_p))
   |>> updateDBEntry categoryEntry (categoryKeyToKey (categoryKey category_p))
        (category2tuple category_p)

--- Deletes an existing Category entity.
deleteCategory :: Category -> Transaction ()
deleteCategory category_p =
  requiredForeignDBKey "Categorizing" categorizingEntry keytuple2Categorizing
   categorizingCategoryCategorizingKey
   (categoryKey category_p)
   |>> (requiredForeignDBKey "AdvisorModule" advisorModuleEntry
         keytuple2AdvisorModule
         advisorModuleCategoryAdvisorCategorizingKey
         (categoryKey category_p)
         |>> deleteDBEntry categoryEntry
              (categoryKeyToKey (categoryKey category_p)))

--- Gets a Category entity stored in the database with the given key.
getCategory :: CategoryKey -> Transaction Category
getCategory key =
  getEntry categoryEntry keytuple2Category (categoryKeyToKey key)

--- Gets all Category entities stored in the database.
queryAllCategorys :: Query [Category]
queryAllCategorys =
  transformQ (map (uncurry keytuple2Category)) (allDBKeyInfos categoryEntry)

--- Gets all Category entities satisfying a given condition.
queryCondCategory :: (Category -> Bool) -> Query [Category]
queryCondCategory econd = transformQ (filter econd) queryAllCategorys

--- Database predicate representing the relation between keys and MasterCoreArea tuple entities.
masterCoreAreaEntry :: Key -> MasterCoreAreaTuple -> Dynamic
masterCoreAreaEntry =
  persistentSQLite dbFile "MasterCoreArea"
   ["Name","ShortName","Description","AreaKey","Position"]

--- Dynamic predicate representing the relation
--- between keys and MasterCoreArea entities.
masterCoreArea :: MasterCoreAreaKey -> MasterCoreArea -> Dynamic
masterCoreArea key obj
  | key =:= masterCoreAreaKey obj
  = masterCoreAreaEntry (masterCoreAreaKeyToKey key)
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
  showDatabaseKey "MasterCoreArea" masterCoreAreaKeyToKey
   (masterCoreAreaKey obj)

--- Transforms a string into a key of a MasterCoreArea entity.
--- Nothing is returned if the string does not represent a reasonable key.
readMasterCoreAreaKey :: String -> Maybe MasterCoreAreaKey
readMasterCoreAreaKey s = readDatabaseKey "MasterCoreArea" MasterCoreAreaKey s

masterCoreAreaKeyToKey :: MasterCoreAreaKey -> Key
masterCoreAreaKeyToKey (MasterCoreAreaKey k) = k

maybeMasterCoreAreaKeyToKey :: Maybe MasterCoreAreaKey -> Maybe Key
maybeMasterCoreAreaKeyToKey Nothing = Nothing
maybeMasterCoreAreaKeyToKey (Just (MasterCoreAreaKey k)) = Just k

--- Inserts a new MasterCoreArea entity.
newMasterCoreArea
  :: String
  -> String -> String -> String -> Maybe Int -> Transaction MasterCoreArea
newMasterCoreArea name_p shortName_p description_p areaKey_p position_p =
  unique "MDB" masterCoreAreaEntry keytuple2MasterCoreArea
   masterCoreAreaAreaKey
   areaKey_p
   |>> newEntry masterCoreAreaEntry keytuple2MasterCoreArea
        (name_p,shortName_p,description_p,areaKey_p,maybe 1 id position_p)

--- Updates an existing MasterCoreArea entity.
updateMasterCoreArea :: MasterCoreArea -> Transaction ()
updateMasterCoreArea masterCoreArea_p =
  uniqueUpdate "MDB" masterCoreAreaEntry keytuple2MasterCoreArea
   (masterCoreAreaKeyToKey . masterCoreAreaKey)
   masterCoreAreaAreaKey
   masterCoreArea_p
   |>> updateDBEntry masterCoreAreaEntry
        (masterCoreAreaKeyToKey (masterCoreAreaKey masterCoreArea_p))
        (masterCoreArea2tuple masterCoreArea_p)

--- Deletes an existing MasterCoreArea entity.
deleteMasterCoreArea :: MasterCoreArea -> Transaction ()
deleteMasterCoreArea masterCoreArea_p =
  requiredForeignDBKey "MasterProgram" masterProgramEntry
   keytuple2MasterProgram
   masterProgramMasterCoreAreaAreaProgramsKey
   (masterCoreAreaKey masterCoreArea_p)
   |>> deleteDBEntry masterCoreAreaEntry
        (masterCoreAreaKeyToKey (masterCoreAreaKey masterCoreArea_p))

--- Gets a MasterCoreArea entity stored in the database with the given key.
getMasterCoreArea :: MasterCoreAreaKey -> Transaction MasterCoreArea
getMasterCoreArea key =
  getEntry masterCoreAreaEntry keytuple2MasterCoreArea
   (masterCoreAreaKeyToKey key)

--- Gets all MasterCoreArea entities stored in the database.
queryAllMasterCoreAreas :: Query [MasterCoreArea]
queryAllMasterCoreAreas =
  transformQ (map (uncurry keytuple2MasterCoreArea))
   (allDBKeyInfos masterCoreAreaEntry)

--- Gets all MasterCoreArea entities satisfying a given condition.
queryCondMasterCoreArea :: (MasterCoreArea -> Bool) -> Query [MasterCoreArea]
queryCondMasterCoreArea econd =
  transformQ (filter econd) queryAllMasterCoreAreas

--- Database predicate representing the relation between keys and User tuple entities.
userEntry :: Key -> UserTuple -> Dynamic
userEntry =
  persistentSQLite dbFile "User"
   ["Login","Name","First","Title","Email","Url","Password","LastLogin"]

--- Dynamic predicate representing the relation
--- between keys and User entities.
user :: UserKey -> User -> Dynamic
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
showUserKey obj = showDatabaseKey "User" userKeyToKey (userKey obj)

--- Transforms a string into a key of a User entity.
--- Nothing is returned if the string does not represent a reasonable key.
readUserKey :: String -> Maybe UserKey
readUserKey s = readDatabaseKey "User" UserKey s

userKeyToKey :: UserKey -> Key
userKeyToKey (UserKey k) = k

maybeUserKeyToKey :: Maybe UserKey -> Maybe Key
maybeUserKeyToKey Nothing = Nothing
maybeUserKeyToKey (Just (UserKey k)) = Just k

--- Inserts a new User entity.
newUser
  :: String
  -> String
  -> String
  -> String -> String -> String -> String -> CalendarTime -> Transaction User
newUser login_p name_p first_p title_p email_p url_p password_p lastLogin_p =
  unique "MDB" userEntry keytuple2User userLogin login_p
   |>> newEntry userEntry keytuple2User
        (login_p,name_p,first_p,title_p,email_p,url_p,password_p,lastLogin_p)

--- Updates an existing User entity.
updateUser :: User -> Transaction ()
updateUser user_p =
  uniqueUpdate "MDB" userEntry keytuple2User (userKeyToKey . userKey)
   userLogin
   user_p
   |>> updateDBEntry userEntry (userKeyToKey (userKey user_p))
        (user2tuple user_p)

--- Deletes an existing User entity.
deleteUser :: User -> Transaction ()
deleteUser user_p =
  requiredForeignDBKey "ModData" modDataEntry keytuple2ModData
   modDataUserResponsibleKey
   (userKey user_p)
   |>> (requiredForeignDBKey "ModInst" modInstEntry keytuple2ModInst
         modInstUserLecturerModsKey
         (userKey user_p)
         |>> (requiredForeignDBKey "AdvisorStudyProgram"
               advisorStudyProgramEntry
               keytuple2AdvisorStudyProgram
               advisorStudyProgramUserStudyAdvisingKey
               (userKey user_p)
               |>> (requiredForeignDBKey "MasterProgram" masterProgramEntry
                     keytuple2MasterProgram
                     masterProgramUserAdvisingKey
                     (userKey user_p)
                     |>> deleteDBEntry userEntry
                          (userKeyToKey (userKey user_p)))))

--- Gets a User entity stored in the database with the given key.
getUser :: UserKey -> Transaction User
getUser key = getEntry userEntry keytuple2User (userKeyToKey key)

--- Gets all User entities stored in the database.
queryAllUsers :: Query [User]
queryAllUsers =
  transformQ (map (uncurry keytuple2User)) (allDBKeyInfos userEntry)

--- Gets all User entities satisfying a given condition.
queryCondUser :: (User -> Bool) -> Query [User]
queryCondUser econd = transformQ (filter econd) queryAllUsers

--- Database predicate representing the relation between keys and ModData tuple entities.
modDataEntry :: Key -> ModDataTuple -> Dynamic
modDataEntry =
  persistentSQLite dbFile "ModData"
   ["Code"
   ,"NameG"
   ,"NameE"
   ,"Cycle"
   ,"Presence"
   ,"ECTS"
   ,"Workload"
   ,"Length"
   ,"URL"
   ,"Visible"
   ,"UserResponsibleKey"]

--- Dynamic predicate representing the relation
--- between keys and ModData entities.
modData :: ModDataKey -> ModData -> Dynamic
modData key obj
  | key =:= modDataKey obj
  = modDataEntry (modDataKeyToKey key) (modData2tuple obj)

--- Gets the key of a ModData entity.
modDataKey :: ModData -> ModDataKey
modDataKey (ModData x _ _ _ _ _ _ _ _ _ _ _) = ModDataKey x

--- Shows the key of a ModData entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showModDataKey :: ModData -> String
showModDataKey obj =
  showDatabaseKey "ModData" modDataKeyToKey (modDataKey obj)

--- Transforms a string into a key of a ModData entity.
--- Nothing is returned if the string does not represent a reasonable key.
readModDataKey :: String -> Maybe ModDataKey
readModDataKey s = readDatabaseKey "ModData" ModDataKey s

modDataKeyToKey :: ModDataKey -> Key
modDataKeyToKey (ModDataKey k) = k

maybeModDataKeyToKey :: Maybe ModDataKey -> Maybe Key
maybeModDataKeyToKey Nothing = Nothing
maybeModDataKeyToKey (Just (ModDataKey k)) = Just k

--- Inserts a new ModData entity.
newModDataWithUserResponsibleKey
  :: String
  -> String
  -> String
  -> String
  -> String
  -> Maybe Int
  -> String -> Maybe Int -> String -> Bool -> UserKey -> Transaction ModData
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
  unique "MDB" modDataEntry keytuple2ModData modDataCode code_p
   |>> (existsEntryWithDBKey "User" userEntry
         (userKeyToKey userResponsibleKey_p)
         |>> newEntry modDataEntry keytuple2ModData
              (code_p
              ,nameG_p
              ,nameE_p
              ,cycle_p
              ,presence_p
              ,maybe 8 id eCTS_p
              ,workload_p
              ,maybe 1 id length_p
              ,uRL_p
              ,visible_p
              ,userKeyToKey userResponsibleKey_p))

--- Updates an existing ModData entity.
updateModData :: ModData -> Transaction ()
updateModData modData_p =
  uniqueUpdate "MDB" modDataEntry keytuple2ModData
   (modDataKeyToKey . modDataKey)
   modDataCode
   modData_p
   |>> (existsEntryWithDBKey "User" userEntry
         (userKeyToKey (modDataUserResponsibleKey modData_p))
         |>> updateDBEntry modDataEntry
              (modDataKeyToKey (modDataKey modData_p))
              (modData2tuple modData_p))

--- Deletes an existing ModData entity.
deleteModData :: ModData -> Transaction ()
deleteModData modData_p =
  requiredForeignDBKey "Categorizing" categorizingEntry keytuple2Categorizing
   categorizingModDataCategorizingKey
   (modDataKey modData_p)
   |>> (requiredForeignDBKey "ModDescr" modDescrEntry keytuple2ModDescr
         modDescrModDataDataDescKey
         (modDataKey modData_p)
         |>> (requiredForeignDBKey "ModInst" modInstEntry keytuple2ModInst
               modInstModDataModuleInstancesKey
               (modDataKey modData_p)
               |>> deleteDBEntry modDataEntry
                    (modDataKeyToKey (modDataKey modData_p))))

--- Gets a ModData entity stored in the database with the given key.
getModData :: ModDataKey -> Transaction ModData
getModData key = getEntry modDataEntry keytuple2ModData (modDataKeyToKey key)

--- Gets all ModData entities stored in the database.
queryAllModDatas :: Query [ModData]
queryAllModDatas =
  transformQ (map (uncurry keytuple2ModData)) (allDBKeyInfos modDataEntry)

--- Gets all ModData entities satisfying a given condition.
queryCondModData :: (ModData -> Bool) -> Query [ModData]
queryCondModData econd = transformQ (filter econd) queryAllModDatas

--- Database predicate representing the relation between keys and ModDescr tuple entities.
modDescrEntry :: Key -> ModDescrTuple -> Dynamic
modDescrEntry =
  persistentSQLite dbFile "ModDescr"
   ["Language"
   ,"ShortDesc"
   ,"Objectives"
   ,"Contents"
   ,"Prereq"
   ,"Exam"
   ,"Methods"
   ,"Use"
   ,"Literature"
   ,"Links"
   ,"Comments"
   ,"ModDataDataDescKey"]

--- Dynamic predicate representing the relation
--- between keys and ModDescr entities.
modDescr :: ModDescrKey -> ModDescr -> Dynamic
modDescr key obj
  | key =:= modDescrKey obj
  = modDescrEntry (modDescrKeyToKey key) (modDescr2tuple obj)

--- Gets the key of a ModDescr entity.
modDescrKey :: ModDescr -> ModDescrKey
modDescrKey (ModDescr x _ _ _ _ _ _ _ _ _ _ _ _) = ModDescrKey x

--- Shows the key of a ModDescr entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showModDescrKey :: ModDescr -> String
showModDescrKey obj =
  showDatabaseKey "ModDescr" modDescrKeyToKey (modDescrKey obj)

--- Transforms a string into a key of a ModDescr entity.
--- Nothing is returned if the string does not represent a reasonable key.
readModDescrKey :: String -> Maybe ModDescrKey
readModDescrKey s = readDatabaseKey "ModDescr" ModDescrKey s

modDescrKeyToKey :: ModDescrKey -> Key
modDescrKeyToKey (ModDescrKey k) = k

maybeModDescrKeyToKey :: Maybe ModDescrKey -> Maybe Key
maybeModDescrKeyToKey Nothing = Nothing
maybeModDescrKeyToKey (Just (ModDescrKey k)) = Just k

--- Inserts a new ModDescr entity.
newModDescrWithModDataDataDescKey
  :: String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> String -> String -> String -> ModDataKey -> Transaction ModDescr
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
  unique "MDB" modDescrEntry keytuple2ModDescr modDescrModDataDataDescKey
   modDataDataDescKey_p
   |>> (existsEntryWithDBKey "ModData" modDataEntry
         (modDataKeyToKey modDataDataDescKey_p)
         |>> newEntry modDescrEntry keytuple2ModDescr
              (language_p
              ,shortDesc_p
              ,objectives_p
              ,contents_p
              ,prereq_p
              ,exam_p
              ,methods_p
              ,use_p
              ,literature_p
              ,links_p
              ,comments_p
              ,modDataKeyToKey modDataDataDescKey_p))

--- Updates an existing ModDescr entity.
updateModDescr :: ModDescr -> Transaction ()
updateModDescr modDescr_p =
  uniqueUpdate "MDB" modDescrEntry keytuple2ModDescr
   (modDescrKeyToKey . modDescrKey)
   modDescrModDataDataDescKey
   modDescr_p
   |>> (existsEntryWithDBKey "ModData" modDataEntry
         (modDataKeyToKey (modDescrModDataDataDescKey modDescr_p))
         |>> updateDBEntry modDescrEntry
              (modDescrKeyToKey (modDescrKey modDescr_p))
              (modDescr2tuple modDescr_p))

--- Deletes an existing ModDescr entity.
deleteModDescr :: ModDescr -> Transaction ()
deleteModDescr modDescr_p =
  deleteDBEntry modDescrEntry (modDescrKeyToKey (modDescrKey modDescr_p))

--- Gets a ModDescr entity stored in the database with the given key.
getModDescr :: ModDescrKey -> Transaction ModDescr
getModDescr key =
  getEntry modDescrEntry keytuple2ModDescr (modDescrKeyToKey key)

--- Gets all ModDescr entities stored in the database.
queryAllModDescrs :: Query [ModDescr]
queryAllModDescrs =
  transformQ (map (uncurry keytuple2ModDescr)) (allDBKeyInfos modDescrEntry)

--- Gets all ModDescr entities satisfying a given condition.
queryCondModDescr :: (ModDescr -> Bool) -> Query [ModDescr]
queryCondModDescr econd = transformQ (filter econd) queryAllModDescrs

--- Database predicate representing the relation between keys and ModInst tuple entities.
modInstEntry :: Key -> ModInstTuple -> Dynamic
modInstEntry =
  persistentSQLite dbFile "ModInst"
   ["Term","Year","UserLecturerModsKey","ModDataModuleInstancesKey"]

--- Dynamic predicate representing the relation
--- between keys and ModInst entities.
modInst :: ModInstKey -> ModInst -> Dynamic
modInst key obj
  | key =:= modInstKey obj
  = modInstEntry (modInstKeyToKey key) (modInst2tuple obj)

--- Gets the key of a ModInst entity.
modInstKey :: ModInst -> ModInstKey
modInstKey (ModInst x _ _ _ _) = ModInstKey x

--- Shows the key of a ModInst entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showModInstKey :: ModInst -> String
showModInstKey obj =
  showDatabaseKey "ModInst" modInstKeyToKey (modInstKey obj)

--- Transforms a string into a key of a ModInst entity.
--- Nothing is returned if the string does not represent a reasonable key.
readModInstKey :: String -> Maybe ModInstKey
readModInstKey s = readDatabaseKey "ModInst" ModInstKey s

modInstKeyToKey :: ModInstKey -> Key
modInstKeyToKey (ModInstKey k) = k

maybeModInstKeyToKey :: Maybe ModInstKey -> Maybe Key
maybeModInstKeyToKey Nothing = Nothing
maybeModInstKeyToKey (Just (ModInstKey k)) = Just k

--- Inserts a new ModInst entity.
newModInstWithUserLecturerModsKeyWithModDataModuleInstancesKey
  :: String -> Maybe Int -> UserKey -> ModDataKey -> Transaction ModInst
newModInstWithUserLecturerModsKeyWithModDataModuleInstancesKey
    term_p year_p userLecturerModsKey_p modDataModuleInstancesKey_p =
  existsEntryWithDBKey "User" userEntry (userKeyToKey userLecturerModsKey_p)
   |>> (existsEntryWithDBKey "ModData" modDataEntry
         (modDataKeyToKey modDataModuleInstancesKey_p)
         |>> newEntry modInstEntry keytuple2ModInst
              (term_p
              ,maybe 2011 id year_p
              ,userKeyToKey userLecturerModsKey_p
              ,modDataKeyToKey modDataModuleInstancesKey_p))

--- Updates an existing ModInst entity.
updateModInst :: ModInst -> Transaction ()
updateModInst modInst_p =
  existsEntryWithDBKey "User" userEntry
   (userKeyToKey (modInstUserLecturerModsKey modInst_p))
   |>> (existsEntryWithDBKey "ModData" modDataEntry
         (modDataKeyToKey (modInstModDataModuleInstancesKey modInst_p))
         |>> updateDBEntry modInstEntry
              (modInstKeyToKey (modInstKey modInst_p))
              (modInst2tuple modInst_p))

--- Deletes an existing ModInst entity.
deleteModInst :: ModInst -> Transaction ()
deleteModInst modInst_p =
  requiredForeignDBKey "AdvisorModule" advisorModuleEntry
   keytuple2AdvisorModule
   advisorModuleModInstAdvisedProgramModuleInstancesKey
   (modInstKey modInst_p)
   |>> deleteDBEntry modInstEntry (modInstKeyToKey (modInstKey modInst_p))

--- Gets a ModInst entity stored in the database with the given key.
getModInst :: ModInstKey -> Transaction ModInst
getModInst key = getEntry modInstEntry keytuple2ModInst (modInstKeyToKey key)

--- Gets all ModInst entities stored in the database.
queryAllModInsts :: Query [ModInst]
queryAllModInsts =
  transformQ (map (uncurry keytuple2ModInst)) (allDBKeyInfos modInstEntry)

--- Gets all ModInst entities satisfying a given condition.
queryCondModInst :: (ModInst -> Bool) -> Query [ModInst]
queryCondModInst econd = transformQ (filter econd) queryAllModInsts

--- Database predicate representing the relation between keys and AdvisorStudyProgram tuple entities.
advisorStudyProgramEntry :: Key -> AdvisorStudyProgramTuple -> Dynamic
advisorStudyProgramEntry =
  persistentSQLite dbFile "AdvisorStudyProgram"
   ["Name"
   ,"Term"
   ,"Year"
   ,"Desc"
   ,"Prereq"
   ,"Comments"
   ,"Visible"
   ,"UserStudyAdvisingKey"
   ,"StudyProgramStudyProgramsAdvisedKey"]

--- Dynamic predicate representing the relation
--- between keys and AdvisorStudyProgram entities.
advisorStudyProgram
  :: AdvisorStudyProgramKey -> AdvisorStudyProgram -> Dynamic
advisorStudyProgram key obj
  | key =:= advisorStudyProgramKey obj
  = advisorStudyProgramEntry (advisorStudyProgramKeyToKey key)
     (advisorStudyProgram2tuple obj)

--- Gets the key of a AdvisorStudyProgram entity.
advisorStudyProgramKey :: AdvisorStudyProgram -> AdvisorStudyProgramKey
advisorStudyProgramKey (AdvisorStudyProgram x _ _ _ _ _ _ _ _ _) =
  AdvisorStudyProgramKey x

--- Shows the key of a AdvisorStudyProgram entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showAdvisorStudyProgramKey :: AdvisorStudyProgram -> String
showAdvisorStudyProgramKey obj =
  showDatabaseKey "AdvisorStudyProgram" advisorStudyProgramKeyToKey
   (advisorStudyProgramKey obj)

--- Transforms a string into a key of a AdvisorStudyProgram entity.
--- Nothing is returned if the string does not represent a reasonable key.
readAdvisorStudyProgramKey :: String -> Maybe AdvisorStudyProgramKey
readAdvisorStudyProgramKey s =
  readDatabaseKey "AdvisorStudyProgram" AdvisorStudyProgramKey s

advisorStudyProgramKeyToKey :: AdvisorStudyProgramKey -> Key
advisorStudyProgramKeyToKey (AdvisorStudyProgramKey k) = k

maybeAdvisorStudyProgramKeyToKey :: Maybe AdvisorStudyProgramKey -> Maybe Key
maybeAdvisorStudyProgramKeyToKey Nothing = Nothing
maybeAdvisorStudyProgramKeyToKey (Just (AdvisorStudyProgramKey k)) = Just k

--- Inserts a new AdvisorStudyProgram entity.
newAdvisorStudyProgramWithUserStudyAdvisingKeyWithStudyProgramStudyProgramsAdvisedKey
  :: String
  -> String
  -> Maybe Int
  -> String
  -> String
  -> String
  -> Bool -> UserKey -> StudyProgramKey -> Transaction AdvisorStudyProgram
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
  existsEntryWithDBKey "User" userEntry (userKeyToKey userStudyAdvisingKey_p)
   |>> (existsEntryWithDBKey "StudyProgram" studyProgramEntry
         (studyProgramKeyToKey studyProgramStudyProgramsAdvisedKey_p)
         |>> newEntry advisorStudyProgramEntry keytuple2AdvisorStudyProgram
              (name_p
              ,term_p
              ,maybe 2015 id year_p
              ,desc_p
              ,prereq_p
              ,comments_p
              ,visible_p
              ,userKeyToKey userStudyAdvisingKey_p
              ,studyProgramKeyToKey studyProgramStudyProgramsAdvisedKey_p))

--- Updates an existing AdvisorStudyProgram entity.
updateAdvisorStudyProgram :: AdvisorStudyProgram -> Transaction ()
updateAdvisorStudyProgram advisorStudyProgram_p =
  existsEntryWithDBKey "User" userEntry
   (userKeyToKey
     (advisorStudyProgramUserStudyAdvisingKey advisorStudyProgram_p))
   |>> (existsEntryWithDBKey "StudyProgram" studyProgramEntry
         (studyProgramKeyToKey
           (advisorStudyProgramStudyProgramStudyProgramsAdvisedKey
             advisorStudyProgram_p))
         |>> updateDBEntry advisorStudyProgramEntry
              (advisorStudyProgramKeyToKey
                (advisorStudyProgramKey advisorStudyProgram_p))
              (advisorStudyProgram2tuple advisorStudyProgram_p))

--- Deletes an existing AdvisorStudyProgram entity.
deleteAdvisorStudyProgram :: AdvisorStudyProgram -> Transaction ()
deleteAdvisorStudyProgram advisorStudyProgram_p =
  requiredForeignDBKey "AdvisorModule" advisorModuleEntry
   keytuple2AdvisorModule
   advisorModuleAdvisorStudyProgramAdvisorProgramModulesKey
   (advisorStudyProgramKey advisorStudyProgram_p)
   |>> deleteDBEntry advisorStudyProgramEntry
        (advisorStudyProgramKeyToKey
          (advisorStudyProgramKey advisorStudyProgram_p))

--- Gets a AdvisorStudyProgram entity stored in the database with the given key.
getAdvisorStudyProgram
  :: AdvisorStudyProgramKey -> Transaction AdvisorStudyProgram
getAdvisorStudyProgram key =
  getEntry advisorStudyProgramEntry keytuple2AdvisorStudyProgram
   (advisorStudyProgramKeyToKey key)

--- Gets all AdvisorStudyProgram entities stored in the database.
queryAllAdvisorStudyPrograms :: Query [AdvisorStudyProgram]
queryAllAdvisorStudyPrograms =
  transformQ (map (uncurry keytuple2AdvisorStudyProgram))
   (allDBKeyInfos advisorStudyProgramEntry)

--- Gets all AdvisorStudyProgram entities satisfying a given condition.
queryCondAdvisorStudyProgram
  :: (AdvisorStudyProgram -> Bool) -> Query [AdvisorStudyProgram]
queryCondAdvisorStudyProgram econd =
  transformQ (filter econd) queryAllAdvisorStudyPrograms

--- Database predicate representing the relation between keys and AdvisorModule tuple entities.
advisorModuleEntry :: Key -> AdvisorModuleTuple -> Dynamic
advisorModuleEntry =
  persistentSQLite dbFile "AdvisorModule"
   ["Mandatory"
   ,"AdvisorStudyProgramAdvisorProgramModulesKey"
   ,"CategoryAdvisorCategorizingKey"
   ,"ModInstAdvisedProgramModuleInstancesKey"]

--- Dynamic predicate representing the relation
--- between keys and AdvisorModule entities.
advisorModule :: AdvisorModuleKey -> AdvisorModule -> Dynamic
advisorModule key obj
  | key =:= advisorModuleKey obj
  = advisorModuleEntry (advisorModuleKeyToKey key) (advisorModule2tuple obj)

--- Gets the key of a AdvisorModule entity.
advisorModuleKey :: AdvisorModule -> AdvisorModuleKey
advisorModuleKey (AdvisorModule x _ _ _ _) = AdvisorModuleKey x

--- Shows the key of a AdvisorModule entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showAdvisorModuleKey :: AdvisorModule -> String
showAdvisorModuleKey obj =
  showDatabaseKey "AdvisorModule" advisorModuleKeyToKey (advisorModuleKey obj)

--- Transforms a string into a key of a AdvisorModule entity.
--- Nothing is returned if the string does not represent a reasonable key.
readAdvisorModuleKey :: String -> Maybe AdvisorModuleKey
readAdvisorModuleKey s = readDatabaseKey "AdvisorModule" AdvisorModuleKey s

advisorModuleKeyToKey :: AdvisorModuleKey -> Key
advisorModuleKeyToKey (AdvisorModuleKey k) = k

maybeAdvisorModuleKeyToKey :: Maybe AdvisorModuleKey -> Maybe Key
maybeAdvisorModuleKeyToKey Nothing = Nothing
maybeAdvisorModuleKeyToKey (Just (AdvisorModuleKey k)) = Just k

--- Inserts a new AdvisorModule entity.
newAdvisorModuleWithAdvisorStudyProgramAdvisorProgramModulesKeyWithCategoryAdvisorCategorizingKeyWithModInstAdvisedProgramModuleInstancesKey
  :: Bool
  -> AdvisorStudyProgramKey
  -> CategoryKey -> ModInstKey -> Transaction AdvisorModule
newAdvisorModuleWithAdvisorStudyProgramAdvisorProgramModulesKeyWithCategoryAdvisorCategorizingKeyWithModInstAdvisedProgramModuleInstancesKey
    mandatory_p
    advisorStudyProgramAdvisorProgramModulesKey_p
    categoryAdvisorCategorizingKey_p
    modInstAdvisedProgramModuleInstancesKey_p =
  existsEntryWithDBKey "AdvisorStudyProgram" advisorStudyProgramEntry
   (advisorStudyProgramKeyToKey advisorStudyProgramAdvisorProgramModulesKey_p)
   |>> (existsEntryWithDBKey "Category" categoryEntry
         (categoryKeyToKey categoryAdvisorCategorizingKey_p)
         |>> (existsEntryWithDBKey "ModInst" modInstEntry
               (modInstKeyToKey modInstAdvisedProgramModuleInstancesKey_p)
               |>> newEntry advisorModuleEntry keytuple2AdvisorModule
                    (mandatory_p
                    ,advisorStudyProgramKeyToKey
                      advisorStudyProgramAdvisorProgramModulesKey_p
                    ,categoryKeyToKey categoryAdvisorCategorizingKey_p
                    ,modInstKeyToKey
                      modInstAdvisedProgramModuleInstancesKey_p)))

--- Updates an existing AdvisorModule entity.
updateAdvisorModule :: AdvisorModule -> Transaction ()
updateAdvisorModule advisorModule_p =
  existsEntryWithDBKey "AdvisorStudyProgram" advisorStudyProgramEntry
   (advisorStudyProgramKeyToKey
     (advisorModuleAdvisorStudyProgramAdvisorProgramModulesKey
       advisorModule_p))
   |>> (existsEntryWithDBKey "Category" categoryEntry
         (categoryKeyToKey
           (advisorModuleCategoryAdvisorCategorizingKey advisorModule_p))
         |>> (existsEntryWithDBKey "ModInst" modInstEntry
               (modInstKeyToKey
                 (advisorModuleModInstAdvisedProgramModuleInstancesKey
                   advisorModule_p))
               |>> updateDBEntry advisorModuleEntry
                    (advisorModuleKeyToKey (advisorModuleKey advisorModule_p))
                    (advisorModule2tuple advisorModule_p)))

--- Deletes an existing AdvisorModule entity.
deleteAdvisorModule :: AdvisorModule -> Transaction ()
deleteAdvisorModule advisorModule_p =
  deleteDBEntry advisorModuleEntry
   (advisorModuleKeyToKey (advisorModuleKey advisorModule_p))

--- Gets a AdvisorModule entity stored in the database with the given key.
getAdvisorModule :: AdvisorModuleKey -> Transaction AdvisorModule
getAdvisorModule key =
  getEntry advisorModuleEntry keytuple2AdvisorModule
   (advisorModuleKeyToKey key)

--- Gets all AdvisorModule entities stored in the database.
queryAllAdvisorModules :: Query [AdvisorModule]
queryAllAdvisorModules =
  transformQ (map (uncurry keytuple2AdvisorModule))
   (allDBKeyInfos advisorModuleEntry)

--- Gets all AdvisorModule entities satisfying a given condition.
queryCondAdvisorModule :: (AdvisorModule -> Bool) -> Query [AdvisorModule]
queryCondAdvisorModule econd =
  transformQ (filter econd) queryAllAdvisorModules

--- Database predicate representing the relation between keys and MasterProgram tuple entities.
masterProgramEntry :: Key -> MasterProgramTuple -> Dynamic
masterProgramEntry =
  persistentSQLite dbFile "MasterProgram"
   ["Name"
   ,"Term"
   ,"Year"
   ,"Desc"
   ,"Prereq"
   ,"Comments"
   ,"Visible"
   ,"UserAdvisingKey"
   ,"MasterCoreAreaAreaProgramsKey"]

--- Dynamic predicate representing the relation
--- between keys and MasterProgram entities.
masterProgram :: MasterProgramKey -> MasterProgram -> Dynamic
masterProgram key obj
  | key =:= masterProgramKey obj
  = masterProgramEntry (masterProgramKeyToKey key) (masterProgram2tuple obj)

--- Gets the key of a MasterProgram entity.
masterProgramKey :: MasterProgram -> MasterProgramKey
masterProgramKey (MasterProgram x _ _ _ _ _ _ _ _ _) = MasterProgramKey x

--- Shows the key of a MasterProgram entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showMasterProgramKey :: MasterProgram -> String
showMasterProgramKey obj =
  showDatabaseKey "MasterProgram" masterProgramKeyToKey (masterProgramKey obj)

--- Transforms a string into a key of a MasterProgram entity.
--- Nothing is returned if the string does not represent a reasonable key.
readMasterProgramKey :: String -> Maybe MasterProgramKey
readMasterProgramKey s = readDatabaseKey "MasterProgram" MasterProgramKey s

masterProgramKeyToKey :: MasterProgramKey -> Key
masterProgramKeyToKey (MasterProgramKey k) = k

maybeMasterProgramKeyToKey :: Maybe MasterProgramKey -> Maybe Key
maybeMasterProgramKeyToKey Nothing = Nothing
maybeMasterProgramKeyToKey (Just (MasterProgramKey k)) = Just k

--- Inserts a new MasterProgram entity.
newMasterProgramWithUserAdvisingKeyWithMasterCoreAreaAreaProgramsKey
  :: String
  -> String
  -> Maybe Int
  -> String
  -> String
  -> String
  -> Bool -> UserKey -> MasterCoreAreaKey -> Transaction MasterProgram
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
  existsEntryWithDBKey "User" userEntry (userKeyToKey userAdvisingKey_p)
   |>> (existsEntryWithDBKey "MasterCoreArea" masterCoreAreaEntry
         (masterCoreAreaKeyToKey masterCoreAreaAreaProgramsKey_p)
         |>> newEntry masterProgramEntry keytuple2MasterProgram
              (name_p
              ,term_p
              ,maybe 2011 id year_p
              ,desc_p
              ,prereq_p
              ,comments_p
              ,visible_p
              ,userKeyToKey userAdvisingKey_p
              ,masterCoreAreaKeyToKey masterCoreAreaAreaProgramsKey_p))

--- Updates an existing MasterProgram entity.
updateMasterProgram :: MasterProgram -> Transaction ()
updateMasterProgram masterProgram_p =
  existsEntryWithDBKey "User" userEntry
   (userKeyToKey (masterProgramUserAdvisingKey masterProgram_p))
   |>> (existsEntryWithDBKey "MasterCoreArea" masterCoreAreaEntry
         (masterCoreAreaKeyToKey
           (masterProgramMasterCoreAreaAreaProgramsKey masterProgram_p))
         |>> updateDBEntry masterProgramEntry
              (masterProgramKeyToKey (masterProgramKey masterProgram_p))
              (masterProgram2tuple masterProgram_p))

--- Deletes an existing MasterProgram entity.
deleteMasterProgram :: MasterProgram -> Transaction ()
deleteMasterProgram masterProgram_p =
  requiredForeignDBKey "MasterProgInfo" masterProgInfoEntry
   keytuple2MasterProgInfo
   masterProgInfoMasterProgramProgramInfoKey
   (masterProgramKey masterProgram_p)
   |>> deleteDBEntry masterProgramEntry
        (masterProgramKeyToKey (masterProgramKey masterProgram_p))

--- Gets a MasterProgram entity stored in the database with the given key.
getMasterProgram :: MasterProgramKey -> Transaction MasterProgram
getMasterProgram key =
  getEntry masterProgramEntry keytuple2MasterProgram
   (masterProgramKeyToKey key)

--- Gets all MasterProgram entities stored in the database.
queryAllMasterPrograms :: Query [MasterProgram]
queryAllMasterPrograms =
  transformQ (map (uncurry keytuple2MasterProgram))
   (allDBKeyInfos masterProgramEntry)

--- Gets all MasterProgram entities satisfying a given condition.
queryCondMasterProgram :: (MasterProgram -> Bool) -> Query [MasterProgram]
queryCondMasterProgram econd =
  transformQ (filter econd) queryAllMasterPrograms

--- Database predicate representing the relation between keys and MasterProgInfo tuple entities.
masterProgInfoEntry :: Key -> MasterProgInfoTuple -> Dynamic
masterProgInfoEntry =
  persistentSQLite dbFile "MasterProgInfo"
   ["ProgModules"
   ,"Praktikum"
   ,"Seminar"
   ,"Thesis"
   ,"AllgGrundlagen"
   ,"Anwendungsfach"
   ,"MasterProgramProgramInfoKey"]

--- Dynamic predicate representing the relation
--- between keys and MasterProgInfo entities.
masterProgInfo :: MasterProgInfoKey -> MasterProgInfo -> Dynamic
masterProgInfo key obj
  | key =:= masterProgInfoKey obj
  = masterProgInfoEntry (masterProgInfoKeyToKey key)
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
  showDatabaseKey "MasterProgInfo" masterProgInfoKeyToKey
   (masterProgInfoKey obj)

--- Transforms a string into a key of a MasterProgInfo entity.
--- Nothing is returned if the string does not represent a reasonable key.
readMasterProgInfoKey :: String -> Maybe MasterProgInfoKey
readMasterProgInfoKey s = readDatabaseKey "MasterProgInfo" MasterProgInfoKey s

masterProgInfoKeyToKey :: MasterProgInfoKey -> Key
masterProgInfoKeyToKey (MasterProgInfoKey k) = k

maybeMasterProgInfoKeyToKey :: Maybe MasterProgInfoKey -> Maybe Key
maybeMasterProgInfoKeyToKey Nothing = Nothing
maybeMasterProgInfoKeyToKey (Just (MasterProgInfoKey k)) = Just k

--- Inserts a new MasterProgInfo entity.
newMasterProgInfoWithMasterProgramProgramInfoKey
  :: String
  -> String
  -> String
  -> String
  -> String -> String -> MasterProgramKey -> Transaction MasterProgInfo
newMasterProgInfoWithMasterProgramProgramInfoKey
    progModules_p
    praktikum_p
    seminar_p
    thesis_p
    allgGrundlagen_p
    anwendungsfach_p
    masterProgramProgramInfoKey_p =
  existsEntryWithDBKey "MasterProgram" masterProgramEntry
   (masterProgramKeyToKey masterProgramProgramInfoKey_p)
   |>> newEntry masterProgInfoEntry keytuple2MasterProgInfo
        (progModules_p
        ,praktikum_p
        ,seminar_p
        ,thesis_p
        ,allgGrundlagen_p
        ,anwendungsfach_p
        ,masterProgramKeyToKey masterProgramProgramInfoKey_p)

--- Updates an existing MasterProgInfo entity.
updateMasterProgInfo :: MasterProgInfo -> Transaction ()
updateMasterProgInfo masterProgInfo_p =
  existsEntryWithDBKey "MasterProgram" masterProgramEntry
   (masterProgramKeyToKey
     (masterProgInfoMasterProgramProgramInfoKey masterProgInfo_p))
   |>> updateDBEntry masterProgInfoEntry
        (masterProgInfoKeyToKey (masterProgInfoKey masterProgInfo_p))
        (masterProgInfo2tuple masterProgInfo_p)

--- Deletes an existing MasterProgInfo entity.
deleteMasterProgInfo :: MasterProgInfo -> Transaction ()
deleteMasterProgInfo masterProgInfo_p =
  deleteDBEntry masterProgInfoEntry
   (masterProgInfoKeyToKey (masterProgInfoKey masterProgInfo_p))

--- Gets a MasterProgInfo entity stored in the database with the given key.
getMasterProgInfo :: MasterProgInfoKey -> Transaction MasterProgInfo
getMasterProgInfo key =
  getEntry masterProgInfoEntry keytuple2MasterProgInfo
   (masterProgInfoKeyToKey key)

--- Gets all MasterProgInfo entities stored in the database.
queryAllMasterProgInfos :: Query [MasterProgInfo]
queryAllMasterProgInfos =
  transformQ (map (uncurry keytuple2MasterProgInfo))
   (allDBKeyInfos masterProgInfoEntry)

--- Gets all MasterProgInfo entities satisfying a given condition.
queryCondMasterProgInfo :: (MasterProgInfo -> Bool) -> Query [MasterProgInfo]
queryCondMasterProgInfo econd =
  transformQ (filter econd) queryAllMasterProgInfos

--- Database predicate representing the relation between keys and UnivisInfo tuple entities.
univisInfoEntry :: Key -> UnivisInfoTuple -> Dynamic
univisInfoEntry =
  persistentSQLite dbFile "UnivisInfo" ["Code","Term","Year","URL"]

--- Dynamic predicate representing the relation
--- between keys and UnivisInfo entities.
univisInfo :: UnivisInfoKey -> UnivisInfo -> Dynamic
univisInfo key obj
  | key =:= univisInfoKey obj
  = univisInfoEntry (univisInfoKeyToKey key) (univisInfo2tuple obj)

--- Gets the key of a UnivisInfo entity.
univisInfoKey :: UnivisInfo -> UnivisInfoKey
univisInfoKey (UnivisInfo x _ _ _ _) = UnivisInfoKey x

--- Shows the key of a UnivisInfo entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showUnivisInfoKey :: UnivisInfo -> String
showUnivisInfoKey obj =
  showDatabaseKey "UnivisInfo" univisInfoKeyToKey (univisInfoKey obj)

--- Transforms a string into a key of a UnivisInfo entity.
--- Nothing is returned if the string does not represent a reasonable key.
readUnivisInfoKey :: String -> Maybe UnivisInfoKey
readUnivisInfoKey s = readDatabaseKey "UnivisInfo" UnivisInfoKey s

univisInfoKeyToKey :: UnivisInfoKey -> Key
univisInfoKeyToKey (UnivisInfoKey k) = k

maybeUnivisInfoKeyToKey :: Maybe UnivisInfoKey -> Maybe Key
maybeUnivisInfoKeyToKey Nothing = Nothing
maybeUnivisInfoKeyToKey (Just (UnivisInfoKey k)) = Just k

--- Inserts a new UnivisInfo entity.
newUnivisInfo :: String -> String -> Int -> String -> Transaction UnivisInfo
newUnivisInfo code_p term_p year_p uRL_p =
  newEntry univisInfoEntry keytuple2UnivisInfo (code_p,term_p,year_p,uRL_p)

--- Updates an existing UnivisInfo entity.
updateUnivisInfo :: UnivisInfo -> Transaction ()
updateUnivisInfo univisInfo_p =
  updateDBEntry univisInfoEntry
   (univisInfoKeyToKey (univisInfoKey univisInfo_p))
   (univisInfo2tuple univisInfo_p)

--- Deletes an existing UnivisInfo entity.
deleteUnivisInfo :: UnivisInfo -> Transaction ()
deleteUnivisInfo univisInfo_p =
  deleteDBEntry univisInfoEntry
   (univisInfoKeyToKey (univisInfoKey univisInfo_p))

--- Gets a UnivisInfo entity stored in the database with the given key.
getUnivisInfo :: UnivisInfoKey -> Transaction UnivisInfo
getUnivisInfo key =
  getEntry univisInfoEntry keytuple2UnivisInfo (univisInfoKeyToKey key)

--- Gets all UnivisInfo entities stored in the database.
queryAllUnivisInfos :: Query [UnivisInfo]
queryAllUnivisInfos =
  transformQ (map (uncurry keytuple2UnivisInfo))
   (allDBKeyInfos univisInfoEntry)

--- Gets all UnivisInfo entities satisfying a given condition.
queryCondUnivisInfo :: (UnivisInfo -> Bool) -> Query [UnivisInfo]
queryCondUnivisInfo econd = transformQ (filter econd) queryAllUnivisInfos

--- Database predicate representing the relation between keys and Categorizing tuple entities.
categorizingEntry :: Key -> CategorizingTuple -> Dynamic
categorizingEntry =
  persistentSQLite dbFile "Categorizing"
   ["ModDataCategorizingKey","CategoryCategorizingKey"]

categorizingModDataCategorizingKey :: Categorizing -> ModDataKey
categorizingModDataCategorizingKey (Categorizing x _) = ModDataKey x

categorizingCategoryCategorizingKey :: Categorizing -> CategoryKey
categorizingCategoryCategorizingKey (Categorizing _ x) = CategoryKey x

--- Dynamic predicate representing the Categorizing relation between ModData entities and Category entities
categorizing :: ModDataKey -> CategoryKey -> Dynamic
categorizing (ModDataKey key1) (CategoryKey key2) =
  categorizingEntry unknown (key1,key2)

--- Inserts a new Categorizing relation between a ModData entity and a Category entity
newCategorizing :: ModDataKey -> CategoryKey -> Transaction ()
newCategorizing key1 key2 =
  existsEntryWithDBKey "ModData" modDataEntry (modDataKeyToKey key1)
   |>> (existsEntryWithDBKey "Category" categoryEntry (categoryKeyToKey key2)
         |>> (unique2 categorizingEntry (modDataKeyToKey key1)
               (categoryKeyToKey key2)
               |>> newEntryR categorizingEntry (modDataKeyToKey key1)
                    (categoryKeyToKey key2)))

--- Deletes an existing Categorizing relation between a ModData entity and a Category entity
deleteCategorizing :: ModDataKey -> CategoryKey -> Transaction ()
deleteCategorizing key1 key2 =
  minTestDelete "Categorizing" categorizingEntry keytuple2Categorizing
   categorizingModDataCategorizingKey
   1
   key1
   |>> deleteEntryR categorizingEntry (modDataKeyToKey key1)
        (categoryKeyToKey key2)

--- Gets the associated ModData entities for a given Category entity
getModDataCategorys :: ModData -> Transaction [Category]
getModDataCategorys e =
  let ekey = modDataKey e
  in getDB
      (queryCondCategorizing
        (\t -> categorizingModDataCategorizingKey t == ekey))
      |>>= (mapT getCategory . map categorizingCategoryCategorizingKey)

--- Gets all Categorizing relationship entities stored in the database.
queryAllCategorizings :: Query [Categorizing]
queryAllCategorizings =
  transformQ (map (uncurry keytuple2Categorizing))
   (allDBKeyInfos categorizingEntry)

--- Gets all Categorizing relationship entities satisfying a given condition.
queryCondCategorizing :: (Categorizing -> Bool) -> Query [Categorizing]
queryCondCategorizing econd = transformQ (filter econd) queryAllCategorizings

--- Dynamic predicate representing the ProgramInfo relation
--- between MasterProgram entities and MasterProgInfo entities.
programInfo :: MasterProgramKey -> MasterProgInfoKey -> Dynamic
programInfo key1 key2
  | masterProgInfoMasterProgramProgramInfoKey en =:= key1
  = masterProgInfoEntry (masterProgInfoKeyToKey key2)
     (masterProgInfo2tuple en)
  where
    en free

--- Dynamic predicate representing role "withProgInfo".
withProgInfo :: MasterProgramKey -> MasterProgInfoKey -> Dynamic
withProgInfo = programInfo

--- Dynamic predicate representing role "withProgInfo".
programInfoOf :: MasterProgInfoKey -> MasterProgramKey -> Dynamic
programInfoOf = flip withProgInfo

--- Dynamic predicate representing the AreaPrograms relation
--- between MasterCoreArea entities and MasterProgram entities.
areaPrograms :: MasterCoreAreaKey -> MasterProgramKey -> Dynamic
areaPrograms key1 key2
  | masterProgramMasterCoreAreaAreaProgramsKey en =:= key1
  = masterProgramEntry (masterProgramKeyToKey key2) (masterProgram2tuple en)
  where
    en free

--- Dynamic predicate representing role "withProgram".
withProgram :: MasterCoreAreaKey -> MasterProgramKey -> Dynamic
withProgram = areaPrograms

--- Dynamic predicate representing role "withProgram".
ofCoreArea :: MasterProgramKey -> MasterCoreAreaKey -> Dynamic
ofCoreArea = flip withProgram

--- Dynamic predicate representing the Advising relation
--- between User entities and MasterProgram entities.
advising :: UserKey -> MasterProgramKey -> Dynamic
advising key1 key2
  | masterProgramUserAdvisingKey en =:= key1
  = masterProgramEntry (masterProgramKeyToKey key2) (masterProgram2tuple en)
  where
    en free

--- Dynamic predicate representing role "organizes".
organizes :: UserKey -> MasterProgramKey -> Dynamic
organizes = advising

--- Dynamic predicate representing role "organizes".
organizedBy :: MasterProgramKey -> UserKey -> Dynamic
organizedBy = flip organizes

--- Dynamic predicate representing the AdvisedProgramModuleInstances relation
--- between ModInst entities and AdvisorModule entities.
advisedProgramModuleInstances :: ModInstKey -> AdvisorModuleKey -> Dynamic
advisedProgramModuleInstances key1 key2
  | advisorModuleModInstAdvisedProgramModuleInstancesKey en =:= key1
  = advisorModuleEntry (advisorModuleKeyToKey key2) (advisorModule2tuple en)
  where
    en free

--- Dynamic predicate representing role "advisorUseofModInst".
advisorUseofModInst :: ModInstKey -> AdvisorModuleKey -> Dynamic
advisorUseofModInst = advisedProgramModuleInstances

--- Dynamic predicate representing role "advisorUseofModInst".
withModInst :: AdvisorModuleKey -> ModInstKey -> Dynamic
withModInst = flip advisorUseofModInst

--- Dynamic predicate representing the AdvisorCategorizing relation
--- between Category entities and AdvisorModule entities.
advisorCategorizing :: CategoryKey -> AdvisorModuleKey -> Dynamic
advisorCategorizing key1 key2
  | advisorModuleCategoryAdvisorCategorizingKey en =:= key1
  = advisorModuleEntry (advisorModuleKeyToKey key2) (advisorModule2tuple en)
  where
    en free

--- Dynamic predicate representing role "containsAdvisorMods".
containsAdvisorMods :: CategoryKey -> AdvisorModuleKey -> Dynamic
containsAdvisorMods = advisorCategorizing

--- Dynamic predicate representing role "containsAdvisorMods".
advisedBelongsTo :: AdvisorModuleKey -> CategoryKey -> Dynamic
advisedBelongsTo = flip containsAdvisorMods

--- Dynamic predicate representing the AdvisorProgramModules relation
--- between AdvisorStudyProgram entities and AdvisorModule entities.
advisorProgramModules :: AdvisorStudyProgramKey -> AdvisorModuleKey -> Dynamic
advisorProgramModules key1 key2
  | advisorModuleAdvisorStudyProgramAdvisorProgramModulesKey en =:= key1
  = advisorModuleEntry (advisorModuleKeyToKey key2) (advisorModule2tuple en)
  where
    en free

--- Dynamic predicate representing role "moduleOfAdvisorProgram".
moduleOfAdvisorProgram
  :: AdvisorStudyProgramKey -> AdvisorModuleKey -> Dynamic
moduleOfAdvisorProgram = advisorProgramModules

--- Dynamic predicate representing role "moduleOfAdvisorProgram".
belongsToAdvisedProgram
  :: AdvisorModuleKey -> AdvisorStudyProgramKey -> Dynamic
belongsToAdvisedProgram = flip moduleOfAdvisorProgram

--- Dynamic predicate representing the StudyProgramsAdvised relation
--- between StudyProgram entities and AdvisorStudyProgram entities.
studyProgramsAdvised :: StudyProgramKey -> AdvisorStudyProgramKey -> Dynamic
studyProgramsAdvised key1 key2
  | advisorStudyProgramStudyProgramStudyProgramsAdvisedKey en =:= key1
  = advisorStudyProgramEntry (advisorStudyProgramKeyToKey key2)
     (advisorStudyProgram2tuple en)
  where
    en free

--- Dynamic predicate representing role "advisedProgram".
advisedProgram :: StudyProgramKey -> AdvisorStudyProgramKey -> Dynamic
advisedProgram = studyProgramsAdvised

--- Dynamic predicate representing role "advisedProgram".
instanceOf :: AdvisorStudyProgramKey -> StudyProgramKey -> Dynamic
instanceOf = flip advisedProgram

--- Dynamic predicate representing the StudyAdvising relation
--- between User entities and AdvisorStudyProgram entities.
studyAdvising :: UserKey -> AdvisorStudyProgramKey -> Dynamic
studyAdvising key1 key2
  | advisorStudyProgramUserStudyAdvisingKey en =:= key1
  = advisorStudyProgramEntry (advisorStudyProgramKeyToKey key2)
     (advisorStudyProgram2tuple en)
  where
    en free

--- Dynamic predicate representing role "advisesProgram".
advisesProgram :: UserKey -> AdvisorStudyProgramKey -> Dynamic
advisesProgram = studyAdvising

--- Dynamic predicate representing role "advisesProgram".
advisedBy :: AdvisorStudyProgramKey -> UserKey -> Dynamic
advisedBy = flip advisesProgram

--- Dynamic predicate representing the ModuleInstances relation
--- between ModData entities and ModInst entities.
moduleInstances :: ModDataKey -> ModInstKey -> Dynamic
moduleInstances key1 key2
  | modInstModDataModuleInstancesKey en =:= key1
  = modInstEntry (modInstKeyToKey key2) (modInst2tuple en)
  where
    en free

--- Dynamic predicate representing role "instOfModule".
instOfModule :: ModDataKey -> ModInstKey -> Dynamic
instOfModule = moduleInstances

--- Dynamic predicate representing role "instOfModule".
withModule :: ModInstKey -> ModDataKey -> Dynamic
withModule = flip instOfModule

--- Dynamic predicate representing the LecturerMods relation
--- between User entities and ModInst entities.
lecturerMods :: UserKey -> ModInstKey -> Dynamic
lecturerMods key1 key2
  | modInstUserLecturerModsKey en =:= key1
  = modInstEntry (modInstKeyToKey key2) (modInst2tuple en)
  where
    en free

--- Dynamic predicate representing role "instOfLecturer".
instOfLecturer :: UserKey -> ModInstKey -> Dynamic
instOfLecturer = lecturerMods

--- Dynamic predicate representing role "instOfLecturer".
withLecturer :: ModInstKey -> UserKey -> Dynamic
withLecturer = flip instOfLecturer

--- Dynamic predicate representing the DataDesc relation
--- between ModData entities and ModDescr entities.
dataDesc :: ModDataKey -> ModDescrKey -> Dynamic
dataDesc key1 key2
  | modDescrModDataDataDescKey en =:= key1
  = modDescrEntry (modDescrKeyToKey key2) (modDescr2tuple en)
  where
    en free

--- Dynamic predicate representing role "withDesc".
withDesc :: ModDataKey -> ModDescrKey -> Dynamic
withDesc = dataDesc

--- Dynamic predicate representing role "withDesc".
descOf :: ModDescrKey -> ModDataKey -> Dynamic
descOf = flip withDesc

--- Dynamic predicate representing role "belongsTo".
belongsTo :: ModDataKey -> CategoryKey -> Dynamic
belongsTo = categorizing

--- Dynamic predicate representing role "contains".
contains :: CategoryKey -> ModDataKey -> Dynamic
contains = flip categorizing

--- Dynamic predicate representing the Responsible relation
--- between User entities and ModData entities.
responsible :: UserKey -> ModDataKey -> Dynamic
responsible key1 key2
  | modDataUserResponsibleKey en =:= key1
  = modDataEntry (modDataKeyToKey key2) (modData2tuple en)
  where
    en free

--- Dynamic predicate representing role "responsibleFor".
responsibleFor :: UserKey -> ModDataKey -> Dynamic
responsibleFor = responsible

--- Dynamic predicate representing role "responsibleFor".
managedBy :: ModDataKey -> UserKey -> Dynamic
managedBy = flip responsibleFor

--- Dynamic predicate representing the ProgramCategories relation
--- between StudyProgram entities and Category entities.
programCategories :: StudyProgramKey -> CategoryKey -> Dynamic
programCategories key1 key2
  | categoryStudyProgramProgramCategoriesKey en =:= key1
  = categoryEntry (categoryKeyToKey key2) (category2tuple en)
  where
    en free

--- Dynamic predicate representing role "withCategory".
withCategory :: StudyProgramKey -> CategoryKey -> Dynamic
withCategory = programCategories

--- Dynamic predicate representing role "withCategory".
ofProgram :: CategoryKey -> StudyProgramKey -> Dynamic
ofProgram = flip withCategory

--- Checks the consistency of the complete database.
checkAllData :: Transaction ()
checkAllData =
  checkCategorizing
   |>> (checkStudyProgram
         |>> (checkCategory
               |>> (checkMasterCoreArea
                     |>> (checkUser
                           |>> (checkModData
                                 |>> (checkModDescr
                                       |>> (checkModInst
                                             |>> (checkAdvisorStudyProgram
                                                   |>> (checkAdvisorModule
                                                         |>> (checkMasterProgram
                                                               |>> (checkMasterProgInfo
                                                                     |>> checkUnivisInfo)))))))))))

--- Checks the consistency of the database for Categorizing entities.
checkCategorizing :: Transaction ()
checkCategorizing =
  getDB (allDBKeyInfos categorizingEntry)
   |>>= (mapT_ checkCategorizingEntry . map (uncurry keytuple2Categorizing))

--- Checks the consistency of the database for StudyProgram entities.
checkStudyProgram :: Transaction ()
checkStudyProgram =
  getDB (allDBKeyInfos studyProgramEntry)
   |>>= (mapT_ checkStudyProgramEntry . map (uncurry keytuple2StudyProgram))

--- Checks the consistency of the database for Category entities.
checkCategory :: Transaction ()
checkCategory =
  getDB (allDBKeyInfos categoryEntry)
   |>>= (mapT_ checkCategoryEntry . map (uncurry keytuple2Category))

--- Checks the consistency of the database for MasterCoreArea entities.
checkMasterCoreArea :: Transaction ()
checkMasterCoreArea =
  getDB (allDBKeyInfos masterCoreAreaEntry)
   |>>= (mapT_ checkMasterCoreAreaEntry
          . map (uncurry keytuple2MasterCoreArea))

--- Checks the consistency of the database for User entities.
checkUser :: Transaction ()
checkUser =
  getDB (allDBKeyInfos userEntry)
   |>>= (mapT_ checkUserEntry . map (uncurry keytuple2User))

--- Checks the consistency of the database for ModData entities.
checkModData :: Transaction ()
checkModData =
  getDB (allDBKeyInfos modDataEntry)
   |>>= (mapT_ checkModDataEntry . map (uncurry keytuple2ModData))

--- Checks the consistency of the database for ModDescr entities.
checkModDescr :: Transaction ()
checkModDescr =
  getDB (allDBKeyInfos modDescrEntry)
   |>>= (mapT_ checkModDescrEntry . map (uncurry keytuple2ModDescr))

--- Checks the consistency of the database for ModInst entities.
checkModInst :: Transaction ()
checkModInst =
  getDB (allDBKeyInfos modInstEntry)
   |>>= (mapT_ checkModInstEntry . map (uncurry keytuple2ModInst))

--- Checks the consistency of the database for AdvisorStudyProgram entities.
checkAdvisorStudyProgram :: Transaction ()
checkAdvisorStudyProgram =
  getDB (allDBKeyInfos advisorStudyProgramEntry)
   |>>= (mapT_ checkAdvisorStudyProgramEntry
          . map (uncurry keytuple2AdvisorStudyProgram))

--- Checks the consistency of the database for AdvisorModule entities.
checkAdvisorModule :: Transaction ()
checkAdvisorModule =
  getDB (allDBKeyInfos advisorModuleEntry)
   |>>= (mapT_ checkAdvisorModuleEntry . map (uncurry keytuple2AdvisorModule))

--- Checks the consistency of the database for MasterProgram entities.
checkMasterProgram :: Transaction ()
checkMasterProgram =
  getDB (allDBKeyInfos masterProgramEntry)
   |>>= (mapT_ checkMasterProgramEntry . map (uncurry keytuple2MasterProgram))

--- Checks the consistency of the database for MasterProgInfo entities.
checkMasterProgInfo :: Transaction ()
checkMasterProgInfo =
  getDB (allDBKeyInfos masterProgInfoEntry)
   |>>= (mapT_ checkMasterProgInfoEntry
          . map (uncurry keytuple2MasterProgInfo))

--- Checks the consistency of the database for UnivisInfo entities.
checkUnivisInfo :: Transaction ()
checkUnivisInfo =
  getDB (allDBKeyInfos univisInfoEntry)
   |>>= (mapT_ checkUnivisInfoEntry . map (uncurry keytuple2UnivisInfo))

checkCategorizingEntry :: Categorizing -> Transaction ()
checkCategorizingEntry categorizing_p =
  existsEntryWithDBKey "ModData" modDataEntry
   (modDataKeyToKey (categorizingModDataCategorizingKey categorizing_p))
   |>> (existsEntryWithDBKey "Category" categoryEntry
         (categoryKeyToKey
           (categorizingCategoryCategorizingKey categorizing_p))
         |>> unique2C categorizingEntry
              (modDataKeyToKey
                (categorizingModDataCategorizingKey categorizing_p))
              (categoryKeyToKey
                (categorizingCategoryCategorizingKey categorizing_p)))

checkStudyProgramEntry :: StudyProgram -> Transaction ()
checkStudyProgramEntry studyProgram_p =
  duplicateKeyTest studyProgramEntry
   |>> (uniqueC "MDB" studyProgramEntry keytuple2StudyProgram
         studyProgramShortName
         studyProgram_p
         |>> uniqueC "MDB" studyProgramEntry keytuple2StudyProgram
              studyProgramProgKey
              studyProgram_p)

checkCategoryEntry :: Category -> Transaction ()
checkCategoryEntry category_p =
  duplicateKeyTest categoryEntry
   |>> existsEntryWithDBKey "StudyProgram" studyProgramEntry
        (studyProgramKeyToKey
          (categoryStudyProgramProgramCategoriesKey category_p))

checkMasterCoreAreaEntry :: MasterCoreArea -> Transaction ()
checkMasterCoreAreaEntry masterCoreArea_p =
  duplicateKeyTest masterCoreAreaEntry
   |>> uniqueC "MDB" masterCoreAreaEntry keytuple2MasterCoreArea
        masterCoreAreaAreaKey
        masterCoreArea_p

checkUserEntry :: User -> Transaction ()
checkUserEntry user_p =
  duplicateKeyTest userEntry
   |>> uniqueC "MDB" userEntry keytuple2User userLogin user_p

checkModDataEntry :: ModData -> Transaction ()
checkModDataEntry modData_p =
  duplicateKeyTest modDataEntry
   |>> (uniqueC "MDB" modDataEntry keytuple2ModData modDataCode modData_p
         |>> (existsEntryWithDBKey "User" userEntry
               (userKeyToKey (modDataUserResponsibleKey modData_p))
               |>> minTestC "Categorizing" categorizingEntry
                    keytuple2Categorizing
                    categorizingModDataCategorizingKey
                    1
                    (modDataKey modData_p)))

checkModDescrEntry :: ModDescr -> Transaction ()
checkModDescrEntry modDescr_p =
  duplicateKeyTest modDescrEntry
   |>> (uniqueC "MDB" modDescrEntry keytuple2ModDescr
         modDescrModDataDataDescKey
         modDescr_p
         |>> existsEntryWithDBKey "ModData" modDataEntry
              (modDataKeyToKey (modDescrModDataDataDescKey modDescr_p)))

checkModInstEntry :: ModInst -> Transaction ()
checkModInstEntry modInst_p =
  duplicateKeyTest modInstEntry
   |>> (existsEntryWithDBKey "User" userEntry
         (userKeyToKey (modInstUserLecturerModsKey modInst_p))
         |>> existsEntryWithDBKey "ModData" modDataEntry
              (modDataKeyToKey (modInstModDataModuleInstancesKey modInst_p)))

checkAdvisorStudyProgramEntry :: AdvisorStudyProgram -> Transaction ()
checkAdvisorStudyProgramEntry advisorStudyProgram_p =
  duplicateKeyTest advisorStudyProgramEntry
   |>> (existsEntryWithDBKey "User" userEntry
         (userKeyToKey
           (advisorStudyProgramUserStudyAdvisingKey advisorStudyProgram_p))
         |>> existsEntryWithDBKey "StudyProgram" studyProgramEntry
              (studyProgramKeyToKey
                (advisorStudyProgramStudyProgramStudyProgramsAdvisedKey
                  advisorStudyProgram_p)))

checkAdvisorModuleEntry :: AdvisorModule -> Transaction ()
checkAdvisorModuleEntry advisorModule_p =
  duplicateKeyTest advisorModuleEntry
   |>> (existsEntryWithDBKey "AdvisorStudyProgram" advisorStudyProgramEntry
         (advisorStudyProgramKeyToKey
           (advisorModuleAdvisorStudyProgramAdvisorProgramModulesKey
             advisorModule_p))
         |>> (existsEntryWithDBKey "Category" categoryEntry
               (categoryKeyToKey
                 (advisorModuleCategoryAdvisorCategorizingKey
                   advisorModule_p))
               |>> existsEntryWithDBKey "ModInst" modInstEntry
                    (modInstKeyToKey
                      (advisorModuleModInstAdvisedProgramModuleInstancesKey
                        advisorModule_p))))

checkMasterProgramEntry :: MasterProgram -> Transaction ()
checkMasterProgramEntry masterProgram_p =
  duplicateKeyTest masterProgramEntry
   |>> (existsEntryWithDBKey "User" userEntry
         (userKeyToKey (masterProgramUserAdvisingKey masterProgram_p))
         |>> existsEntryWithDBKey "MasterCoreArea" masterCoreAreaEntry
              (masterCoreAreaKeyToKey
                (masterProgramMasterCoreAreaAreaProgramsKey masterProgram_p)))

checkMasterProgInfoEntry :: MasterProgInfo -> Transaction ()
checkMasterProgInfoEntry masterProgInfo_p =
  duplicateKeyTest masterProgInfoEntry
   |>> existsEntryWithDBKey "MasterProgram" masterProgramEntry
        (masterProgramKeyToKey
          (masterProgInfoMasterProgramProgramInfoKey masterProgInfo_p))

checkUnivisInfoEntry :: UnivisInfo -> Transaction ()
checkUnivisInfoEntry _ = duplicateKeyTest univisInfoEntry

--- Saves the complete database as Curry terms.
--- The first argument is the directory where the term files should be stored.
saveAllData :: String -> IO ()
saveAllData path =
  do saveDBTerms path "StudyProgram" studyProgramEntry keytuple2StudyProgram
     saveDBTerms path "Category" categoryEntry keytuple2Category
     saveDBTerms path "MasterCoreArea" masterCoreAreaEntry
      keytuple2MasterCoreArea
     saveDBTerms path "User" userEntry keytuple2User
     saveDBTerms path "ModData" modDataEntry keytuple2ModData
     saveDBTerms path "ModDescr" modDescrEntry keytuple2ModDescr
     saveDBTerms path "ModInst" modInstEntry keytuple2ModInst
     saveDBTerms path "AdvisorStudyProgram" advisorStudyProgramEntry
      keytuple2AdvisorStudyProgram
     saveDBTerms path "AdvisorModule" advisorModuleEntry
      keytuple2AdvisorModule
     saveDBTerms path "MasterProgram" masterProgramEntry
      keytuple2MasterProgram
     saveDBTerms path "MasterProgInfo" masterProgInfoEntry
      keytuple2MasterProgInfo
     saveDBTerms path "UnivisInfo" univisInfoEntry keytuple2UnivisInfo
     saveDBTerms path "Categorizing" categorizingEntry keytuple2Categorizing

--- Restore the complete database from files containing Curry terms.
--- The first argument is the directory where the term files are stored.
restoreAllData :: String -> IO ()
restoreAllData path =
  do restoreDBTerms path "StudyProgram" studyProgramEntry
      (studyProgramKeyToKey . studyProgramKey)
      studyProgram2tuple
     restoreDBTerms path "Category" categoryEntry
      (categoryKeyToKey . categoryKey)
      category2tuple
     restoreDBTerms path "MasterCoreArea" masterCoreAreaEntry
      (masterCoreAreaKeyToKey . masterCoreAreaKey)
      masterCoreArea2tuple
     restoreDBTerms path "User" userEntry (userKeyToKey . userKey) user2tuple
     restoreDBTerms path "ModData" modDataEntry (modDataKeyToKey . modDataKey)
      modData2tuple
     restoreDBTerms path "ModDescr" modDescrEntry
      (modDescrKeyToKey . modDescrKey)
      modDescr2tuple
     restoreDBTerms path "ModInst" modInstEntry (modInstKeyToKey . modInstKey)
      modInst2tuple
     restoreDBTerms path "AdvisorStudyProgram" advisorStudyProgramEntry
      (advisorStudyProgramKeyToKey . advisorStudyProgramKey)
      advisorStudyProgram2tuple
     restoreDBTerms path "AdvisorModule" advisorModuleEntry
      (advisorModuleKeyToKey . advisorModuleKey)
      advisorModule2tuple
     restoreDBTerms path "MasterProgram" masterProgramEntry
      (masterProgramKeyToKey . masterProgramKey)
      masterProgram2tuple
     restoreDBTerms path "MasterProgInfo" masterProgInfoEntry
      (masterProgInfoKeyToKey . masterProgInfoKey)
      masterProgInfo2tuple
     restoreDBTerms path "UnivisInfo" univisInfoEntry
      (univisInfoKeyToKey . univisInfoKey)
      univisInfo2tuple
     restoreDBRelTerms path "Categorizing" categorizingEntry
      categorizing2tuple

-----------------------------------------------------------------------
-- Some extensions to the generated MDB code:


-- store DBs in term files:
storeTermDB :: IO ()
storeTermDB = saveAllData storageDir

-- initialize DBs from term files:
readTermDB :: IO ()
readTermDB = restoreAllData storageDir

-----------------------------------------------------------------------
--- Gets the pair of term and year of a ModInst entity.
modInstSemester :: ModInst -> (String,Int)
modInstSemester (ModInst _ t y _ _) = (t,y)

-----------------------------------------------------------------------
--- Database table for ModData entities.
modData'Table :: DBTable ModData
modData'Table = dbTable modDataEntry keytuple2ModData

--- Attribute Key of entity ModData.
modData'Key :: DBAttr ModData ModDataKey
modData'Key = dbKeyAttr modData'Table (-1) modDataKeyToKey ModDataKey

--- Attribute Code of entity ModData.
modData'Code :: DBAttr ModData String
modData'Code = dbAttr modData'Table 0

--- Attribute NameG of entity ModData.
modData'NameG :: DBAttr ModData String
modData'NameG = dbAttr modData'Table 1

--- Attribute NameE of entity ModData.
modData'NameE :: DBAttr ModData String
modData'NameE = dbAttr modData'Table 2

--- Attribute UserResponsibleKey of entity ModData.
modData'UserResponsibleKey :: DBAttr ModData UserKey
modData'UserResponsibleKey = dbKeyAttr modData'Table 10 userKeyToKey UserKey

--- Gets all ModData entities with a given module code.
queryModDataWithCode :: String -> Query [ModData]
queryModDataWithCode mcode =
  selectFrom modData'Table `whereQ` modData'Code @== mcode

--- Gets all ModData entities of a user.
queryModDataOfUser :: UserKey -> Query [ModData]
queryModDataOfUser ukey =
  selectFrom modData'Table `whereQ` modData'UserResponsibleKey @== ukey

--- Query the key/code/name of all ModData entities.
queryModDataCodeName :: Query [(ModDataKey,String,String,String)]
queryModDataCodeName =
  selectAll4 modData'Key modData'Code modData'NameG modData'NameE

-----------------------------------------------------------------------
--- Database table for ModDescr entities.
modDescr'Table :: DBTable ModDescr
modDescr'Table = dbTable modDescrEntry keytuple2ModDescr

--- Attribute Exam of entity ModDescr.
modDescr'Exam :: DBAttr ModDescr String
modDescr'Exam = dbAttr modDescr'Table 5

--- Attribute ModDataDataDescKey of entity ModDescr.
modDescr'ModDataDataDescKey :: DBAttr ModDescr ModDataKey
modDescr'ModDataDataDescKey =
  dbKeyAttr modDescr'Table 11 modDataKeyToKey ModDataKey

--- Gets the ModDescr entity associated to a given ModData key.
queryDescriptionOfMod :: ModDataKey -> Query (Maybe ModDescr)
queryDescriptionOfMod mdk =
  transformQ
   (\l -> if null l then Nothing else Just (head l))
   (selectFrom modDescr'Table `whereQ` modDescr'ModDataDataDescKey @== mdk)

--- Gets the Exam attribute associated to a given ModData key.
queryExamOfMod :: ModDataKey -> Query (Maybe String)
queryExamOfMod mdk =
  transformQ
   (\l -> if null l then Nothing else Just (head l))
   (select1 modDescr'Exam `whereQ` modDescr'ModDataDataDescKey @== mdk)

-----------------------------------------------------------------------
--- Database table for ModData entities.
modInst'Table :: DBTable ModInst
modInst'Table = dbTable modInstEntry keytuple2ModInst

--- Attribute Code of entity ModData.
modInst'Term :: DBAttr ModInst String
modInst'Term = dbAttr modInst'Table 0

--- Attribute Code of entity ModData.
modInst'Year :: DBAttr ModInst Int
modInst'Year = dbAttr modInst'Table 1

--- Attribute Code of entity ModData.
modInst'ModDataModuleInstancesKey :: DBAttr ModInst ModDataKey
modInst'ModDataModuleInstancesKey =
 dbKeyAttr modInst'Table 3 modDataKeyToKey ModDataKey

--- Gets all module instances for a given module (key).
queryInstancesOfMod :: ModDataKey -> Query [ModInst]
queryInstancesOfMod mdk =
  selectFrom modInst'Table `whereQ` modInst'ModDataModuleInstancesKey @== mdk

--- Gets the ModData keys of all module instances in a given semester.
queryModKeysOfSem :: (String,Int) -> Query [ModDataKey]
queryModKeysOfSem (term,year) =
  select1 modInst'ModDataModuleInstancesKey
    `whereQ` modInst'Term @== term @&& modInst'Year @== year

-----------------------------------------------------------------------
--- Shows the key of a MasterProgram entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
masterProgramKeyToString :: MasterProgramKey -> String
masterProgramKeyToString key =
  ERDGeneric.showDatabaseKey "MasterProgram" masterProgramKeyToKey key

-----------------------------------------------------------------------
--- Database table for MasterProgram entities.
masterProgram'Table :: DBTable MasterProgram
masterProgram'Table = dbTable masterProgramEntry keytuple2MasterProgram

--- Attribute Key of entity ModData.
masterProgram'Key :: DBAttr MasterProgram MasterProgramKey
masterProgram'Key = dbKeyAttr masterProgram'Table (-1) masterProgramKeyToKey
                              MasterProgramKey

--- Attribute Code of entity ModData.
masterProgram'Name :: DBAttr MasterProgram String
masterProgram'Name = dbAttr masterProgram'Table 0

--- Attribute Code of entity ModData.
masterProgram'Term :: DBAttr MasterProgram String
masterProgram'Term = dbAttr masterProgram'Table 1

--- Attribute Code of entity ModData.
masterProgram'Year :: DBAttr MasterProgram Int
masterProgram'Year = dbAttr masterProgram'Table 2

--- Attribute Code of entity ModData.
masterProgram'Visible :: DBAttr MasterProgram Bool
masterProgram'Visible = dbAttr masterProgram'Table 6

--- Attribute UserResponsibleKey of entity ModData.
masterProgram'UserAdvisingKey :: DBAttr MasterProgram UserKey
masterProgram'UserAdvisingKey =
  dbKeyAttr masterProgram'Table 7 userKeyToKey UserKey

--- Attribute UserResponsibleKey of entity ModData.
masterProgram'MasterCoreAreaAreaProgramsKey
  :: DBAttr MasterProgram MasterCoreAreaKey
masterProgram'MasterCoreAreaAreaProgramsKey =
  dbKeyAttr masterProgram'Table 8 masterCoreAreaKeyToKey MasterCoreAreaKey

queryMasterProgramMainInfos
  :: Query [(MasterProgramKey,String,String,Int,Bool,MasterCoreAreaKey)]
queryMasterProgramMainInfos =
  selectAll6 masterProgram'Key masterProgram'Name masterProgram'Term
             masterProgram'Year masterProgram'Visible
             masterProgram'MasterCoreAreaAreaProgramsKey

--- Gets all MasterProgram entities belonging to a user.
queryMasterProgramOfUser :: UserKey -> Query [MasterProgram]
queryMasterProgramOfUser ukey =
  selectFrom masterProgram'Table `whereQ` masterProgram'UserAdvisingKey @== ukey

--- Gets all MasterProgram (keys) for each ModInst of a given ModInst list.
getMasterProgramKeysOfModInst :: [ModInst] -> Query [[MasterProgramKey]]
getMasterProgramKeysOfModInst mis =
  transformQ
   (\mpis ->
     map (\mi -> let mdk = modInstModDataModuleInstancesKey mi in
           map snd
               (filter (\mpi ->
                         any (\ (_,_,smpk,trm,yr) ->
                               readModDataKey smpk == Just mdk &&
                               modInstTerm mi == trm && modInstYear mi == yr)
                             (readProgModules (fst mpi)))
                       mpis))
         mis)
   (selectAll2 masterProgInfo'ProgModules
               masterProgInfo'MasterProgramProgramInfoKey)

-- to avoid typing problem with kics2
readProgModules :: String -> [(String,Bool,String,String,Int)]
readProgModules s = readQTerm s

-----------------------------------------------------------------------
--- Database table for MasterProgram entities.
masterProgInfo'Table :: DBTable MasterProgInfo
masterProgInfo'Table = dbTable masterProgInfoEntry keytuple2MasterProgInfo

--- Attribute Code of entity ModData.
masterProgInfo'ProgModules :: DBAttr MasterProgInfo String
masterProgInfo'ProgModules = dbAttr masterProgInfo'Table 0

--- Attribute Code of entity ModData.
masterProgInfo'MasterProgramProgramInfoKey
  :: DBAttr MasterProgInfo MasterProgramKey
masterProgInfo'MasterProgramProgramInfoKey =
  dbKeyAttr masterProgInfo'Table 6 masterProgramKeyToKey MasterProgramKey

--- Gets the MasterProgInfo entity associated to a MasterProgram key.
queryInfoOfMasterProgram :: MasterProgramKey
                         -> Query (Maybe MasterProgInfo)
queryInfoOfMasterProgram mpk =
  transformQ
   (\l -> if null l then Nothing else Just (head l))
   (selectFrom masterProgInfo'Table
      `whereQ` masterProgInfo'MasterProgramProgramInfoKey @== mpk)
{-
  transformQ
   ((\l -> if null l then Nothing else Just (head l))
      . map (uncurry keytuple2MasterProgInfo))
   (KeyDatabase.someDBKeyInfos masterProgInfoEntry
                               [6 @= masterProgramKeyToKey mpk])
-}

-----------------------------------------------------------------------
--- Database table for UnivisInfo entities.
univisInfo'Table :: DBTable UnivisInfo
univisInfo'Table = dbTable univisInfoEntry keytuple2UnivisInfo

--- Query condition for equality of attribute Code of entity UnivisInfo.
univisInfo'Code :: DBAttr UnivisInfo String
univisInfo'Code = dbAttr univisInfo'Table 0

--- Query condition for equality of attribute Term of entity UnivisInfo.
univisInfo'Term :: DBAttr UnivisInfo String
univisInfo'Term = dbAttr univisInfo'Table 1

--- Attribute Term of entity UnivisInfo.
univisInfo'Year :: DBAttr UnivisInfo Int
univisInfo'Year = dbAttr univisInfo'Table 2

--- query the univis URLs for a module in a semester:
queryUnivisURL :: String -> (String,Int) -> Query [String]
queryUnivisURL mcode (term,year) = transformQ (map univisInfoURL) $
  selectFrom univisInfo'Table
   `whereQ` univisInfo'Code @== mcode @&&
            univisInfo'Term @== term @&&
            univisInfo'Year @== year

--- query whether a module has a UnivIS instance in a semester:
queryHasUnivisEntry :: String -> (String,Int) -> Query Bool
queryHasUnivisEntry mcode (term,year) = transformQ (not . null) $
  selectFrom univisInfo'Table
   `whereQ` univisInfo'Code @== mcode @&&
            univisInfo'Term @== term @&&
            univisInfo'Year @== year

-----------------------------------------------------------------------
--- Destroy an existing Categorizing relation between a ModData entity
--- and a Category entity without ensuring the minimal constraint.
--- This can be used instead of `deleteCategorizing` if the corresponding
--- module is also deleted.
destroyCategorizing :: ModDataKey -> CategoryKey -> Transaction ()
destroyCategorizing key1 key2 =
  ERDGeneric.deleteEntryR categorizingEntry (modDataKeyToKey key1)
    (categoryKeyToKey key2)

--- Gets all categories for a given module (key).
queryModDataKeyCategorys :: ModDataKey -> Query [Categorizing]
queryModDataKeyCategorys mdk =
  transformQ
   (map (uncurry keytuple2Categorizing))
   (KeyDatabase.someDBKeyInfos categorizingEntry [0 @= modDataKeyToKey mdk])

--- Gets all categories for a given module (key).
queryModDataKeysOfCategory :: CategoryKey -> Query [ModDataKey]
queryModDataKeysOfCategory ck =
  transformQ
   (map (categorizingModDataCategorizingKey . uncurry keytuple2Categorizing))
   (KeyDatabase.someDBKeyInfos categorizingEntry [1 @= categoryKeyToKey ck])

--- Gets the associated Category entities for a given ModData entity
getModDataCategories :: ModData -> Transaction [Category]
getModDataCategories e = getModDataKeyCategorys (modDataKey e)

--- Gets the associated Category entities for a given ModDataKey
getModDataKeyCategorys :: ModDataKey -> Transaction [Category]
getModDataKeyCategorys mdk =
  getDB (queryModDataKeyCategorys mdk) |>>=
  mapT getCategory . map categorizingCategoryCategorizingKey

-----------------------------------------------------------------------
-- Tests for better querying

ex1 = runQ (selectAll1 modData'Key)
        >>= putStrLn . unlines . map show

ex2 = runQ (selectAll3 modData'Key modData'Code modData'NameG)
        >>= putStrLn . unlines . map show

ex3 = runQ (selectAll2 modData'Code modData'UserResponsibleKey)
        >>= putStrLn . unlines . map show

ex4 = runQ (selectAll2 modData'Key modData'Code)
        >>= putStrLn . unlines . map show

ex5 = runQ (select1 modData'NameG `whereQ` modData'Code @=="Inf-Prog")

ex5' = runQ (selectFrom modData'Table `whereQ` modData'Code @=="Inf-Prog")

ex6 = runQ (select2 modData'NameG modData'UserResponsibleKey
              `whereQ` modData'Code @=="Inf-Prog")

ex7 = runQ queryAllMasterPrograms
ex8 = runQ (selectAll1 masterProgram'UserAdvisingKey)
ex9 = runQ (selectAll6 masterProgram'Key masterProgram'Name masterProgram'Term
                       masterProgram'Year masterProgram'Visible
                       --masterProgram'UserAdvisingKey
                       masterProgram'MasterCoreAreaAreaProgramsKey)

------------------------------------------------------------------------
