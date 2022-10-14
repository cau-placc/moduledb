module View.MDBEntitiesToHtml where

import HTML.WUI
import HTML.Base
import HTML.Styles.Bootstrap4
import Data.Time
import System.Spicey
import MDB
import System.Helpers
import Data.List
import System.MultiLang
import System.SessionInfo

--- Shows a reference to a study program with its title according
--- to the current language.
studyProgramToHRef :: HTML h => UserSessionInfo -> StudyProgram -> h
studyProgramToHRef sinfo sprog =
  href ("?Category/studyprogram/" ++ showStudyProgramKey sprog)
       [textstyle "studyprogram"
               ((langSelect sinfo studyProgramNameE studyProgramName) sprog)]

--- The list view of a StudyProgram entity in HTML format.
--- This view is used in a row of a table of all entities.
studyProgramToListView :: HTML h => UserSessionInfo -> StudyProgram -> [[h]]
studyProgramToListView sinfo sprog =
  [name2href ((langSelect sinfo studyProgramNameE studyProgramName) sprog)
  ,[stringToHtml (studyProgramShortName sprog)]
  ,[stringToHtml (studyProgramProgKey sprog)]
  ,[intToHtml (studyProgramPosition sprog)]]
 where
  name2href n =
    [hrefStudyProgram
       ("?Category/studyprogram/" ++ showStudyProgramKey sprog)
       [textstyle "studyprogram" n]]

--- The short view of a StudyProgram entity as a string.
--- This view is used in menus and comments to refer to a StudyProgram entity.
studyProgramToShortView :: StudyProgram -> String
studyProgramToShortView studyProgram = studyProgramShortName studyProgram

--- The detailed view of a StudyProgram entity in HTML format.
studyProgramToDetailsView :: HTML h => StudyProgram -> [h]
studyProgramToDetailsView studyProgram =
  [spTable
    (map (\(label,value) -> [label,value])
      (zip studyProgramLabelList detailedView))]
  where
    detailedView =
      [[stringToHtml (studyProgramName studyProgram)]
      ,[stringToHtml (studyProgramNameE studyProgram)]
      ,[stringToHtml (studyProgramShortName studyProgram)]
      ,[stringToHtml (studyProgramProgKey studyProgram)]
      ,[intToHtml (studyProgramPosition studyProgram)]]

--- The labels of a StudyProgram entity, as used in HTML tables.
studyProgramLabelList :: HTML h => [[h]]
studyProgramLabelList =
  [[textstyle "spicey_label spicey_label_for_type_string" "Name"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "English name"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "ShortName"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "ProgKey (to identify programs)"]
  ,[textstyle "spicey_label spicey_label_for_type_int" "Position"]]

--- The list view of a Category entity in HTML format.
--- This view is used in a row of a table of all entities.
categoryToListView :: HTML h => UserSessionInfo -> Category -> [[h]]
categoryToListView sinfo category =
  [name2href ((langSelect sinfo categoryNameE categoryName) category)
  ,[stringToHtml (categoryShortName category)]
  ,[intToHtml (categoryPosition category)]]
 where
   name2href n = [hrefCategory ("?Category/show/" ++ showCategoryKey category)
                               [stringToHtml n]]

--- The HTML view of a Category entity.
categoryToHtmlView :: HTML h => Category -> h
categoryToHtmlView category =
  smallHrefCategory ("?Category/show/" ++ showCategoryKey category)
    [htxt (categoryShortName category)]

--- The short view of a Category entity as a string.
--- This view is used in menus and comments to refer to a Category entity.
categoryToShortView :: Category -> String
categoryToShortView category = categoryShortName category

--- The detailed view of a Category entity in HTML format.
--- It also takes associated entities for every associated entity type.
categoryToDetailsView :: HTML h => Category -> StudyProgram -> [h]
categoryToDetailsView category relatedStudyProgram =
  [spTable
    (map (\(label,value) -> [label,value])
      (zip categoryLabelList detailedView))]
  where
    detailedView =
      [[stringToHtml (categoryName category)]
      ,[stringToHtml (categoryNameE category)]
      ,[stringToHtml (categoryShortName category)]
      ,[intToHtml (categoryPosition category)]
      ,[htxt (studyProgramToShortView relatedStudyProgram)]]

--- The labels of a Category entity, as used in HTML tables.
categoryLabelList :: HTML h => [[h]]
categoryLabelList =
  [[textstyle "spicey_label spicey_label_for_type_string" "Deutscher Name"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Englischer Name"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Abkürzung"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Kommentar", markdownRef]
  ,[textstyle "spicey_label spicey_label_for_type_int" "Minimale ECTS"]
  ,[textstyle "spicey_label spicey_label_for_type_int" "Maximale ECTS"]
  ,[textstyle "spicey_label spicey_label_for_type_int" "Position"]
  ,[textstyle "spicey_label spicey_label_for_type_relation" "Studiengang"]]

--- The list view of a MasterCoreArea entity in HTML format.
--- This view is used in a row of a table of all entities.
masterCoreAreaToListView :: HTML h => MasterCoreArea -> [[h]]
masterCoreAreaToListView masterCoreArea =
  [[stringToHtml (masterCoreAreaName masterCoreArea)]
  ,[stringToHtml (masterCoreAreaShortName masterCoreArea)]
  ,[stringToHtml (masterCoreAreaDescription masterCoreArea)]
  ,[stringToHtml (masterCoreAreaAreaKey masterCoreArea)]
  ,[intToHtml (masterCoreAreaPosition masterCoreArea)]]

--- The short view of a MasterCoreArea entity as a string.
--- This view is used in menus and comments to refer to a MasterCoreArea entity.
masterCoreAreaToShortView :: MasterCoreArea -> String
masterCoreAreaToShortView masterCoreArea =
  masterCoreAreaName masterCoreArea

--- The detailed view of a MasterCoreArea entity in HTML format.
masterCoreAreaToDetailsView :: HTML h => MasterCoreArea -> [h]
masterCoreAreaToDetailsView masterCoreArea =
  [spTable
    (map (\(label,value) -> [label,value])
      (zip masterCoreAreaLabelList detailedView))]
  where
    detailedView =
      [[stringToHtml (masterCoreAreaName masterCoreArea)]
      ,[stringToHtml (masterCoreAreaShortName masterCoreArea)]
      ,[stringToHtml (masterCoreAreaDescription masterCoreArea)]
      ,[stringToHtml (masterCoreAreaAreaKey masterCoreArea)]
      ,[intToHtml (masterCoreAreaPosition masterCoreArea)]]

--- The labels of a MasterCoreArea entity, as used in HTML tables.
masterCoreAreaLabelList :: HTML h => [[h]]
masterCoreAreaLabelList =
  [[textstyle "spicey_label spicey_label_for_type_string" "Name"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "ShortName"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Description", markdownRef]
  ,[textstyle "spicey_label spicey_label_for_type_string" "AreaKey"]
  ,[textstyle "spicey_label spicey_label_for_type_int" "Position"]]

--- The list view of a User entity in HTML format.
--- This view is used in a row of a table of all entities.
userToListView :: HTML h => User -> [[h]]
userToListView user =
  [[stringToHtml (userLogin user)]
  ,[stringToHtml (userName user)]
  ,[stringToHtml (userFirst user)]
  ,[dateToHtml (userLastLogin user)]]

--- The HTML view of a User entity.
userToHtmlView :: HTML h => User -> h
userToHtmlView user =
  let name = userToShortView user
   in if null (userUrl user)
      then htxt name
      else ehref (userUrl user) [htxt name]

--- The short view of a User entity as a string.
--- This view is used in menus and comments to refer to a User entity.
userToShortView :: User -> String
userToShortView user =
  let ut = userTitle user
   in if null ut then userFirst user ++ " " ++ userName user
                 else ut ++ " " ++ userFirst user ++ " " ++ userName user

--- The detailed view of a User entity in HTML format.
userToDetailsView :: HTML h => User -> [h]
userToDetailsView user =
  [spTable
    (map (\(label,value) -> [label,value]) (zip userLabelList detailedView))]
  where
    detailedView =
      [[stringToHtml (userLogin user)]
      ,[stringToHtml (userName user)]
      ,[stringToHtml (userFirst user)]
      ,[stringToHtml (userTitle user)]
      ,[stringToHtml (userEmail user)]
      ,[stringToHtml (userUrl user)]
      ,[stringToHtml (userPassword user)]
      ,[dateToHtml (userLastLogin user)]]

--- The labels of a User entity, as used in HTML tables.
userLabelList :: HTML h => [[h]]
userLabelList =
  [[textstyle "spicey_label spicey_label_for_type_string" "Login"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Name"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "First"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Title"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Email"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Url"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Password"]
  ,[textstyle "spicey_label spicey_label_for_type_calendarTime" "LastLogin"]]

--- The list view of a ModData entity in HTML format.
--- This view is used in a row of a table of all entities.
modDataToListView :: HTML h => ModData -> [[h]]
modDataToListView modData =
  [[withHref (stringToHtml (modDataCode modData))],
   [withHref (stringToHtml (modDataNameG modData))],
   [stringToHtml (showDiv10 (modDataECTS modData))]]
 where
   withHref hexp =
     let txtelem = [if modDataVisible modData then hexp else italic [hexp]]
      in  if null (modDataURL modData)
          then hrefModule ("?ModData/show/" ++ showModDataKey modData) txtelem
          else hrefExtModule (modDataURL modData) txtelem

--- A more compact list view of a ModData entity in HTML format
--- where code and title is shown in one column.
modDataToCompactListView :: HTML h => UserSessionInfo -> ModData -> [[h]]
modDataToCompactListView sinfo modData =
  [[withHref (stringToHtml
                 (modDataCode modData ++ ": " ++
                  (langSelect sinfo modDataNameE modDataNameG) modData))],
   [stringToHtml (showDiv10 (modDataECTS modData))]]
 where
   withHref hexp =
     let txtelem = [if modDataVisible modData then hexp else italic [hexp]]
     in if null (modDataURL modData) || isAdminSession sinfo
          then hrefModule ("?ModData/show/" ++ showModDataKey modData) txtelem
          else hrefExtModule (modDataURL modData) txtelem


--- The short view of a ModData entity as a string.
--- This view is used in menus and comments to refer to a ModData entity.
modDataToShortView :: ModData -> String
modDataToShortView modData = modDataCode modData

--- The detailed view of a ModData entity in HTML format.
--- It also takes associated entities for every associated entity type.
modDataToDetailsView :: HTML h => ModData -> User -> [Category] -> [h]
modDataToDetailsView modData relatedUser categorys =
  [spTable
    (map (\(label,value) -> [label,value])
      (zip modDataLabelList detailedView))]
  where
    detailedView =
      [[stringToHtml (modDataCode modData)]
      ,[stringToHtml (modDataNameG modData)]
      ,[stringToHtml (modDataNameE modData)]
      ,[stringToHtml (modDataCycle modData)]
      ,[stringToHtml (modDataPresence modData)]
      ,[intToHtml (modDataECTS modData)]
      ,[stringToHtml (modDataWorkload modData)]
      ,[intToHtml (modDataLength modData)]
      ,[stringToHtml (modDataURL modData)]
      ,[boolToHtml (modDataVisible modData)]
      ,[htxt (userToShortView relatedUser)]
      ,[htxt (unwords (map categoryToShortView categorys))]]

--- The labels of a ModData entity, as used in HTML tables.
modDataLabelList :: HTML h => [[h]]
modDataLabelList =
  [[textstyle "spicey_label spicey_label_for_type_string" "Modulcode"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Titel"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Englischer Titel"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Turnus"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Präsenzzeiten"]
  ,[textstyle "spicey_label spicey_label_for_type_int" "ECTS"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Workload"]
  ,[textstyle "spicey_label spicey_label_for_type_int" "Dauer (Semester)"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "URL (für externe Module)"]
  ,[textstyle "spicey_label spicey_label_for_type_bool" "Sichtbarkeit"]
  ,[textstyle "spicey_label spicey_label_for_type_relation" "Modulverantwortlicher"]
  ,[textstyle "spicey_label spicey_label_for_type_relation" "Kategorien"]]

--- The list view of a ModDescr entity in HTML format.
--- This view is used in a row of a table of all entities.
modDescrToListView :: HTML h => ModDescr -> [[h]]
modDescrToListView modDescr =
  [[stringToHtml (modDescrLanguage modDescr)]
  ,[stringToHtml (modDescrShortDesc modDescr)]
  ,[stringToHtml (modDescrObjectives modDescr)]
  ,[stringToHtml (modDescrContents modDescr)]
  ,[stringToHtml (modDescrPrereq modDescr)]
  ,[stringToHtml (modDescrExam modDescr)]
  ,[stringToHtml (modDescrMethods modDescr)]
  ,[stringToHtml (modDescrUse modDescr)]
  ,[stringToHtml (modDescrLiterature modDescr)]
  ,[stringToHtml (modDescrLinks modDescr)]
  ,[stringToHtml (modDescrComments modDescr)]]

--- The short view of a ModDescr entity as a string.
--- This view is used in menus and comments to refer to a ModDescr entity.
modDescrToShortView :: ModDescr -> String
modDescrToShortView modDescr = modDescrLanguage modDescr

--- The detailed view of a ModDescr entity in HTML format.
--- It also takes associated entities for every associated entity type.
modDescrToDetailsView :: HTML h => ModDescr -> ModData -> [h]
modDescrToDetailsView modDescr relatedModData =
  [spTable
    (map (\(label,value) -> [label,value])
      (zip modDescrLabelList detailedView))]
  where
    detailedView =
      [[stringToHtml (modDescrLanguage modDescr)]
      ,[stringToHtml (modDescrShortDesc modDescr)]
      ,[stringToHtml (modDescrObjectives modDescr)]
      ,[stringToHtml (modDescrContents modDescr)]
      ,[stringToHtml (modDescrPrereq modDescr)]
      ,[stringToHtml (modDescrExam modDescr)]
      ,[stringToHtml (modDescrMethods modDescr)]
      ,[stringToHtml (modDescrUse modDescr)]
      ,[stringToHtml (modDescrLiterature modDescr)]
      ,[stringToHtml (modDescrLinks modDescr)]
      ,[stringToHtml (modDescrComments modDescr)]
      ,[htxt (modDataToShortView relatedModData)]]

--- The labels of a ModDescr entity, as used in HTML tables.
modDescrLabelList :: HTML h => [[h]]
modDescrLabelList =
  [[textstyle "spicey_label spicey_label_for_type_string" "Lehrsprache"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Kurzbeschreibung", markdownRef]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Lernziele", markdownRef]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Inhalt", markdownRef]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Voraussetzungen", markdownRef]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Prüfungsleistung", markdownRef]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Lehr- und Lernmethoden",
    markdownRef]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Verwendbarkeit", markdownRef]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Literatur", markdownRef]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Verweise", markdownRef]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Kommentar", markdownRef]
  ,[textstyle "spicey_label spicey_label_for_type_relation" "Modul"]]

-- Reference to markdown syntax description:
markdownRef :: HTML h => h
markdownRef = ehref "edit_infos.html#markdown" [htxt "(mit Markdown-Syntax)"]

--- The list view of a ModInst entity in HTML format.
--- This view is used in a row of a table of all entities.
modInstToListView :: HTML h => ModInst -> [[h]]
modInstToListView modInst =
  [[stringToHtml (modInstTerm modInst)],[intToHtml (modInstYear modInst)]]

--- The short view of a ModInst entity as a string.
--- This view is used in menus and comments to refer to a ModInst entity.
modInstToShortView :: ModInst -> String
modInstToShortView modInst = modInstTerm modInst

--- The detailed view of a ModInst entity in HTML format.
--- It also takes associated entities for every associated entity type.
modInstToDetailsView :: HTML h => ModInst -> ModData -> User -> [h]
modInstToDetailsView modInst relatedModData relatedUser =
  [spTable
    (map (\ (label ,value) -> [label,value])
      (zip modInstLabelList detailedView))]
  where detailedView = [[stringToHtml (modInstTerm modInst)]
                       ,[intToHtml (modInstYear modInst)]
                       ,[htxt (modDataToShortView relatedModData)]
                       ,[htxt (userToShortView relatedUser)]]

--- The labels of a ModInst entity, as used in HTML tables.
modInstLabelList :: HTML h => [[h]]
modInstLabelList =
  [[textstyle "spicey_label spicey_label_for_type_string" "Semester"]
  ,[textstyle "spicey_label spicey_label_for_type_int" "Jahr"]
  ,[textstyle "spicey_label spicey_label_for_type_relation" "ModData"]
  ,[textstyle "spicey_label spicey_label_for_type_relation" "Dozent"]]

--- The list view of a AdvisorStudyProgram entity in HTML format.
--- This view is used in a row of a table of all entities.
advisorStudyProgramToListView :: HTML h => AdvisorStudyProgram -> [[h]]
advisorStudyProgramToListView advisorStudyProgram =
  [[stringToHtml (advisorStudyProgramName advisorStudyProgram)]
  ,[stringToHtml (advisorStudyProgramTerm advisorStudyProgram)]
  ,[intToHtml (advisorStudyProgramYear advisorStudyProgram)]
  ,[stringToHtml (advisorStudyProgramDesc advisorStudyProgram)]
  ,[stringToHtml (advisorStudyProgramPrereq advisorStudyProgram)]
  ,[stringToHtml (advisorStudyProgramComments advisorStudyProgram)]
  ,[boolToHtml (advisorStudyProgramVisible advisorStudyProgram)]]

--- The short view of a AdvisorStudyProgram entity as a string.
--- This view is used in menus and comments to refer to a AdvisorStudyProgram entity.
advisorStudyProgramToShortView :: AdvisorStudyProgram -> String
advisorStudyProgramToShortView advisorStudyProgram =
  advisorStudyProgramName advisorStudyProgram

--- The detailed view of a AdvisorStudyProgram entity in HTML format.
--- It also takes associated entities for every associated entity type.
advisorStudyProgramToDetailsView
  :: HTML h => AdvisorStudyProgram -> StudyProgram -> User -> [h]
advisorStudyProgramToDetailsView
    advisorStudyProgram relatedStudyProgram relatedUser =
  [spTable
    (map (\(label,value) -> [label,value])
      (zip advisorStudyProgramLabelList detailedView))]
  where
    detailedView =
      [[stringToHtml (advisorStudyProgramName advisorStudyProgram)]
      ,[stringToHtml (advisorStudyProgramTerm advisorStudyProgram)]
      ,[intToHtml (advisorStudyProgramYear advisorStudyProgram)]
      ,[stringToHtml (advisorStudyProgramDesc advisorStudyProgram)]
      ,[stringToHtml (advisorStudyProgramPrereq advisorStudyProgram)]
      ,[stringToHtml (advisorStudyProgramComments advisorStudyProgram)]
      ,[boolToHtml (advisorStudyProgramVisible advisorStudyProgram)]
      ,[htxt (studyProgramToShortView relatedStudyProgram)]
      ,[htxt (userToShortView relatedUser)]]

--- The labels of a AdvisorStudyProgram entity, as used in HTML tables.
advisorStudyProgramLabelList :: HTML h => [[h]]
advisorStudyProgramLabelList =
  [[textstyle "spicey_label spicey_label_for_type_string" "Titel"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Beginn im Semester"]
  ,[textstyle "spicey_label spicey_label_for_type_int"    "Beginn im Jahr"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Beschreibung", markdownRef]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Voraussetzungen", markdownRef]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Kommentar", markdownRef]
  ,[textstyle "spicey_label spicey_label_for_type_bool" "Sichtbarkeit"]
  ,[textstyle "spicey_label spicey_label_for_type_relation" "Zugehöriger Studiengang"]
  ,[textstyle "spicey_label spicey_label_for_type_relation" "Research Advisor"]]

--- The list view of a AdvisorModule entity in HTML format.
--- This view is used in a row of a table of all entities.
advisorModuleToListView :: HTML h => AdvisorModule -> [[h]]
advisorModuleToListView advisorModule =
  [[boolToHtml (advisorModuleMandatory advisorModule)]]

--- The short view of a AdvisorModule entity as a string.
--- This view is used in menus and comments to refer to a AdvisorModule entity.
advisorModuleToShortView :: AdvisorModule -> String
advisorModuleToShortView advisorModule =
  show (advisorModuleMandatory advisorModule)

--- The detailed view of a AdvisorModule entity in HTML format.
--- It also takes associated entities for every associated entity type.
advisorModuleToDetailsView
  :: HTML h => AdvisorModule -> ModInst -> Category -> AdvisorStudyProgram -> [h]
advisorModuleToDetailsView
    advisorModule relatedModInst relatedCategory relatedAdvisorStudyProgram =
  [spTable
    (map (\(label,value) -> [label,value])
      (zip advisorModuleLabelList detailedView))]
  where
    detailedView =
      [[boolToHtml (advisorModuleMandatory advisorModule)]
      ,[htxt (modInstToShortView relatedModInst)]
      ,[htxt (categoryToShortView relatedCategory)]
      ,[htxt (advisorStudyProgramToShortView relatedAdvisorStudyProgram)]]

--- The labels of a AdvisorModule entity, as used in HTML tables.
advisorModuleLabelList :: HTML h => [[h]]
advisorModuleLabelList =
  [[textstyle "spicey_label spicey_label_for_type_bool" "Status"]
  ,[textstyle "spicey_label spicey_label_for_type_relation" "Modulinstanz"]
  ,[textstyle "spicey_label spicey_label_for_type_relation" "Kategorie"]
  ,[textstyle "spicey_label spicey_label_for_type_relation" "Studienprogramm"]]

--- The list view of a MasterProgram entity in HTML format.
--- This view is used in a row of a table of all entities.
masterProgramToListView :: HTML h => MasterProgram -> h
masterProgramToListView masterProgram =
  hrefPrimSmButton
    ("?MasterProgram/show/" ++ showMasterProgramKey masterProgram)
    [if masterProgramVisible masterProgram
       then stringToHtml (masterProgramName masterProgram)
       else italic [stringToHtml (masterProgramName masterProgram)]]

--- The short view of a MasterProgram entity as a string.
--- This view is used in menus and comments to refer to a MasterProgram entity.
masterProgramToShortView :: MasterProgram -> String
masterProgramToShortView mprog =
  masterProgramName mprog ++ " (Beginn: " ++
  showSemester (masterProgramTerm mprog,masterProgramYear mprog) ++ ")"

--- The detailed view of a MasterProgram entity in HTML format.
--- It also takes associated entities for every associated entity type.
masterProgramToDetailsView
 :: HTML h => MasterProgram -> MasterCoreArea -> User -> [h]
masterProgramToDetailsView masterProgram relatedMasterCoreArea relatedUser =
  [spTable
    (map (\(label,value) -> [label,value])
      (zip masterProgramLabelList detailedView))]
  where
    detailedView =
      [[stringToHtml (masterProgramName masterProgram)]
      ,[stringToHtml (masterProgramTerm masterProgram)]
      ,[intToHtml (masterProgramYear masterProgram)]
      ,[stringToHtml (masterProgramDesc masterProgram)]
      ,[stringToHtml (masterProgramPrereq masterProgram)]
      ,[stringToHtml (masterProgramComments masterProgram)]
      ,[boolToHtml (masterProgramVisible masterProgram)]
      ,[htxt (masterCoreAreaToShortView relatedMasterCoreArea)]
      ,[htxt (userToShortView relatedUser)]]

--- The labels of a MasterProgram entity, as used in HTML tables.
masterProgramLabelList :: HTML h => [[h]]
masterProgramLabelList =
  [[textstyle "spicey_label spicey_label_for_type_string" "Titel"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Beginn im Semester"]
  ,[textstyle "spicey_label spicey_label_for_type_int" "Beginn im Jahr"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Beschreibung", markdownRef]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Voraussetzungen", markdownRef]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Kommentar", markdownRef]
  ,[textstyle "spicey_label spicey_label_for_type_bool" "Sichtbarkeit"]
  ,[textstyle "spicey_label spicey_label_for_type_relation" "Masterbereich"]
  ,[textstyle "spicey_label spicey_label_for_type_relation" "Research Advisor"]]

--- The list view of a MasterProgInfo entity in HTML format.
--- This view is used in a row of a table of all entities.
masterProgInfoToListView :: HTML h => MasterProgInfo -> [[h]]
masterProgInfoToListView masterProgInfo =
  [[stringToHtml (masterProgInfoProgModules masterProgInfo)]
  ,[stringToHtml (masterProgInfoPraktikum masterProgInfo)]
  ,[stringToHtml (masterProgInfoSeminar masterProgInfo)]
  ,[stringToHtml (masterProgInfoThesis masterProgInfo)]
  ,[stringToHtml (masterProgInfoAllgGrundlagen masterProgInfo)]
  ,[stringToHtml (masterProgInfoAnwendungsfach masterProgInfo)]]

--- The short view of a MasterProgInfo entity as a string.
--- This view is used in menus and comments to refer to a MasterProgInfo entity.
masterProgInfoToShortView :: MasterProgInfo -> String
masterProgInfoToShortView masterProgInfo =
  masterProgInfoProgModules masterProgInfo

--- The detailed view of a MasterProgInfo entity in HTML format.
--- It also takes associated entities for every associated entity type.
masterProgInfoToDetailsView :: HTML h => MasterProgInfo -> MasterProgram -> [h]
masterProgInfoToDetailsView masterProgInfo relatedMasterProgram =
  [spTable
    (map (\(label,value) -> [label,value])
      (zip masterProgInfoLabelList detailedView))]
  where
    detailedView =
      [[stringToHtml (masterProgInfoProgModules masterProgInfo)]
      ,[stringToHtml (masterProgInfoPraktikum masterProgInfo)]
      ,[stringToHtml (masterProgInfoSeminar masterProgInfo)]
      ,[stringToHtml (masterProgInfoThesis masterProgInfo)]
      ,[stringToHtml (masterProgInfoAllgGrundlagen masterProgInfo)]
      ,[stringToHtml (masterProgInfoAnwendungsfach masterProgInfo)]
      ,[htxt (masterProgramToShortView relatedMasterProgram)]]

--- The labels of a MasterProgInfo entity, as used in HTML tables.
masterProgInfoLabelList :: HTML h => [[h]]
masterProgInfoLabelList =
  [[textstyle "spicey_label spicey_label_for_type_string" "Studienbereiche"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Praktikum"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Seminar"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Thesis"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "AllgGrundlagen"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Anwendungsfach"]
  ,[textstyle "spicey_label spicey_label_for_type_relation" "MasterProgram"]]

--- The list view of a UnivisInfo entity in HTML format.
--- This view is used in a row of a table of all entities.
univisInfoToListView :: HTML h => UnivisInfo -> [[h]]
univisInfoToListView univisInfo =
  [[stringToHtml (univisInfoCode univisInfo)]
  ,[stringToHtml (univisInfoTerm univisInfo)]
  ,[intToHtml (univisInfoYear univisInfo)]
  ,[stringToHtml (univisInfoURL univisInfo)]]

--- The short view of a UnivisInfo entity as a string.
--- This view is used in menus and comments to refer to a UnivisInfo entity.
univisInfoToShortView :: UnivisInfo -> String
univisInfoToShortView univisInfo = univisInfoCode univisInfo

--- The detailed view of a UnivisInfo entity in HTML format.
univisInfoToDetailsView :: HTML h => UnivisInfo -> [h]
univisInfoToDetailsView univisInfo =
  [spTable
    (map (\(label,value) -> [label,value])
      (zip univisInfoLabelList detailedView))]
  where
    detailedView =
      [[stringToHtml (univisInfoCode univisInfo)]
      ,[stringToHtml (univisInfoTerm univisInfo)]
      ,[intToHtml (univisInfoYear univisInfo)]
      ,[stringToHtml (univisInfoURL univisInfo)]]

--- The labels of a UnivisInfo entity, as used in HTML tables.
univisInfoLabelList :: HTML h => [[h]]
univisInfoLabelList =
  [[textstyle "spicey_label spicey_label_for_type_string" "Code"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Term"]
  ,[textstyle "spicey_label spicey_label_for_type_int" "Year"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "URL"]]

-- Show the short name of each category and its study program
-- for a given list of categories:
showStudyProgCategories :: UserSessionInfo -> [StudyProgram] -> [Category]
                        -> String
showStudyProgCategories sinfo sprogs =
  intercalate ", " . map (showStudyProgCategory sinfo True sprogs)

-----------------------------------------------------------------------------
-- Shows the short name of each category and its study program
-- with a HTML link for a given list of categories.
-- The list of categories is sorted by the position of the corresponding
-- study program and the position of the category in the study program.
showStudyProgCategoriesAsHtml :: HTML h => UserSessionInfo -> [StudyProgram]
                              -> [Category] -> h
showStudyProgCategoriesAsHtml sinfo sprogs cats = inline $
  intersperse nbsp
    (map (\ (c,mbsp) ->
           smallHrefCategory ("?Category/show/" ++ showCategoryKey c)
             [stringToHtml (showCategoryWithStudyProg sinfo True (c,mbsp))]
             `addTitle` (showCategoryWithStudyProg sinfo False (c,mbsp)))
       (sortBy leqCatSP
          (map addStProg cats)))
 where
  leqCatSP (_ , Nothing)  _              = False
  leqCatSP (_ , Just _)   (_ , Nothing)  = True
  leqCatSP (c1, Just sp1) (c2, Just sp2) =
    (studyProgramPosition sp1, categoryPosition c1) <=
    (studyProgramPosition sp2, categoryPosition c2)

  addStProg cat =
    let spkey = categoryStudyProgramProgramCategoriesKey cat
    in (cat, find (\p -> studyProgramKey p == spkey) sprogs)

showCategoryWithStudyProg :: UserSessionInfo -> Bool
                          -> (Category, Maybe StudyProgram) -> String
showCategoryWithStudyProg sinfo short (cat, mbsprog) =
  (if short then categoryShortName
            else langSelect sinfo categoryNameE categoryName) cat ++
  " (" ++
  maybe "?"
        (if short then studyProgramShortName
                  else langSelect sinfo studyProgramNameE studyProgramName)
        mbsprog ++
  ")"

showStudyProgCategory :: UserSessionInfo -> Bool -> [StudyProgram] -> Category
                      -> String
showStudyProgCategory sinfo short sprogs cat =
  let spkey = categoryStudyProgramProgramCategoriesKey cat
  in showCategoryWithStudyProg sinfo short
       (cat, find (\p -> studyProgramKey p == spkey) sprogs)

-----------------------------------------------------------------------------
-- Shows a list of modules as HTML links to the module description:
showModDatasAsLinks :: HTML h => UserSessionInfo -> [ModData] -> h
showModDatasAsLinks _ mods =
  inline
    (intersperse nbsp
       (map (\md -> smallHrefModule ("?ModData/show/" ++ showModDataKey md)
                     [stringToHtml (modDataCode md)]
                    `addTitle` (modDataNameG md))
            mods))

--- The list view of a Student entity in HTML format.
--- This view is used in a row of a table of all entities.
studentToListView :: HTML h => Student -> [[h]]
studentToListView student =
  [[stringToHtml (studentEmail student)]
  ,[stringToHtml (studentName student)]
  ,[stringToHtml (studentFirst student)]
  --,[stringToHtml (studentTAN student)]
  ,[dateToHtml (studentLastLogin student)]
  ]

--- The short view of a Student entity as a string.
--- This view is used in menus and comments to refer to a Student entity.
studentToShortView :: Student -> String
studentToShortView student = studentEmail student

--- The detailed view of a Student entity in HTML format.
studentToDetailsView :: HTML h => UserSessionInfo -> Student -> [h]
studentToDetailsView sinfo student =
  [spTable
    (map (\(label,value) -> [label,value])
      (zip (studentLabelList sinfo) detailedView))]
  where
    detailedView =
      [[stringToHtml (studentEmail student)]
      ,[stringToHtml (studentName student)]
      ,[stringToHtml (studentFirst student)]
      ,[stringToHtml (studentTAN student)]
      ,[dateToHtml (studentLastLogin student)]]

--- The labels of a Student entity, as used in HTML tables.
studentLabelList :: HTML h => UserSessionInfo -> [[h]]
studentLabelList sinfo =
  [[textstyle "spicey_label spicey_label_for_type_string" "Email"]
  ,[textstyle "spicey_label spicey_label_for_type_string" (t "Last name")]
  ,[textstyle "spicey_label spicey_label_for_type_string" (t "First name")]
  ,[textstyle "spicey_label spicey_label_for_type_string" "TAN"]
  ,[textstyle "spicey_label spicey_label_for_type_date" "Last Login"]]
 where
  t = translate sinfo

--- The list view of a StudentCourse entity in HTML format.
--- This view is used in a row of a table of all entities.
studentCourseToListView :: HTML h => StudentCourse -> [[h]]
studentCourseToListView studentCourse =
  [[dateToHtml (studentCourseSelectDate studentCourse)]]

--- The short view of a StudentCourse entity as a string.
--- This view is used in menus and comments to refer to a StudentCourse entity.
studentCourseToShortView :: StudentCourse -> String
studentCourseToShortView studentCourse =
  show (studentCourseSelectDate studentCourse)

--- The detailed view of a StudentCourse entity in HTML format.
--- It also takes associated entities for every associated entity type.
studentCourseToDetailsView :: HTML h => StudentCourse -> ModInst -> Student -> [h]
studentCourseToDetailsView studentCourse relatedModInst relatedStudent =
  [spTable
    (map (\(label,value) -> [label,value])
      (zip studentCourseLabelList detailedView))]
  where
    detailedView =
      [[dateToHtml (studentCourseSelectDate studentCourse)]
      ,[htxt (modInstToShortView relatedModInst)]
      ,[htxt (studentToShortView relatedStudent)]]

--- The labels of a StudentCourse entity, as used in HTML tables.
studentCourseLabelList :: HTML h => [[h]]
studentCourseLabelList =
  [[textstyle "spicey_label spicey_label_for_type_date" "SelectDate"]
  ,[textstyle "spicey_label spicey_label_for_type_relation" "ModInst"]
  ,[textstyle "spicey_label spicey_label_for_type_relation" "Student"]]

-----------------------------------------------------------------------------
