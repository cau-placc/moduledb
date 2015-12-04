module MDBEntitiesToHtml where

import WUI
import HTML
import Time
import Spicey
import MDB
import Helpers
import List
import MultiLang
import SessionInfo

--- Shows a reference to a study program with its title according
--- to the current language.
studyProgramToHRef :: UserSessionInfo -> StudyProgram -> HtmlExp
studyProgramToHRef sinfo sprog =
  href ("?Category/studyprogram/"++showStudyProgramKey sprog)
       [textstyle "studyprogram"
               ((langSelect sinfo studyProgramNameE studyProgramName) sprog)]

--- The list view of a StudyProgram entity in HTML format.
--- This view is used in a row of a table of all entities.
studyProgramToListView :: StudyProgram -> [[HtmlExp]]
studyProgramToListView studyProgram =
  [name2href (studyProgramName studyProgram)
  ,name2href (studyProgramNameE studyProgram)
  ,[stringToHtml (studyProgramShortName studyProgram)]
  ,[stringToHtml (studyProgramProgKey studyProgram)]
  ,[intToHtml (studyProgramPosition studyProgram)]]
 where
  name2href n =
    [hrefStudyProgram
       ("?Category/studyprogram/"++showStudyProgramKey studyProgram)
       [textstyle "studyprogram" n]]

--- The short view of a StudyProgram entity as a string.
--- This view is used in menus and comments to refer to a StudyProgram entity.
studyProgramToShortView :: StudyProgram -> String
studyProgramToShortView studyProgram = studyProgramShortName studyProgram

--- The detailed view of a StudyProgram entity in HTML format.
studyProgramToDetailsView :: StudyProgram -> [HtmlExp]
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
studyProgramLabelList :: [[HtmlExp]]
studyProgramLabelList =
  [[textstyle "label label_for_type_string" "Name"]
  ,[textstyle "label label_for_type_string" "English name"]
  ,[textstyle "label label_for_type_string" "ShortName"]
  ,[textstyle "label label_for_type_string" "ProgKey (to identify programs)"]
  ,[textstyle "label label_for_type_int" "Position"]]

--- The list view of a Category entity in HTML format.
--- This view is used in a row of a table of all entities.
categoryToListView :: Category -> [[HtmlExp]]
categoryToListView category =
  [name2href (categoryName category)
  ,name2href (categoryNameE category)
  ,[stringToHtml (categoryShortName category)]
  ,[intToHtml (categoryPosition category)]]
 where
   name2href n = [hrefCategory ("?Category/show/"++showCategoryKey category)
                               [stringToHtml n]]

--- The HTML view of a Category entity.
categoryToHtmlView :: Category -> HtmlExp
categoryToHtmlView category =
  smallHrefCategory ("?Category/show/"++showCategoryKey category)
    [htxt (categoryShortName category)]

--- The short view of a Category entity as a string.
--- This view is used in menus and comments to refer to a Category entity.
categoryToShortView :: Category -> String
categoryToShortView category = categoryShortName category

--- The detailed view of a Category entity in HTML format.
--- It also takes associated entities for every associated entity type.
categoryToDetailsView :: Category -> StudyProgram -> [HtmlExp]
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
categoryLabelList :: [[HtmlExp]]
categoryLabelList =
  [[textstyle "label label_for_type_string" "Deutscher Name"]
  ,[textstyle "label label_for_type_string" "Englischer Name"]
  ,[textstyle "label label_for_type_string" "ShortName"]
  ,[textstyle "label label_for_type_string" "Kommentar"]
  ,[textstyle "label label_for_type_int" "Minimale ECTS"]
  ,[textstyle "label label_for_type_int" "Maximale ECTS"]
  ,[textstyle "label label_for_type_int" "Position"]
  ,[textstyle "label label_for_type_relation" "StudyProgram"]]

--- The list view of a MasterCoreArea entity in HTML format.
--- This view is used in a row of a table of all entities.
masterCoreAreaToListView :: MasterCoreArea -> [[HtmlExp]]
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
masterCoreAreaToDetailsView :: MasterCoreArea -> [HtmlExp]
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
masterCoreAreaLabelList :: [[HtmlExp]]
masterCoreAreaLabelList =
  [[textstyle "label label_for_type_string" "Name"]
  ,[textstyle "label label_for_type_string" "ShortName"]
  ,[textstyle "label label_for_type_string" "Description", markdownRef]
  ,[textstyle "label label_for_type_string" "AreaKey"]
  ,[textstyle "label label_for_type_int" "Position"]]

--- The list view of a User entity in HTML format.
--- This view is used in a row of a table of all entities.
userToListView :: User -> [[HtmlExp]]
userToListView user =
  [[stringToHtml (userLogin user)]
  ,[stringToHtml (userName user)]
  ,[stringToHtml (userFirst user)]
  ,[calendarTimeToHtml (userLastLogin user)]]

--- The HTML view of a User entity.
userToHtmlView :: User -> HtmlExp
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
userToDetailsView :: User -> [HtmlExp]
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
      ,[calendarTimeToHtml (userLastLogin user)]]

--- The labels of a User entity, as used in HTML tables.
userLabelList :: [[HtmlExp]]
userLabelList =
  [[textstyle "label label_for_type_string" "Login"]
  ,[textstyle "label label_for_type_string" "Name"]
  ,[textstyle "label label_for_type_string" "First"]
  ,[textstyle "label label_for_type_string" "Title"]
  ,[textstyle "label label_for_type_string" "Email"]
  ,[textstyle "label label_for_type_string" "Url"]
  ,[textstyle "label label_for_type_string" "Password"]
  ,[textstyle "label label_for_type_calendarTime" "LastLogin"]]

--- The list view of a ModData entity in HTML format.
--- This view is used in a row of a table of all entities.
modDataToListView :: ModData -> [[HtmlExp]]
modDataToListView modData =
  [[withHref (stringToHtml (modDataCode modData))],
   [withHref (stringToHtml (modDataNameG modData))],
   [stringToHtml (showDiv10 (modDataECTS modData))]]
 where
   withHref hexp =
     let txtelem = [if modDataVisible modData then hexp else italic [hexp]]
      in  if null (modDataURL modData)
          then hrefModule ("?ModData/show/"++showModDataKey modData) txtelem
          else hrefExtModule (modDataURL modData) txtelem

--- A more compact list view of a ModData entity in HTML format
--- where code and title is shown in one column.
modDataToCompactListView :: UserSessionInfo -> ModData -> [[HtmlExp]]
modDataToCompactListView sinfo modData =
  [[withHref (stringToHtml
                 (modDataCode modData ++ ": " ++
                  (langSelect sinfo modDataNameE modDataNameG) modData))],
   [stringToHtml (showDiv10 (modDataECTS modData))]]
 where
   withHref hexp =
     let txtelem = [if modDataVisible modData then hexp else italic [hexp]]
      in if null (modDataURL modData)
         then hrefModule ("?ModData/show/"++showModDataKey modData) txtelem
         else hrefExtModule (modDataURL modData) txtelem


--- The short view of a ModData entity as a string.
--- This view is used in menus and comments to refer to a ModData entity.
modDataToShortView :: ModData -> String
modDataToShortView modData = modDataCode modData

--- The detailed view of a ModData entity in HTML format.
--- It also takes associated entities for every associated entity type.
modDataToDetailsView :: ModData -> User -> [Category] -> [HtmlExp]
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
modDataLabelList :: [[HtmlExp]]
modDataLabelList =
  [[textstyle "label label_for_type_string" "Code"]
  ,[textstyle "label label_for_type_string" "Titel"]
  ,[textstyle "label label_for_type_string" "Englischer Titel"]
  ,[textstyle "label label_for_type_string" "Turnus"]
  ,[textstyle "label label_for_type_string" "Präsenzzeiten"]
  ,[textstyle "label label_for_type_int" "ECTS"]
  ,[textstyle "label label_for_type_string" "Workload"]
  ,[textstyle "label label_for_type_int" "Dauer (Semester)"]
  ,[textstyle "label label_for_type_string" "URL (für externe Module)"]
  ,[textstyle "label label_for_type_bool" "Sichtbarkeit"]
  ,[textstyle "label label_for_type_relation" "Modulverantwortlicher"]
  ,[textstyle "label label_for_type_relation" "Kategorien"]]

--- The list view of a ModDescr entity in HTML format.
--- This view is used in a row of a table of all entities.
modDescrToListView :: ModDescr -> [[HtmlExp]]
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
modDescrToDetailsView :: ModDescr -> ModData -> [HtmlExp]
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
modDescrLabelList :: [[HtmlExp]]
modDescrLabelList =
  [[textstyle "label label_for_type_string" "Lehrsprache"]
  ,[textstyle "label label_for_type_string" "Kurzbeschreibung", markdownRef]
  ,[textstyle "label label_for_type_string" "Lernziele", markdownRef]
  ,[textstyle "label label_for_type_string" "Inhalt", markdownRef]
  ,[textstyle "label label_for_type_string" "Voraussetzungen", markdownRef]
  ,[textstyle "label label_for_type_string" "Prüfungsleistung", markdownRef]
  ,[textstyle "label label_for_type_string" "Lehr- und Lernmethoden",
    markdownRef]
  ,[textstyle "label label_for_type_string" "Verwendbarkeit", markdownRef]
  ,[textstyle "label label_for_type_string" "Literatur", markdownRef]
  ,[textstyle "label label_for_type_string" "Verweise", markdownRef]
  ,[textstyle "label label_for_type_string" "Kommentar", markdownRef]
  ,[textstyle "label label_for_type_relation" "Modul"]]

-- Reference to markdown syntax description:
markdownRef = ehref "edit_infos.html#markdown" [htxt "(mit Markdown-Syntax)"]

--- The list view of a ModInst entity in HTML format.
--- This view is used in a row of a table of all entities.
modInstToListView :: ModInst -> [[HtmlExp]]
modInstToListView modInst =
  [[stringToHtml (modInstTerm modInst)],[intToHtml (modInstYear modInst)]]

--- The short view of a ModInst entity as a string.
--- This view is used in menus and comments to refer to a ModInst entity.
modInstToShortView :: ModInst -> String
modInstToShortView modInst = modInstTerm modInst

--- The detailed view of a ModInst entity in HTML format.
--- It also takes associated entities for every associated entity type.
modInstToDetailsView :: ModInst -> ModData -> User -> [HtmlExp]
modInstToDetailsView modInst relatedModData relatedUser =
  [spTable
    (map (\ (label ,value) -> [label,value])
      (zip modInstLabelList detailedView))]
  where detailedView = [[stringToHtml (modInstTerm modInst)]
                       ,[intToHtml (modInstYear modInst)]
                       ,[htxt (modDataToShortView relatedModData)]
                       ,[htxt (userToShortView relatedUser)]]

--- The labels of a ModInst entity, as used in HTML tables.
modInstLabelList :: [[HtmlExp]]
modInstLabelList =
  [[textstyle "label label_for_type_string" "Semester"]
  ,[textstyle "label label_for_type_int" "Jahr"]
  ,[textstyle "label label_for_type_relation" "ModData"]
  ,[textstyle "label label_for_type_relation" "Dozent"]]

--- The list view of a AdvisorStudyProgram entity in HTML format.
--- This view is used in a row of a table of all entities.
advisorStudyProgramToListView :: AdvisorStudyProgram -> [[HtmlExp]]
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
  :: AdvisorStudyProgram -> StudyProgram -> User -> [HtmlExp]
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
advisorStudyProgramLabelList :: [[HtmlExp]]
advisorStudyProgramLabelList =
  [[textstyle "label label_for_type_string" "Titel"]
  ,[textstyle "label label_for_type_string" "Beginn im Semester"]
  ,[textstyle "label label_for_type_int"    "Beginn im Jahr"]
  ,[textstyle "label label_for_type_string" "Beschreibung", markdownRef]
  ,[textstyle "label label_for_type_string" "Voraussetzungen", markdownRef]
  ,[textstyle "label label_for_type_string" "Kommentar", markdownRef]
  ,[textstyle "label label_for_type_bool" "Sichtbarkeit"]
  ,[textstyle "label label_for_type_relation" "Zugehöriger Studiengang"]
  ,[textstyle "label label_for_type_relation" "Research Advisor"]]

--- The list view of a AdvisorModule entity in HTML format.
--- This view is used in a row of a table of all entities.
advisorModuleToListView :: AdvisorModule -> [[HtmlExp]]
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
  :: AdvisorModule -> ModInst -> Category -> AdvisorStudyProgram -> [HtmlExp]
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
advisorModuleLabelList :: [[HtmlExp]]
advisorModuleLabelList =
  [[textstyle "label label_for_type_bool" "Status"]
  ,[textstyle "label label_for_type_relation" "Modulinstanz"]
  ,[textstyle "label label_for_type_relation" "Kategorie"]
  ,[textstyle "label label_for_type_relation" "Studienprogramm"]]

--- The list view of a MasterProgram entity in HTML format.
--- This view is used in a row of a table of all entities.
masterProgramToListView :: MasterProgram -> HtmlExp
masterProgramToListView masterProgram =
  spHref ("?MasterProgram/show/"++showMasterProgramKey masterProgram)
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
 :: MasterProgram -> MasterCoreArea -> User -> [HtmlExp]
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
masterProgramLabelList :: [[HtmlExp]]
masterProgramLabelList =
  [[textstyle "label label_for_type_string" "Titel"]
  ,[textstyle "label label_for_type_string" "Beginn im Semester"]
  ,[textstyle "label label_for_type_int" "Beginn im Jahr"]
  ,[textstyle "label label_for_type_string" "Beschreibung", markdownRef]
  ,[textstyle "label label_for_type_string" "Voraussetzungen", markdownRef]
  ,[textstyle "label label_for_type_string" "Kommentar", markdownRef]
  ,[textstyle "label label_for_type_bool" "Sichtbarkeit"]
  ,[textstyle "label label_for_type_relation" "Masterbereich"]
  ,[textstyle "label label_for_type_relation" "Research Advisor"]]

--- The list view of a MasterProgInfo entity in HTML format.
--- This view is used in a row of a table of all entities.
masterProgInfoToListView :: MasterProgInfo -> [[HtmlExp]]
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
masterProgInfoToDetailsView :: MasterProgInfo -> MasterProgram -> [HtmlExp]
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
masterProgInfoLabelList :: [[HtmlExp]]
masterProgInfoLabelList =
  [[textstyle "label label_for_type_string" "Studienbereiche"]
  ,[textstyle "label label_for_type_string" "Praktikum"]
  ,[textstyle "label label_for_type_string" "Seminar"]
  ,[textstyle "label label_for_type_string" "Thesis"]
  ,[textstyle "label label_for_type_string" "AllgGrundlagen"]
  ,[textstyle "label label_for_type_string" "Anwendungsfach"]
  ,[textstyle "label label_for_type_relation" "MasterProgram"]]

--- The list view of a UnivisInfo entity in HTML format.
--- This view is used in a row of a table of all entities.
univisInfoToListView :: UnivisInfo -> [[HtmlExp]]
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
univisInfoToDetailsView :: UnivisInfo -> [HtmlExp]
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
univisInfoLabelList :: [[HtmlExp]]
univisInfoLabelList =
  [[textstyle "label label_for_type_string" "Code"]
  ,[textstyle "label label_for_type_string" "Term"]
  ,[textstyle "label label_for_type_int" "Year"]
  ,[textstyle "label label_for_type_string" "URL"]]

-- Show the short name of each category and its study program
-- for a given list of categories:
showStudyProgCategories :: [StudyProgram] -> [Category] -> String
showStudyProgCategories sprogs cats =
  concat (intersperse ", " (map (showStudyProgCategory sprogs) cats))

-----------------------------------------------------------------------------
-- Show the short name of each category and its study program
-- with a HTML link for a given list of categories:
showStudyProgCategoriesAsHtml :: [StudyProgram] -> [Category] -> HtmlExp
showStudyProgCategoriesAsHtml sprogs cats =
  inline
    (intersperse nbsp --(stringToHtml " ")
       (map (\c -> smallHrefCategory ("?Category/show/"++showCategoryKey c)
                     [stringToHtml (showStudyProgCategory sprogs c)])
            cats))

showStudyProgCategory :: [StudyProgram] -> Category -> String
showStudyProgCategory sprogs cat =
    let pkey = categoryStudyProgramProgramCategoriesKey cat
     in categoryShortName cat ++
        " (" ++ showShortStudyProgramWithKey pkey ++ ")"
 where
  showShortStudyProgramWithKey spk =
    let sp = find (\p -> studyProgramKey p == spk) sprogs
     in maybe "?" studyProgramShortName sp

-----------------------------------------------------------------------------
