-- Entity-relation model for the module database

import Database.ERD

mdbERD :: ERD
mdbERD =
 (ERD "MDB" 
     [Entity "StudyProgram" 
          [Attribute "Name"        (StringDom Nothing) NoKey  False, 
           Attribute "NameE"       (StringDom Nothing) NoKey  True, 
           Attribute "ShortName"   (StringDom Nothing) Unique False,
           Attribute "ProgKey"     (StringDom Nothing) PKey   False,
           Attribute "Position"    (IntDom Nothing)    NoKey  False],
      Entity "Category"
          [Attribute "Name"        (StringDom Nothing) NoKey False, 
           Attribute "NameE"       (StringDom Nothing) NoKey True, 
           Attribute "ShortName"   (StringDom Nothing) NoKey False,
           Attribute "Comment"     (StringDom Nothing) NoKey True,
           Attribute "MinECTS"     (IntDom (Just 0))   NoKey False,
           Attribute "MaxECTS"     (IntDom (Just 180)) NoKey False,
           Attribute "Position"    (IntDom Nothing)    NoKey False],
      Entity "MasterCoreArea"
          [Attribute "Name"        (StringDom Nothing) NoKey False, 
           Attribute "ShortName"   (StringDom Nothing) NoKey False,
           Attribute "Description" (StringDom Nothing) NoKey True,
           Attribute "AreaKey"     (StringDom Nothing) PKey  False,
           Attribute "Position"    (IntDom (Just 1))   NoKey False],
      Entity "User"
          [Attribute "Login"       (StringDom Nothing) PKey  False, 
           Attribute "Name"        (StringDom Nothing) NoKey False, 
           Attribute "First"       (StringDom Nothing) NoKey True,
           Attribute "Title"       (StringDom Nothing) NoKey True,
           Attribute "Email"       (StringDom Nothing) NoKey True,
           Attribute "Url"         (StringDom Nothing) NoKey True,
           Attribute "Password"    (StringDom Nothing) NoKey True,
           Attribute "LastLogin"   (DateDom   Nothing) NoKey False],
      Entity "ModData"
          [Attribute "Code"        (StringDom Nothing) PKey  False, 
           Attribute "NameG"       (StringDom Nothing) NoKey False, 
           Attribute "NameE"       (StringDom Nothing) NoKey True, 
           Attribute "Cycle"       (StringDom Nothing) NoKey True,
           Attribute "Presence"    (StringDom Nothing) NoKey True,
           Attribute "ECTS"        (IntDom (Just 8))   NoKey False,
           Attribute "Workload"    (StringDom Nothing) NoKey True,
           Attribute "Length"      (IntDom (Just 1))   NoKey False,
           Attribute "URL"         (StringDom Nothing) NoKey True,
           Attribute "Visible"     (BoolDom Nothing)   NoKey False],
      Entity "ModDescr"
          [Attribute "Language"    (StringDom Nothing) NoKey False, 
           Attribute "ShortDesc"   (StringDom Nothing) NoKey True, 
           Attribute "Objectives"  (StringDom Nothing) NoKey True, 
           Attribute "Contents"    (StringDom Nothing) NoKey True,
           Attribute "Prereq"      (StringDom Nothing) NoKey True,
           Attribute "Exam"        (StringDom Nothing) NoKey True,
           Attribute "Methods"     (StringDom Nothing) NoKey True,
           Attribute "Use"         (StringDom Nothing) NoKey True,
           Attribute "Literature"  (StringDom Nothing) NoKey True,
           Attribute "Links"       (StringDom Nothing) NoKey True,
           Attribute "Comments"    (StringDom Nothing) NoKey True],
      Entity "ModInst"
          [Attribute "Term"        (StringDom Nothing)  NoKey False, 
           Attribute "Year"        (IntDom (Just 2019)) NoKey False],
      Entity "AdvisorStudyProgram"
          [Attribute "Name"        (StringDom Nothing)  NoKey False, 
           Attribute "Term"        (StringDom Nothing)  NoKey False, 
           Attribute "Year"        (IntDom (Just 2019)) NoKey False,
           Attribute "Desc"        (StringDom Nothing)  NoKey True, 
           Attribute "Prereq"      (StringDom Nothing)  NoKey True, 
           Attribute "Comments"    (StringDom Nothing)  NoKey True,
           Attribute "Visible"     (BoolDom Nothing)    NoKey False],
      Entity "AdvisorModule"
          [Attribute "Mandatory"   (BoolDom Nothing)    NoKey False],
      Entity "MasterProgram"
          [Attribute "Name"        (StringDom Nothing)  NoKey False, 
           Attribute "Term"        (StringDom Nothing)  NoKey False, 
           Attribute "Year"        (IntDom (Just 2019)) NoKey False,
           Attribute "Desc"        (StringDom Nothing)  NoKey True, 
           Attribute "Prereq"      (StringDom Nothing)  NoKey True, 
           Attribute "Comments"    (StringDom Nothing)  NoKey True,
           Attribute "Visible"     (BoolDom Nothing)    NoKey False],
      Entity "MasterProgInfo"
          [Attribute "ProgModules"    (StringDom Nothing) NoKey False, 
           Attribute "Praktikum"      (StringDom Nothing) NoKey True, 
           Attribute "Seminar"        (StringDom Nothing) NoKey True, 
           Attribute "Thesis"         (StringDom Nothing) NoKey True,
           Attribute "AllgGrundlagen" (StringDom Nothing) NoKey True,
           Attribute "Anwendungsfach" (StringDom Nothing) NoKey True],
      Entity "UnivisInfo"
          [Attribute "Code"        (StringDom Nothing) NoKey False, 
           Attribute "Term"        (StringDom Nothing) NoKey False, 
           Attribute "Year"        (IntDom    Nothing) NoKey False,
           Attribute "URL"         (StringDom Nothing) NoKey False],
      Entity "Student"
          [Attribute "Email"       (StringDom Nothing) PKey  False, 
           Attribute "Name"        (StringDom Nothing) NoKey False, 
           Attribute "First"       (StringDom Nothing) NoKey True,
           Attribute "TAN"         (StringDom Nothing) NoKey False,
           Attribute "LastLogin"   (DateDom   Nothing) NoKey False],
      Entity "StudentCourse"
          [Attribute "SelectDate"  (DateDom Nothing)   NoKey False]
     ]
     [Relationship "ProgramCategories"
       [REnd "StudyProgram" "ofProgram"    (Exactly 1),
        REnd "Category"     "withCategory" (Between 0 Infinite)],
      Relationship "Responsible"
       [REnd "User"    "managedBy"      (Exactly 1),
        REnd "ModData" "responsibleFor" (Between 0 Infinite)],
      Relationship "Categorizing"
       [REnd "ModData"  "contains"  (Between 0 Infinite),
        REnd "Category" "belongsTo" (Between 1 Infinite)],
      Relationship "DataDesc"
       [REnd "ModData"  "descOf"   (Exactly 1),
        REnd "ModDescr" "withDesc" (Between 0 (Max 1))],
      Relationship "Prerequisites"
       [REnd "ModData" "isRequiredBy" (Between 0 Infinite),
        REnd "ModData" "requires"     (Between 0 Infinite)],
      Relationship "LecturerMods"
       [REnd "User"     "withLecturer"   (Exactly 1),
        REnd "ModInst"  "instOfLecturer" (Between 0 Infinite)],
      Relationship "ModuleInstances"
       [REnd "ModData"  "withModule"   (Exactly 1),
        REnd "ModInst"  "instOfModule" (Between 0 Infinite)],
      Relationship "StudyAdvising"
       [REnd "User"                "advisedBy"        (Exactly 1),
        REnd "AdvisorStudyProgram" "advisesProgram"   (Between 0 Infinite)],
      Relationship "StudyProgramsAdvised"
       [REnd "StudyProgram"        "instanceOf"       (Exactly 1),
        REnd "AdvisorStudyProgram" "advisedProgram"   (Between 0 Infinite)],
      Relationship "AdvisorProgramModules"
       [REnd "AdvisorStudyProgram" "belongsToAdvisedProgram" (Exactly 1),
        REnd "AdvisorModule"     "moduleOfAdvisorProgram" (Between 0 Infinite)],
      Relationship "AdvisorCategorizing"
       [REnd "Category"        "advisedBelongsTo"     (Exactly 1),
        REnd "AdvisorModule"   "containsAdvisorMods"  (Between 0 Infinite)],
      Relationship "AdvisedProgramModuleInstances"
       [REnd "ModInst"         "withModInst"          (Exactly 1),
        REnd "AdvisorModule"   "advisorUseofModInst"  (Between 0 Infinite)],
      Relationship "Advising"
       [REnd "User"          "organizedBy" (Exactly 1),
        REnd "MasterProgram" "organizes"   (Between 0 Infinite)],
      Relationship "AreaPrograms"
       [REnd "MasterCoreArea" "ofCoreArea"  (Exactly 1),
        REnd "MasterProgram"  "withProgram" (Between 0 Infinite)],
      Relationship "ProgramInfo"
       [REnd "MasterProgram"   "programInfoOf" (Exactly 1),
        REnd "MasterProgInfo"  "withProgInfo"  (Between 0 Infinite)],
      Relationship "StudentCourses"
       [REnd "Student"        "withStudent"      (Exactly 1),
        REnd "StudentCourse"  "coursesOfStudent" (Between 0 Infinite)],
      Relationship "StudentCourseInstances"
       [REnd "ModInst"        "withModInst"  (Exactly 1),
        REnd "StudentCourse"  "instOfModule" (Between 0 Infinite)]
     ]
 )
