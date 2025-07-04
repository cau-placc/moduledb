module View.ModData
  ( wModData, tuple2ModData, modData2Tuple, wModDataType
  , showModDataView, listModDataView
  , singleModDataView
  , numberModuleView, studentModuleView, studentModuleEmailView
  , leqModData, copyModView, improveCycle
  , selectPreqModuleFormView
 ) where

import Data.List
import Data.Time
import Numeric    ( readNat )
import HTML.Base
import HTML.Styles.Bootstrap4
import HTML.WUI

import System.Spicey
import Model.MDB
import View.MDBEntitiesToHtml
import System.Helpers
import View.ModInst
import Model.ConfigMDB
import View.User(leqUser)
import System.SessionInfo
import System.MultiLang

--- The WUI specification of the module cycle:
wCycle :: WuiSpec String
wCycle = wSelect id ["unregelmäßig","jedes Semester","jedes Jahr"]

--- A WUI for the presence time of a module:
wPresence :: WuiSpec String
wPresence =
  transformWSpec (showPresence,readPresence)
   (w5Tuple wSWS wSWS wSWS wSWS wSWS
     `withRendering`
        (\ [v,u,pue,p,s] -> inline [v, htxt "Vorlesung ", u, htxt "Übung ",
                                    pue, htxt "Praktische Übung ",
                                    p, htxt "Praktikum ", s, htxt "Seminar"]))
 where
   wSWS = wSelect show [0..15]
            `withRendering` (\ [s] -> inline [s `addClass` "numwidth"])

   showPresence (vor,ueb,pue,prk,sem) =
    show vor ++ "V " ++ show ueb ++ "Ü " ++ show pue ++ "PÜ " ++
    show prk ++ "P " ++ show sem ++ "S"

   readPresence ps =
     let presnums = if null ps then [0,0,0,0] else map readNatPrefix (words ps)
      in case presnums of
           [v,u,pue,s]   -> (v,u,pue,0,s) -- old format
           [v,u,pue,p,s] -> (v,u,pue,p,s) -- new format
           _             -> presError ps
    where
     readNatPrefix s = case readNat s of [(n,_)] -> n
                                         _       -> presError ps
     presError s = error $ "View.ModData.wPresence: illegal presence: " ++ s

-- a WUI to select a set of category keys from a given list of categories:
wCatList :: UserSessionInfo -> [(StudyProgram,[Category])] -> WuiSpec [Category]
wCatList sinfo spcats_unsorted =
  wMultiCheckSelect (\c -> [catref c, nbsp]) allcats
    `withCondition` (not . null)
    `withError` "Es muss mindestens eine Kategorie angegeben werden!"
    `withRendering` renderCats
 where
  spcats = map (\ (sp,cats) -> (sp, sortBy leqCat cats)) spcats_unsorted

  leqCat c1 c2 = categoryPosition c1 <= categoryPosition c2

  allcats = concatMap snd spcats

  catref c = ehref ("?Category/show/" ++ showCategoryKey c)
                   [htxt (categoryShortName c)]
               `addTitle` showStudyProgCategory sinfo False (map fst spcats) c

  renderCats hexps = spTable (addBadge2TableData (split2rows spcats hexps))

  split2rows [] _ = []
  split2rows ((sp,cats):sps) hexps =
    ([htxt (studyProgramShortName sp ++ ":")]
                             : map (\h -> [h]) (take (length cats) hexps))
    : split2rows sps (drop (length cats) hexps)

-- Enclose all data items of a table as a light badge.
addBadge2TableData :: HTML h => [[[h]]] -> [[[h]]]
addBadge2TableData xss =
  map (\xs -> map (\x -> [style "badge badge-light" x]) xs) xss

--- The WUI specification for the entity type ModData.
--- It also includes fields for associated entities.
wModData :: UserSessionInfo -> Bool -> Bool -> [User]
         -> [(StudyProgram,[Category])]
         -> WuiSpec (String,String,String,String,String,Int,String,Int,
                     String,Bool,User,[Category])
wModData sinfo admin allowchangemcode userList spcats =
  withRendering
   (w12Tuple (if allowchangemcode then wLargeRequiredString
                                  else wConstant htxt)
             wLargeRequiredString wLargeString wCycle wPresence wECTS
             wLargeRequiredString wLength wURL wHidden wResp wCats)
   -- render such that Visibility field is omitted:
   (\fields -> blockstyle "table-responsive"
                 [if admin
                    then renderLabels (take 9 labelList ++ drop 10 labelList)
                                      (take 9 fields ++ drop 10 fields)
                    else renderLabels (take 9 labelList) fields] )
 where
  labelList = if allowchangemcode
              then [textstyle "label label_for_type_string"
                      "Code (Vorsicht beim Ändern!)"] : drop 1 modDataLabelList
              else modDataLabelList

  wECTS = (if admin then wSelect showDiv10 [0,5..300]
                    else wConstant (htxt . showDiv10))
             `withRendering` numwidthRendering

  wLength = wSelect show [1,2,3]
             `withRendering` numwidthRendering

  wCats = if admin
            then wCatList sinfo spcats
            else wConstant (htxt . unwords . map categoryToShortView)


  wURL = if admin then wLargeString else wConstant htxt

  wResp = if admin then wSelect userToShortView (sortBy leqUser userList)
                   else wConstant (stringToHtml . userToShortView)

  numwidthRendering = inline . map (`addClass` "numwidth")

--- Transformation from data of a WUI form to entity type ModData.
tuple2ModData
 :: ModData
  -> (String,String,String,String,String,Int,String,Int,String,Bool,User
     ,[Category])
  -> (ModData,[Category])
tuple2ModData modDataToUpdate (code ,nameG ,nameE ,cycle ,presence ,eCTS
                               ,workload ,length ,uRL ,visible ,user
                               ,categorys) =
  (setModDataCode
    (setModDataNameG
      (setModDataNameE
        (setModDataCycle
          (setModDataPresence
            (setModDataECTS
              (setModDataWorkload
                (setModDataLength
                  (setModDataURL
                    (setModDataVisible
                      (setModDataUserResponsibleKey modDataToUpdate
                        (userKey user))
                      visible)
                    uRL)
                  length)
                workload)
              eCTS)
            presence)
          cycle)
        nameE)
      nameG)
    code,categorys)

--- Transformation from entity type ModData to a tuple
--- which can be used in WUI specifications.
modData2Tuple
 :: User -> (ModData,[Category])
  -> (String,String,String,String,String,Int,String,Int,String,Bool,User
     ,[Category])
modData2Tuple user (modData ,categorys) =
  (modDataCode modData,modDataNameG modData,modDataNameE modData
  ,modDataCycle modData,modDataPresence modData,modDataECTS modData
  ,modDataWorkload modData,modDataLength modData,modDataURL modData
  ,modDataVisible modData,user,categorys)

--- WUI Type for editing or creating ModData entities.
--- Includes fields for associated entities.
wModDataType :: UserSessionInfo -> Bool -> ModData -> User -> [User]
             -> [(StudyProgram,[Category])]
             -> WuiSpec (ModData,[Category])
wModDataType sinfo admin modData user userList spcats =
  transformWSpec (tuple2ModData modData, modData2Tuple user)
   (wModData sinfo admin admin userList spcats)


-----------------------------------------------------------------------------
--- Supplies a view to show the details of a ModData.
showModDataView :: ModData -> User -> [Category] -> Controller -> [HtmlExp]
showModDataView modData relatedUser categorys controller =
  modDataToDetailsView modData relatedUser categorys ++
   [spButton "back to ModData list" (nextController controller)]


--- A view to show the number of students of a module in a semester.
numberModuleView :: String -> ModData -> Int -> [BaseHtml]
numberModuleView semcode modData mdbnums =
  [h1 [htxt $ "Modul \""++modDataNameG modData++"\""],
   par [htxt "Anzahl der Studierenden, die dieses Modul für das Semester '",
        htxt semcode, htxt "' in der Moduldatenbank eingeplant haben: ",
        htxt (show mdbnums)]]

--- A view to show the students registered for a module in a semester.
studentModuleView :: String -> ModData -> [(String,String,String)] -> [BaseHtml]
studentModuleView semcode moddata studs =
  [h1 [htxt $ "Modul \"" ++ modDataNameG moddata ++ "\""],
   par [htxt "Studierende, die dieses Modul für das Semester '",
        htxt semcode, htxt "' in der Moduldatenbank eingeplant haben: "],
   spTable ([[htxt "Vorname"], [htxt "Nachname"], [htxt "Email"]] :
            map stud2row studs),
   hrefPrimBadge
     ("?ModData/studmails/" ++ showModDataKey moddata ++ "/" ++ semcode)
     [htxt $ "...im Email-Adressenformat"]
  ]
 where
  stud2row (email,name,first) = [[htxt first], [htxt name], [htxt email]]

--- A view to show the email addresses of the students registered
--- for a module in a semester.
studentModuleEmailView :: String -> ModData -> [(String,String,String)]
                       -> [BaseHtml]
studentModuleEmailView semcode moddata studs =
  [h1 [htxt $ "Modul \"" ++ modDataNameG moddata ++ "\""],
   par [htxt $ "Email-Adressen der Studierenden, die dieses Modul " ++
               "für das Semester '" ++ semcode ++
               "' in der Moduldatenbank eingeplant haben: "],
   hrule,
   verbatim (unlines (map stud2email studs)),
   hrule,
   htxt "Email-Adressenliste ohne Namen:",
   hrule,
   verbatim (intercalate ", " (map (\ (email,_,_) -> email) studs))
  ]
 where
  stud2email (email,name,first) =
    "\"" ++ first ++ " " ++ name ++ "\" <" ++ email ++ ">"


--- A view for searching modules.
copyModView :: ModData -> (String -> Controller) -> [HtmlExp]
copyModView oldmod controller =
  [h1 [htxt $ "Modul \""++modDataNameG oldmod++"\" kopieren"],
   par [htxt $ "Hierdurch wird eine Kopie des Moduls \""++
               modDataNameG oldmod++"\" mit einem neuen Modulcode angelegt. "++
               "Die Semesterinstanzen des alten Moduls werden dabei nicht "++
               "kopiert!"],
   par [htxt "Neuer Modulcode für das kopierte Modul: ",
        textField newcode "" `addAttr` ("size","20"),
        spPrimButton "Modul kopieren" copyHandler]]
 where
  newcode free

  copyHandler env =
    let ncode = env newcode
     in if null ncode
        then displayError "Fehler: ungueltiger Modulcode" >>= getPage
        else controller ncode >>= getPage


--- Compares two ModData entities. This order is used in the list view.
leqModData :: ModData -> ModData -> Bool
leqModData x1 x2 =
  (modDataCode x1,modDataNameG x1,modDataNameE x1) <=
   (modDataCode x2,modDataNameG x2,modDataNameE x2)

--- Supplies a list view for a given list of ModData entities.
--- Shows also buttons to show, delete, or edit entries.
--- The arguments are the list of ModData entities
--- and the controller functions to show, delete and edit entities.
listModDataView :: Bool -> String -> [ModData] -> [BaseHtml]
listModDataView admin title modDatas =
  [h1 [htxt title]
  ,spTable
    ([take 2 modDataLabelList ++ [[htxt "ECTS"]]] ++
     map listModData (sortBy leqModData modDatas))]
  where listModData :: ModData -> [[BaseHtml]]
        listModData modData =
          modDataToListView modData ++
            if not admin then [] else
              [[hrefPrimBadge ("?ModData/show/" ++ showModDataKey modData)
                 [htxt "show"]]
              ,[hrefPrimBadge ("?ModData/edit/" ++ showModDataKey modData)
                 [htxt "edit"]]
              ,[hrefPrimBadge ("?ModData/delete/" ++ showModDataKey modData)
                 [htxt "delete"]]]

--- Improves the cycle information of a module w.r.t. a given list
--- of module instances.
improveCycle :: ModData -> [ModInst] -> String
improveCycle md mis =
  let cycle = modDataCycle md
   in if cycle == "jedes Jahr"
      then let uterm = getUniqueTerm mis
            in if null uterm then cycle
                             else cycle++" im "++uterm
      else cycle

--- Computes the unique term (or nothing if not unique) of
--- a list of module instances.
getUniqueTerm :: [ModInst] -> String
getUniqueTerm mis
  | null mis = ""
  | all (\mi -> modInstTerm mi == "WS" ) mis = "WS"
  | all (\mi -> modInstTerm mi == "SS" ) mis = "SS"
  | otherwise = ""

--- Supplies a view for a given ModData entity.
--- Shows also buttons to show, delete, or edit entries.
--- The arguments are the list of ModData entities
--- and the controller functions to show, delete and edit entities.
singleModDataView :: UserSessionInfo -> Bool -> ModData -> User
                  -> [StudyProgram] -> [Category]
                  -> [ModData] -> [ModInst] -> Maybe ModDescr -> String
                  -> [BaseHtml]
singleModDataView sinfo editallowed modData responsibleUser
     sprogs categorys prerequisites modinsts maybedesc xmlurl =
  [par $ stopPlanNote sinfo,
   h1 [htxt ((langSelect sinfo modDataNameE modDataNameG) modData), nbsp,
       ehrefScndBadge ("?ModData/url/" ++ modKeyString) [htxt "URL"], nbsp,
       ehrefScndBadge ("?ModData/pdf/" ++ modKeyString) [htxt "PDF"], nbsp,
       ehrefScndBadge xmlurl [htxt "XML"]],
   par $ (if admin || editallowed
          then [modDataEditButton "edit" "Change basic data", nbsp ] ++
               (maybe []
                  (\_ ->
                    [modDataEditButton "editdesc" "Change description", nbsp])
                  maybedesc) ++
               (if admin || not (modDataVisible modData)
                  then [modDataEditButton "visible" $
                          if admin then "Change visibility" else "Make visible",
                        nbsp]
                  else []) ++
               [modDataEditButton "addinst" "Add semester", nbsp,
                modDataEditButton "editinst" "Change semesters", nbsp,
                modDataEditButton "newpreq" "Add prerequisite", nbsp,
                modDataEditButton "editpreq" "Delete prerequisites", nbsp]
          else []) ++
         (if admin
          then (maybe []
                      (\_ -> [modDataEditButton "copy" "Copy module", nbsp])
                      maybedesc) ++
               [modDataEditButton "email" "Email", nbsp,
                modDataEditButton "delete" "Delete module"]
          else [])] ++
  mainContentsWithSideMenu
   (map (\tag -> [hrefNav ('#':tag) [htxt tag]]) descTitles)
  ([spTable $
    [[[bold [stringToHtml $ t "Module code:"]],
      [stringToHtml (modDataCode modData)]],
     [[bold [stringToHtml $
               langSelect sinfo "German title:" "Englische Bezeichnung:"]],
      [stringToHtml ((langSelect sinfo modDataNameG modDataNameE) modData)]],
     [[bold [stringToHtml $ t "Person in charge:"]],
      [userToHtmlView responsibleUser]],
     [[bold [stringToHtml $ t "Cycle:"]],
      [stringToHtml $ t (toEnglish (improveCycle modData modinsts))] ++
        if null modinsts
          then []
          else htxt " " :
               showSemsOfModInstances (sortBy leqModInst modinsts)],
     [[bold [stringToHtml $ t "Presence:"]],
      [stringToHtml (formatPresence (modDataPresence modData))]],
     [[bold [stringToHtml "ECTS:"]],
      [stringToHtml $ showDiv10 (modDataECTS modData)]],
     [[bold [stringToHtml "Workload:"]],
      [stringToHtml (modDataWorkload modData)]],
     [[bold [stringToHtml $ t "Duration:"]],
      [stringToHtml $ showLen (modDataLength modData) ++ " " ++ t "semester"]],
     [[bold [stringToHtml $ t "Module categories:"]],
      [showStudyProgCategoriesAsHtml sinfo sprogs categorys]],
     [[bold [stringToHtml $ t "Teaching language:"]],
      [stringToHtml
           (maybe ""
                  (\md -> langSelect sinfo toEnglish id (modDescrLanguage md))
                  maybedesc)]],
     [[bold [stringToHtml $ t "Prerequisites" ++ ": ",
             textWithInfoIcon (prereqExplainText sinfo)]],
      [showModDatasAsLinks sinfo prerequisites]]
    ] ++
     (let url = modDataURL modData
       in if null url then [] else
          [[[bold [stringToHtml "URL:"]],[ehref url [stringToHtml url]]]]) ++
     (let vis = if modDataVisible modData
                then t "public"
                else t "only for internal use"
       in if admin || editallowed
          then [[[bold [stringToHtml $ t "Visibility:"]],[stringToHtml vis]]]
          else [])
  ] ++
  maybe []
   (\moddesc ->
     concatMap (\ (title,cnt) ->
                  [htmlStruct "section" [("id",title)] [h3 [htxt $ title++":"]],
                   par [htmlText (if title=="Verweise"
                                    then docText2html (hrefs2markdown cnt)
                                    else docText2html cnt)]])
               (zip descTitles
                 (map (\sel -> sel moddesc)
                      [modDescrShortDesc,modDescrObjectives,modDescrContents,
                       modDescrPrereq,modDescrExam,modDescrMethods,modDescrUse,
                       modDescrLiterature,modDescrLinks,modDescrComments])))
   maybedesc)
 where
   modKeyString = showModDataKey modData

   modDataEditButton act label =
     hrefPrimSmButton ("?ModData/" ++ act ++ "/" ++ modKeyString)
                      [htxt $ t label]

   admin = isAdminSession sinfo

   t = translate sinfo

   descTitles = langSelect sinfo
      ["Abstract","Objectives","Contents","Additional prerequisites",
       "Examination","Learning methods","Usage",
       "Literature","Links","Comments"]
      ["Kurzfassung","Lernziele","Lehrinhalte","Weitere Voraussetzungen",
       "Prüfungsleistung","Lehr- und Lernmethoden","Verwendbarkeit",
       "Literatur","Verweise","Kommentar"]

   showLen l | l==1      = t "one"
             | l==2      = t "two"
             | otherwise = show l

-- show the semesters of module instances enclosed in brackets:
showSemsOfModInstances :: [ModInst] -> [BaseHtml]
showSemsOfModInstances mis =
  if null mis then []
              else htxt "(" : intersperse (htxt " ") (map showSem mis) ++
                   [htxt ")"]
 where
  showSem mi = 
    ehrefInfoBadge ("?ModInst/show/"++showModInstKey mi)
                   [htxt $ showSemester (modInstTerm mi,modInstYear mi)]

-----------------------------------------------------------------------------
--- A form view to select a module and to add it as a prerequisite.
selectPreqModuleFormView ::
  (UserSessionInfo , [(ModDataID,String)], Maybe ModDataID -> Controller)
  -> [HtmlExp]
selectPreqModuleFormView (sinfo, mods, addpreqcontroller) =
  [h1 [htxt $ t "Prerequisite selection"],
   htxt $ t "Select a module:",
   selection selmod modSelection,
   spPrimButton (t "Add") selectModule,
   spButton (t "Cancel") (\_ -> addpreqcontroller Nothing >>= getPage)
  ]
 where
  selmod free
  
  t = translate sinfo

  modSelection = map (\ ((_,ms),i) -> (ms, show i))
                     (zip mods [0..])

  selectModule env =
    let msel = maybe Nothing
                     (\i -> Just (fst (mods!!i)))
                     (findIndex (\ (_,i) -> i == env selmod) modSelection)
    in addpreqcontroller msel >>= getPage

-----------------------------------------------------------------------------
