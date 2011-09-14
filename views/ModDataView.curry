module ModDataView (
 wModData, tuple2ModData, modData2Tuple, wModDataType, blankModDataView,
 createModDataView, editModDataView, showModDataView, listModDataView,
 singleModDataView, leqModData, copyModView
 ) where

import WUI
import HTML
import Time
import Sort
import Spicey
import MDB
import MDBEntitiesToHtml
import Helpers
import List
import ModInstView
import Read
import Mail
import ConfigMDB
import DefaultController

--- The WUI specification of the module cycle:
wCycle = wSelect id ["unregelm��ig","jedes Semester","jedes Jahr"]

--- A WUI for the presence time of a module:
wPresence :: WuiSpec String
wPresence =
  transformWSpec (showPresence,readPresence)
   (w4Tuple wSWS wSWS wSWS wSWS
     `withRendering`
        (\ [v,u,p,s] -> inline [v, htxt "Vorlesung ", u, htxt "�bung ",
                                p, htxt "Praktikum ", s, htxt "Seminar"]))
 where
   wSWS = wSelect show [0..15]

   showPresence (vor,ueb,prk,sem) =
    show vor ++ "V " ++ show ueb ++ "� " ++ show prk ++ "P " ++ show sem ++ "S"

   readPresence ps =
     let [v,u,p,s] = if null ps then [0,0,0,0] else map Read.readNat (words ps)
      in (v,u,p,s)

-- a WUI to select a set of category keys from a given list of categories:
wCatList :: [(StudyProgram,[Category])] -> WuiSpec [Category]
wCatList spcats =
  wMultiCheckSelect (\c->[catref (categoryCatKey c),nbsp]) allcats
       `withCondition` (not . null)
       `withError` "Es muss mindestens eine Kategorie angegeben werden!"
       `withRendering` renderCats
 where
  allcats = concatMap snd spcats

  catref t = let mbcat = find (\c -> categoryCatKey c == t)
                              allcats
                 short = maybe "?" categoryShortName mbcat
              in ehref "show.cgi?showcats" [htxt short]

  renderCats hexps = table (split2rows spcats hexps)

  split2rows [] _ = []
  split2rows ((sp,cats):sps) hexps =
    ([htxt (studyProgramShortName sp ++ ":")]
                             : map (\h -> [h]) (take (length cats) hexps))
    : split2rows sps (drop (length cats) hexps)


--- The WUI specification for the entity type ModData.
--- It also includes fields for associated entities.
wModData :: Bool -> Bool -> [User] -> [(StudyProgram,[Category])]
         -> WuiSpec (String,String,String,String,String,Int,String,Int,
                     String,Bool,User,[Category])
wModData admin allowchangemcode userList spcats =
  withRendering
   (w12Tuple (if allowchangemcode then wRequiredStringSize 15
                                  else wConstant htxt)
             wReqStr wStr wCycle wPresence wECTS
             wReqStr (wSelect show [1,2,3]) wURL wVisible wResp wCats)
   (renderLabels (if admin then labelList else take 10 labelList))
 where
  labelList = if allowchangemcode
              then [textstyle "label label_for_type_string"
                      "Code (Vorsicht beim �ndern!)"] : drop 1 modDataLabelList
              else modDataLabelList

  wECTS = if admin then wSelect showDiv10 [0,5..300]
                   else wConstant (htxt . showDiv10)

  wCats = if admin
          then wCatList spcats
          else wConstant (htxt . unwords . map categoryToShortView)


  wURL = if admin then wStr else wConstant htxt

  wResp = if admin then wSelect userToShortView userList
                   else wConstant (stringToHtml . userToShortView)

  wStr = wStringSize 70
  wReqStr = wRequiredStringSize 70


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
wModDataType :: Bool -> ModData -> User -> [User]
             -> [(StudyProgram,[Category])]
             -> WuiSpec (ModData,[Category])
wModDataType admin modData user userList spcats =
  transformWSpec (tuple2ModData modData,modData2Tuple user)
   (wModData admin admin userList spcats)

--- Supplies a WUI form to create a new ModData entity.
--- The fields of the entity have some default values.
blankModDataView
 :: Bool -> Bool -> [User] -> [(StudyProgram,[Category])]
  -> (Bool
       -> (String,String,String,String,String,Int,String,Int,String,Bool,User
          ,[Category])
       -> Controller)
  -> [HtmlExp]
blankModDataView admin isimport possibleUsers spcats controller =
  createModDataView admin isimport [] [] [] [] [] 80 wload 1 [] False
                    (head possibleUsers) [] possibleUsers spcats controller
 where
  wload = "60 Std. Vorlesung, 30 Std. Pr�senz�bung, 150 Std. Selbststudium"

--- Supplies a WUI form to create a new ModData entity.
--- Takes default values to be prefilled in the form fields.
createModDataView
 :: Bool -> Bool
  -> String -> String -> String -> String -> String -> Int -> String -> Int
  -> String -> Bool -> User -> [Category] -> [User]
  -> [(StudyProgram,[Category])]
  -> (Bool
       -> (String,String,String,String,String,Int,String,Int,String,Bool,User
          ,[Category])
       -> Controller)
  -> [HtmlExp]
createModDataView admin isimport
                  defaultCode defaultNameG defaultNameE defaultCycle
                  defaultPresence defaultECTS defaultWorkload defaultLength
                  defaultURL defaultVisible defaultUser defaultCategorys
                  possibleUsers spcats controller =
  let initdata = (defaultCode,defaultNameG,defaultNameE,defaultCycle
                 ,defaultPresence,defaultECTS,defaultWorkload,defaultLength
                 ,defaultURL,defaultVisible,defaultUser,defaultCategorys)
      
      wuiframe = wuiEditForm
                    ("Neues "++if isimport then "Importmodul" else "Modul")
                    "Anlegen"
                    (controller False initdata)
      
      (hexp ,handler) = wuiWithErrorForm
                         (wModData admin True possibleUsers spcats)
                         initdata
                         (nextControllerForData (controller True))
                         (wuiFrameToForm wuiframe)
   in wuiframe hexp handler

--- Supplies a WUI form to edit the given ModData entity.
--- Takes also associated entities and a list of possible associations
--- for every associated entity type.
editModDataView
 :: Bool -> (ModData,[Category]) -> User -> [User]
  -> [(StudyProgram,[Category])]
  -> (Bool -> (ModData,[Category]) -> Controller) -> [HtmlExp]
editModDataView admin (modData ,categorys) relatedUser possibleUsers
                spcats controller =
  let initdata = (modData,categorys)
      
      wuiframe = wuiEditFormWithText
                   "Moduldaten �ndern" "�nderungen speichern"
                   [par [htxt "Bitte auch die allgemeinen ",
                         ehref "edit_infos.html"
                               [htxt "Hinweise zu Modulbeschreibungen"],
                         htxt " beachten!"]]
                   (controller False initdata)
      
      (hexp ,handler) = wuiWithErrorForm
                         (wModDataType admin modData relatedUser possibleUsers
                                       spcats)
                         initdata (nextControllerForData (controller True))
                         (wuiFrameToForm wuiframe)
   in wuiframe hexp handler

--- Supplies a view to show the details of a ModData.
showModDataView :: ModData -> User -> [Category] -> Controller -> [HtmlExp]
showModDataView modData relatedUser categorys controller =
  modDataToDetailsView modData relatedUser categorys ++
   [button "back to ModData list" (nextController controller)]

--- A view for searching modules.
copyModView :: ModData -> (String -> Controller) -> [HtmlExp]
copyModView oldmod controller =
  [h1 [htxt $ "Modul \""++modDataNameG oldmod++"\" kopieren"],
   par [htxt $ "Hierdurch wird eine Kopie des Moduls \""++
               modDataNameG oldmod++"\" mit einem neuen Modulcode angelegt. "++
               "Die Semesterinstanzen des alten Moduls werden dabei nicht "++
               "kopiert!"],
   par [htxt "Neuer Modulcode f�r das kopierte Modul: ",
        textfield newcode "" `addAttr` ("size","20"),
        button "Modul kopieren" copyHandler]]
 where
  newcode free

  copyHandler env =
    let ncode = env newcode
     in if null ncode
        then displayError "Fehler: ungueltiger Modulcode" >>= getForm
        else controller ncode >>= getForm


--- Compares two ModData entities. This order is used in the list view.
leqModData :: ModData -> ModData -> Bool
leqModData x1 x2 =
  (modDataCode x1,modDataNameG x1,modDataNameE x1,modDataCycle x1
  ,modDataPresence x1,modDataECTS x1,modDataWorkload x1,modDataLength x1
  ,modDataURL x1,modDataVisible x1) <=
   (modDataCode x2,modDataNameG x2,modDataNameE x2,modDataCycle x2
   ,modDataPresence x2,modDataECTS x2,modDataWorkload x2,modDataLength x2
   ,modDataURL x2,modDataVisible x2)

--- Supplies a list view for a given list of ModData entities.
--- Shows also buttons to show, delete, or edit entries.
--- The arguments are the list of ModData entities
--- and the controller functions to show, delete and edit entities.
listModDataView
 :: Bool -> String -> [ModData]
  -> (ModData -> Controller) -> (ModData -> Controller)
  -> (ModData -> Bool -> Controller) -> [HtmlExp]
listModDataView admin title modDatas showModDataController editModDataController
                deleteModDataController =
  [h1 [htxt title]
  ,table
    ([take 2 modDataLabelList ++ [[htxt "ECTS"]]] ++
     map listModData (mergeSort leqModData modDatas))]
  where listModData :: ModData -> [[HtmlExp]]
        listModData modData =
          modDataToListView modData ++
            if not admin then [] else
            [[button "show" (nextController (showModDataController modData)),
              button "edit" (nextController (editModDataController modData)),
              button "delete"
              (confirmNextController
                (h3
                  [htxt
                    (concat
                      ["Really delete entity \"",modDataToShortView modData
                      ,"\"?"])])
                (deleteModDataController modData))]]

--- Supplies a view for a given ModData entity.
--- Shows also buttons to show, delete, or edit entries.
--- The arguments are the list of ModData entities
--- and the controller functions to show, delete and edit entities.
singleModDataView
 :: Bool -> Bool -> ModData -> User -> [StudyProgram] -> [Category]
  -> [ModInst] -> Maybe ModDescr -> String
  -> (ModData -> Controller)
  -> (ModData -> Bool -> Controller) -> (ModDescr -> Controller)
  -> Controller -> Controller
  -> (ModData -> ModDescr -> Controller)
  -> (ModData -> User -> Controller) -> [HtmlExp]
singleModDataView admin editallowed modData responsibleUser sprogs categorys
     modinsts maybedesc xmlurl editModDataController deleteModDataController
     editModDescrController
     modinstaddController modinsteditController copyModController
     emailController =
  [h1 [htxt ("Modul " ++ modDataCode modData), nbsp,
       HtmlStruct "sup" []
         [ehref ("?listModData/"++showModDataKey modData++"/pdf")
                [imageNB "images/topdf.png" "Convert to PDF"]], nbsp,
       HtmlStruct "sup" []
         [ehref xmlurl [imageNB "images/xml.gif" "XML representation"]]]] ++
  [par $ (if admin || editallowed
          then [button "Semester hinzuf�gen"
                       (nextController modinstaddController),
                button "Semesterangaben �ndern"
                       (nextController modinsteditController),
                button "Moduldaten �ndern"
                       (nextController (editModDataController modData))] ++
                maybe []
                      (\desc ->
                        [button "Modulbeschreibung �ndern"
                             (nextController (editModDescrController desc))] ++
                        if admin
                        then [button "Modul kopieren"
                              (nextController (copyModController modData desc))]
                        else [])
                      maybedesc
          else []) ++
         (if admin
          then [button "Email"
                    (nextController (emailController modData responsibleUser)),
                button "Modul l�schen"
                 (confirmNextController
                    (h3 [htxt (concat
                         ["Really delete entity \"",modDataToShortView modData
                          ,"\"?"])])
                    (deleteModDataController modData))]
          else [])] ++
  [h2 [htxt $ "Titel: "++ modDataNameG modData],
   table $
    [[[bold [stringToHtml "Englische Bezeichnung:"]],
      [stringToHtml (modDataNameE modData)]],
     [[bold [stringToHtml "Modulverantwortliche(r):"]],
      [userToHtmlView responsibleUser]],
     [[bold [stringToHtml "Turnus:"]],
      [stringToHtml (modDataCycle modData)] ++
        if null modinsts then []
        else htxt " ": showSemsOfModInstances (mergeSort leqModInst modinsts)],
     [[bold [stringToHtml "Pr�senzzeiten:"]],
      [stringToHtml (formatPresence (modDataPresence modData))]],
     [[bold [stringToHtml "ECTS:"]],
      [stringToHtml $ showDiv10 (modDataECTS modData)]],
     [[bold [stringToHtml "Workload:"]],
      [stringToHtml (modDataWorkload modData)]],
     [[bold [stringToHtml "Dauer:"]],
      [stringToHtml $ showLen (modDataLength modData) ++ " Semester"]],
     [[bold [stringToHtml "Modulkategorien:"]],
      [showStudyProgCategoriesAsHtml sprogs categorys]],
     [[bold [stringToHtml "Lehrsprache:"]],
      [stringToHtml (maybe "" modDescrLanguage maybedesc)]]] ++
     (let url = modDataURL modData
       in if null url then [] else
          [[[bold [stringToHtml "URL:"]],[ehref url [stringToHtml url]]]]) ++
     (let vis = if modDataVisible modData
                then "�ffentlich sichtbar"
                else "nur zur internen Bearbeitung"
       in if admin || editallowed
          then [[[bold [stringToHtml "Sichtbarkeit"]],[stringToHtml vis]]]
          else [])
  ] ++
  maybe []
   (\moddesc ->
     concatMap (\ (title,cnt) ->
                  [h3 [htxt $ title++":"],
                   par [HtmlText (if title=="Verweise"
                                  then string2hrefs (latex2html cnt)
                                  else latex2html cnt)]])
               (zip descTitles
                 (map (\sel -> sel moddesc)
                      [modDescrShortDesc,modDescrObjectives,modDescrContents,
                       modDescrPrereq,modDescrExam,modDescrMethods,modDescrUse,
                       modDescrLiterature,modDescrLinks,modDescrComments])))
   maybedesc
 where
   descTitles = ["Kurzfassung","Lernziele","Lehrinhalte","Voraussetzungen",
                 "Pr�fungsleistung","Lehr- und Lernmethoden","Verwendbarkeit",
                 "Literatur","Verweise","Kommentar"]

   showLen l | l==1 = "ein"
             | l==2 = "zwei"
             | otherwise = show l

   formatPresence ps = let xs = words ps
                        in unwords (filter (\s -> head s /= '0') xs)

-- show the semesters of module instances enclosed in brackets:
showSemsOfModInstances :: [ModInst] -> [HtmlExp]
showSemsOfModInstances mis =
  if null mis then []
              else htxt "(" : intersperse (htxt ", ") (map showSem mis) ++
                   [htxt ")"]
 where
  showSem mi = 
    ehref ("?listModInst/"++showModInstKey mi)
          [htxt $ showSemester (modInstTerm mi,modInstYear mi)]

----------------------------------------------------------------------
