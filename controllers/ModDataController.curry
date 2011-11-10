module ModDataController (
 newModDataController, newImportModDataController,
 showModDataController, editModDataController, deleteModDataController,
 listModDataController, getModDataOfCategory, showModDataWithCode,
 getResponsibleUser, showXmlIndex, showXmlModule,
 formatModulesForm
 ) where

import ConfigMDB
import Spicey
import KeyDatabase
import HTML
import XML
import Time
import MDB
import ModDataView
import ModDescrController
import ModInstController
import Maybe
import Authorization
import AuthorizedControllers
import UserProcesses
import Authentication
import List
import Sort
import StudyProgramView
import DefaultController
import Helpers
import MDBEntitiesToHtml
import ModInstView
import FileGoodies(baseName)
import Mail
import Directory

--- Shows a form to create a new ModData entity.
newModDataController :: Controller
newModDataController = newModuleController False

--- Shows a form to create a new ModData entity.
newImportModDataController :: Controller
newImportModDataController = newModuleController True

--- Shows a form to create a new ModData entity.
--- The first argument is true if it should be an import module
--- (without a description)
newModuleController :: Bool -> Controller
newModuleController isimport =
  checkAuthorization (modDataOperationAllowed NewEntity) $
   (do admin <- isAdmin
       spcats <- getStudyProgramsWithCats
       allUsers <- runQ queryAllUsers
       return (blankModDataView admin isimport allUsers spcats
                                (createModDataController isimport)))

--- Persists a new ModData entity to the database.
--- The first argument is true if it should be an import module
--- (without a description)
createModDataController
 :: Bool -> Bool
  -> (String,String,String,String,String,Int,String,Int,String,Bool,User
     ,[Category])
  -> Controller
createModDataController _ False _ = defaultController
createModDataController isimport True
     (code ,nameG ,nameE ,cycle ,presence ,eCTS
     ,workload ,len ,uRL ,visible ,user ,categorys) =
  runT (newModDataWithUserResponsibleKey code nameG nameE cycle presence
          (Just eCTS) workload (Just len) uRL visible (userKey user) |>>= \md ->
        addCategorizing categorys md |>>
        if isimport then returnT (md,Nothing)
         else newModDescrWithModDataDataDescKey "Deutsch" "" "" "" "" "" ""
                       "" "" "" "" (modDataKey md) |>>= \mi ->
              returnT (md,Just mi)) >>=
  either (\ (md,mi) -> logEvent (NewModData md) >>
                       maybe done (logEvent . NewModDescr) mi >>
                       nextInProcessOr defaultController Nothing)
         (\ error -> displayError (showTError error))

--- Shows a form to edit the given ModData entity.
editModDataController :: ModData -> Controller
editModDataController modDataToEdit =
  checkAuthorization (modDataOperationAllowed (UpdateEntity modDataToEdit)) $
   (do admin <- isAdmin
       spcats <- if admin then getStudyProgramsWithCats else return []
       allUsers <- runQ queryAllUsers
       responsibleUser <- runJustT (getResponsibleUser modDataToEdit)
       categorizingCategorys <- runJustT (getModDataCategorys modDataToEdit)
       return
        (editModDataView admin (modDataToEdit,categorizingCategorys)
          responsibleUser allUsers spcats updateModDataController))

--- Persists modifications of a given ModData entity to the
--- database depending on the Boolean argument. If the Boolean argument
--- is False, nothing is changed.
updateModDataController :: Bool -> (ModData,[Category]) -> Controller
updateModDataController False _ = listModDataController
updateModDataController True (modData,newcats) = do
  admin <- isAdmin
  tr <- runT $
    if admin
    then updateModData modData |>>
         getModDataCategorys modData |>>= \oldcats ->
         removeCategorizing (filter (`notElem` newcats) oldcats) modData |>>
         addCategorizing (filter (`notElem` oldcats) newcats) modData
    else updateModData modData
  either (\ _ -> logEvent (UpdateModData modData) >>
                 nextInProcessOr listModDataController Nothing)
         (\ error -> displayError (showTError error)) tr

--- Deletes a given ModData entity (depending on the Boolean
--- argument) and proceeds with the list controller.
deleteModDataController :: ModData -> Bool -> Controller
deleteModDataController _ False = listModDataController
deleteModDataController modData True =
  checkAuthorization (modDataOperationAllowed (DeleteEntity modData)) $
    runT (getModDataCategorys modData |>>= \oldCategorizingCategorys ->
          removeCategorizing oldCategorizingCategorys modData |>>
          getDB (queryDescriptionOfMod (modDataKey modData)) |>>= \mbdescr ->
          maybe doneT deleteModDescr mbdescr |>>
          deleteModData modData |>>
          returnT mbdescr) >>=
    either (\ mbdescr -> logEvent (DeleteModData modData) >>
                         maybe done (logEvent . DeleteModDescr) mbdescr >>
                         defaultController)
           (\ error -> displayError (showTError error))

--- Controller for copying a module with a new code:
copyModController :: ModData -> ModDescr -> Controller
copyModController mdata mdesc =
  return (copyModView mdata (storeCopiedModController mdata mdesc))

--- Controller for copying a module with a new code:
storeCopiedModController :: ModData -> ModDescr -> String -> Controller
storeCopiedModController mdata mdesc newcode = do
  codemods <- runQ $ queryModDataWithCode newcode
  if not (null codemods)
   then displayError "Der eingegebene Modulcode existiert bereits!"
   else runT
         (newModDataWithUserResponsibleKey
            newcode (modDataNameG mdata) (modDataNameE mdata)
            (modDataCycle mdata) (modDataPresence mdata)
            (Just (modDataECTS mdata)) (modDataWorkload mdata)
            (Just (modDataLength mdata)) (modDataURL mdata) False
            (modDataUserResponsibleKey mdata) |>>= \md ->
          getModDataCategorys mdata |>>= \oldcats ->
          addCategorizing oldcats md |>>
          newModDescrWithModDataDataDescKey
            (modDescrLanguage mdesc)
            (modDescrShortDesc mdesc)
            (modDescrObjectives mdesc)
            (modDescrContents mdesc)
            (modDescrPrereq mdesc)
            (modDescrExam mdesc)
            (modDescrMethods mdesc)
            (modDescrUse mdesc)
            (modDescrLiterature mdesc)
            (modDescrLinks mdesc)
            (modDescrComments mdesc)
            (modDataKey md) |>>= \mi ->
          returnT (md,mi)) >>=
        either (\ (md,mi) -> logEvent (NewModData md) >>
                             logEvent (NewModDescr mi) >>
                             setPageMessage "Modul kopiert" >>
                             nextInProcessOr defaultController Nothing)
               (\ error -> displayError (showTError error))


--- Lists all ModData entities with buttons to show, delete,
--- or edit an entity.
listModDataController :: Controller
listModDataController =
  checkAuthorization (modDataOperationAllowed ListEntities) $ do
    admin <- isAdmin
    login <- getSessionLogin
    args <- getControllerParams
    if null args
     then do modDatas <- runQ queryAllModDatas
             return (listModDataView admin "Alle Module"
                        (maybe (filter modDataVisible modDatas) (const modDatas)
                               login)
                        showModDataController
                        editModDataController deleteModDataController)
     else
      if take 5 (head args) == "user="
      then do let lname = drop 5 (head args)
              -- get user entries with a given login name
              users <- runQ $ queryCondUser (\u -> userLogin u == lname)
              if null users then return [h1 [htxt "Illegal URL"]] else
               do mods <- runQ $ queryModDataOfUser (userKey (head users))
                  return (listModDataView admin "Eigene Module"
                           mods showModDataController
                           editModDataController deleteModDataController)
      else maybe (displayError "Illegal URL")
            (\mdkey -> do
              modData <- runJustT $ getModData mdkey
              responsibleUser <- runJustT (getResponsibleUser modData)
              categories <- runJustT (getModDataCategorys modData)
              sprogs <- runQ queryAllStudyPrograms
              moddesc <- runQ $ queryDescriptionOfMod mdkey
              modinsts <- runQ $ queryInstancesOfMod mdkey
              let lname = maybe "" id login
              if null (tail args)
               then return (singleModDataView admin
                        (userLogin responsibleUser == lname)
                        modData responsibleUser
                        sprogs categories modinsts moddesc (xmlURL modData)
                        editModDataController deleteModDataController
                        (editModDescrController listModDataController)
                        (addModInstController modData responsibleUser
                                              listModDataController)
                        (editAllModInstController modData
                                                  listModDataController)
                        copyModController emailModuleController)
               else case args!!1 of
                      "pdf" -> formatModuleForm modData
                                  responsibleUser sprogs categories moddesc
                      "url" -> moduleUrlForm modData
                      _ -> displayError "Illegal URL"
            )
            (readModDataKey (head args))

showModDataWithCode :: String -> Controller
showModDataWithCode mcode = do
   admin <- isAdmin
   modDatas <- runQ $ queryModDataWithCode mcode
   if null modDatas then displayError "Illegal URL" else do
    let modData = head modDatas
    responsibleUser <- runJustT (getResponsibleUser modData)
    categories <- runJustT (getModDataCategorys modData)
    sprogs <- runQ queryAllStudyPrograms
    moddesc <- runQ $ queryDescriptionOfMod (modDataKey modData)
    modinsts <- runQ $ queryInstancesOfMod (modDataKey modData)
    lname <- getSessionLogin >>= return . maybe "" id
    return (singleModDataView admin
              (userLogin responsibleUser == lname)
              modData responsibleUser
              sprogs categories modinsts moddesc (xmlURL modData)
              editModDataController deleteModDataController
              (editModDescrController listModDataController)
              (addModInstController modData responsibleUser
                                    listModDataController)
              (editAllModInstController modData listModDataController)
              copyModController emailModuleController)

--- Shows a ModData entity.
showModDataController :: ModData -> Controller
showModDataController modData =
  checkAuthorization (modDataOperationAllowed (ShowEntity modData)) $
   (do responsibleUser <- runJustT (getResponsibleUser modData)
       categorizingCategorys <- runJustT (getModDataCategorys modData)
       return
        (showModDataView modData responsibleUser categorizingCategorys
          listModDataController))

--- Associates given entities with the ModData entity.
addCategorizing :: [Category] -> ModData -> Transaction ()
addCategorizing categorys modData =
  mapT_ (\ t -> newCategorizing (modDataKey modData) (categoryKey t))
   categorys

--- Removes association to the given entities with the ModData entity.
removeCategorizing :: [Category] -> ModData -> Transaction ()
removeCategorizing categorys modData =
  mapT_ (\ t -> deleteCategorizing (modDataKey modData) (categoryKey t))
   categorys

--- Gets the associated User entity for a given ModData entity.
getResponsibleUser :: ModData -> Transaction User
getResponsibleUser mUser = getUser (modDataUserResponsibleKey mUser)

--- Query the module descriptions of a given category.
getModDataOfCategory :: CategoryKey -> Transaction [ModData]
getModDataOfCategory ck =
  getDB (queryModDataKeysOfCategory ck) |>>= mapT getModData


-- Get all study programs with their categories:
getStudyProgramsWithCats :: IO [(StudyProgram,[Category])]
getStudyProgramsWithCats = do
  sps <- runQ $ transformQ (mergeSort leqStudyProgram) queryAllStudyPrograms
  mapIO (\sp -> do cs <- runQ (queryCategorysOfStudyProgram (studyProgramKey sp))
                   return (sp,cs))
        sps
 where
  queryCategorysOfStudyProgram sp =
    queryCondCategory (\c -> categoryStudyProgramProgramCategoriesKey c == sp)


-------------------------------------------------------------------------
-- A controller (and view) to send an email:
emailModuleController :: ModData -> User -> IO [HtmlExp]
emailModuleController mdata user = return
   [h1 [htxt "Email an Modulverantwortlichen"],
    table
     [[[bold [htxt "Empfänger: "]],
       [longTextField mto (userEmail user)]],
      [[bold [htxt "Kopie an: "]],
       [longTextField mcc adminEmail]],
      [[bold [htxt "Betreff:"]],
       [longTextField msub
                     ("Modul "++modDataCode mdata++": "++modDataNameG mdata)]],
      [[bold [htxt "Inhalt:"]],
       [textarea mcnt (10,70) ("Lieber \n\n\nViele Gruesse\n\n")]]],
    par [button "Absenden" sendTo,
         button "Abbrechen" (const (cancelOperation >>
                                    listModDataController >>= getForm))]]
 where
  mto,mcc,msub,mcnt free

  longTextField ref txt = textfield ref txt `addAttr` ("size","40")

  sendTo env = do
    let cc = env mcc
    sendMailWithOptions adminEmail
                        (env msub)
                        (TO (env mto) : if null cc then [] else [CC cc])
                        (env mcnt)
    setPageMessage "Mail gesendet!"
    listModDataController >>= getForm

----------------------------------------------------------------------
-- Show the permanent URL of a module
moduleUrlForm :: ModData -> IO [HtmlExp]
moduleUrlForm md = do
  let url = baseURL ++ "?mod=" ++ string2urlencoded (modDataCode md)
  return
    [h1 [htxt ("Externe URL für das Modul \""++modDataNameG md++"\"")],
     par [htxt $ "Bitte verwenden Sie die folgende URL, um das Modul aus "++
                 "anderen Webseiten zu referenzieren:"],
     h3 [ehref url [htxt url]]]

----------------------------------------------------------------------
-- Format a module as PDF
formatModuleForm :: ModData -> User -> [StudyProgram] -> [Category]
                 -> Maybe ModDescr -> IO [HtmlExp]
formatModuleForm md respuser sprogs categorys mbdesc = do
  pid <- getPID
  let tmp = "tmp_"++show pid
  writeModulesLatexFile (tmp++".tex")
                        md respuser sprogs categorys mbdesc
  latexFormatForm tmp "Formatierte Modulbeschreibung"

-- Format a list of modules as PDF
formatModulesForm :: [ModData] -> IO [HtmlExp]
formatModulesForm mods = do
  sprogs <- runQ queryAllStudyPrograms
  pid <- getPID
  let tmp = "tmp_"++show pid
  mstr <- mapIO (formatModData sprogs) mods
  writeStandaloneLatexFile (tmp++".tex") (concat mstr)
  latexFormatForm tmp "Formatierte Modulbeschreibungen"
 where
  formatModData sprogs md = do
    respuser <- runJustT (getResponsibleUser md)
    categories <- runJustT (getModDataCategorys md)
    mbdesc <- runQ $ queryDescriptionOfMod (modDataKey md)
    return (quoteUnknownLatexCmd
              (mod2latex md respuser sprogs categories mbdesc))

-- Form to format a file tmp.tex with pdflatex and show the result
latexFormatForm :: String -> String -> IO [HtmlExp]
latexFormatForm tmp title = do
  system ("pdflatex \'\\nonstopmode\\input{"++tmp++".tex}\' 2>&1 > "++
                                                        tmp++".output")
  pdfexist <- doesFileExist (tmp++".pdf")
  if pdfexist then system ("chmod 644 "++tmp++".pdf")
              else return 0
  output <- readFile (tmp++".output")
  --system ("/bin/rm -f "++tmp++".tex "++tmp++".aux "++tmp++".log")
  system ("/bin/rm -f "++tmp++".aux "++tmp++".log")
  return
    [h1 [htxt title],
     par [href (tmp++".pdf") [htxt (title++" (PDF)")]],
     hrule,
     h3 [htxt "LaTeX-Ausgaben während des Formatierens"],
     verbatim output]

-----------------------------------------------------------------------------
-- Formatting modules as LaTeX documents:

-- Generate LaTeX document containing a detailed description of a module:
writeModulesLatexFile :: String -> ModData -> User -> [StudyProgram]
                      -> [Category] -> Maybe ModDescr -> IO ()
writeModulesLatexFile fname md respuser sprogs categorys mbdesc =
  writeStandaloneLatexFile fname
   (quoteUnknownLatexCmd (mod2latex md respuser sprogs categorys mbdesc))

-- Put a latex string into a file with headers and footers
writeStandaloneLatexFile :: String -> String -> IO ()
writeStandaloneLatexFile fname latexstring = do
  writeFile fname ("\\documentclass{article}\n\\input{moddefs}\n" ++
                   "\\begin{document}\n" ++
                   latexstring ++
                   "\\end{document}\n")

mod2latex :: ModData -> User -> [StudyProgram] -> [Category]
          -> Maybe ModDescr -> String

mod2latex md _ _ _ Nothing =
    "%%%%%%%%%% "++modDataCode md++" %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n"++
    "\\importmodule{"++modDataCode md++"}{"++modDataNameG md++"}{"++
    modDataURL md++"}{"++baseName (modDataURL md)++"}\n\n"

mod2latex md responsibleUser sprogs categorys (Just desc) =
    "%%%%%%%%%% "++modDataCode md++" %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n"++
    "\\module{"++modDataCode md++"}{"++modDataNameG md++"}{"++
    userToShortView responsibleUser++"}{"++
    modDataCycle md++"}{"++
    formatPresence (modDataPresence md)++"}{"++
    showDiv10 (modDataECTS md)++"}{"++modDataWorkload md++"}{"++
    showLen (modDataLength md)++" Semester}{"++
    (showStudyProgCategories sprogs categorys)++
    "}\n\\descmain{"++
    modDescrLanguage desc++"}{"++
    docText2latex (modDescrShortDesc desc)++"}{"++
    docText2latex (modDescrObjectives desc)++"}{"++
    docText2latex (modDescrContents desc)++"}{"++
    docText2latex (modDescrPrereq desc)++"}{"++
    docText2latex (modDescrExam desc)++"}{"++
    docText2latex (modDescrMethods desc)++"}{"++
    docText2latex (modDescrUse desc)++"}\n\\descrest{"++
    docText2latex (modDescrLiterature desc)++"}{"++
    docText2latex (modDescrLinks desc)++"}{"++
    docText2latex (modDescrComments desc)++"}\n\n"
 where
   showLen l | l==1 = "ein"
             | l==2 = "zwei"
             | otherwise = show l


-----------------------------------------------------------------------------
-- Formatting modules as XML documents:

-- XML URL of a module:
xmlURL :: ModData -> String
xmlURL md = baseURL++"?xml="++string2urlencoded (modDataCode md)

showXmlIndex :: IO HtmlForm
showXmlIndex = do
  allmods <- runQ (transformQ (filter modDataVisible) queryAllModDatas)
  return (HtmlAnswer "text/xml"
           (showXmlDoc (xml "index"
                            (map mod2index (filter isNotImported allmods)))))
 where
  isNotImported md = modDataURL md == ""

  mod2index m = let c = modDataCode m in
    xml "modul" [xml "code" [xtxt c],
                 xml "url"  [xtxt (xmlURL m)]]

showXmlModule :: String -> IO HtmlForm
showXmlModule mcode = do
   modDatas <- runQ $ queryModDataWithCode mcode
   if null modDatas then displayError "Illegal URL" >>= getForm else do
    let md = head modDatas
    responsibleUser <- runJustT (getResponsibleUser md)
    categories <- runJustT (getModDataCategorys md)
    sprogs <- runQ queryAllStudyPrograms
    moddesc <- runQ $ queryDescriptionOfMod (modDataKey md)
    modinsts <- runQ $ queryInstancesOfMod (modDataKey md)
    users <- runQ queryAllUsers
    return $ HtmlAnswer "text/xml"
               (showXmlDoc (mod2xml md responsibleUser users
                                    sprogs categories modinsts moddesc))

mod2xml :: ModData -> User -> [User] -> [StudyProgram]
        -> [Category] -> [ModInst] -> Maybe ModDescr -> XmlExp
mod2xml md responsibleUser users sprogs categorys modinsts (Just desc) =
  xml "modul"
   ([xml "modulcode" [xtxt (modDataCode md)],
     xml "modulname" [xml "deutsch" [xtxt (modDataNameG md)],
                      xml "englisch" [xtxt (modDataNameE md)]],
     xml "verantwortlich" [xtxt (userToShortView responsibleUser)],
     xml "ectspunkte" [xtxt (showDiv10 (modDataECTS md))],
     xml "workload"   [xtxt (modDataWorkload md)],
     xml "lehrsprache" [xtxt $ modDescrLanguage desc]] ++
    map (\ (tag,sel) -> xml tag [xtxt (docText2html (sel desc))])
        (zip descTitles
             [modDescrShortDesc,modDescrObjectives,modDescrContents,
              modDescrPrereq,modDescrExam,modDescrMethods,modDescrUse,
              modDescrLiterature,modDescrLinks,modDescrComments]) ++
    [xml "studiengaenge"
         (map (\key -> xml "studiengang"
                           [xtxt (showLongStudyProgramWithKey key)])
              (map categoryStudyProgramProgramCategoriesKey categorys)),
     xml "kategorien"
         (map (\c ->  xml "kategorie"
                          [xtxt (showLongCategory c)])
              categorys),
     xml "durchfuehrung"
      ([xml "praesenz" [xtxt (modDataPresence md)],
        xml "dauer"    [xtxt (show (modDataLength md))],
        xml "turnus"   [xtxt (modDataCycle md)]] ++
       map modinst2xml (mergeSort leqModInst modinsts))])
 where
   showLongStudyProgramWithKey spk =
     maybe "?" studyProgramName (find (\p -> studyProgramKey p == spk) sprogs)

   showLongCategory cat =
     let pkey = categoryStudyProgramProgramCategoriesKey cat
      in categoryName cat ++
         " (" ++ maybe "?" studyProgramShortName
                       (find (\p -> studyProgramKey p == pkey) sprogs) ++ ")"

   modinst2xml modinst =
     xml "veranstaltung"
      [xml "semester" [xtxt (showSemester (modInstSemester modinst))],
       xml "dozent"
           [let lect = find (\u-> userKey u==modInstUserLecturerModsKey modinst) 
                            users
             in xtxt $ maybe "" userToShortView lect]]
      -- spaeter noch: zeiten und orte fuer Vorlesungen, Uebungen,...

   descTitles = ["kurzfassung","lernziele","lehrinhalte","voraussetzungen",
                 "pruefungsleistung","lehrmethoden","verwendbarkeit",
                 "literatur","verweise","kommentar"]

mod2xml md _ _ _ _ _  Nothing =
  xml "modul"
      [xml "modulcode" [xtxt (modDataCode md)],
       xml "modulname" [xml "deutsch" [xtxt (modDataNameG md)],
                        xml "englisch" [xtxt (modDataNameE md)]],
       xml "importurl" [xtxt (modDataURL md)]]

-----------------------------------------------------------------------------