module Controller.ModData (
 mainModDataController, showModDataWithCode,
 showXmlIndex, showXmlModule,
 formatCatModulesForm, emailModuleMessageController
 ) where

import ConfigMDB
import System.Spicey
import System.Transaction
import HTML.Base
import XML
import Time
import MDB
import MDBExts
import View.ModData
import Controller.ModDescr
import Controller.ModInst
import Maybe
import System.Authorization
import System.AuthorizedActions
import Config.UserProcesses
import System.Authentication
import List
import Sort
import View.StudyProgram
import Controller.Default
import System.Helpers
import View.MDBEntitiesToHtml
import View.ModInst
import FileGoodies(baseName)
import Mail
import Directory
import System.SessionInfo
import System.MultiLang
import System.StudyPlanner

--- Choose the controller for a ModData entity according to the URL parameter.
mainModDataController :: Controller
mainModDataController =
  do args <- getControllerParams
     case args of
      [] -> listAllModDataController
      ["list"]   -> listAllModDataController
      ["new"]    -> newModDataController
      ["newimp"] -> newImportModDataController
      ["show" ,s] ->
       applyControllerOn (readModDataKey s) getModData showModDataController
      ["edit" ,s] ->
       applyControllerOn (readModDataKey s) getModData editModDataController
      ["delete" ,s] ->
       applyControllerOn (readModDataKey s) getModData
        confirmDeleteModDataController
      ["url" ,s] ->
       applyControllerOn (readModDataKey s) getModData moduleUrlForm
      ["pdf" ,s] ->
       applyControllerOn (readModDataKey s) getModData pdfModDataController
      ["email" ,s] ->
       applyControllerOn (readModDataKey s) getModData emailModuleController
      ["copy" ,s] ->
       applyControllerOn (readModDataKey s) getModData copyModuleController
      ["number" ,s,sem] ->
       applyControllerOn (readModDataKey s) getModData
                         (numberModuleController sem)
      ["newpreq",  s] -> applyControllerOn (readModDataKey s) getModData
                                           newPreqModDataController
      ["editpreq", s] -> applyControllerOn (readModDataKey s) getModData
                                            editPreqModDataController
      _ -> displayError "Illegal URL"

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
  checkAuthorization (modDataOperationAllowed NewEntity) $ \_ ->
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
  flip either (\ (md,mi) -> logEvent (NewModData md) >>
                       maybe done (logEvent . NewModDescr) mi >>
                       nextInProcessOr defaultController Nothing)
         (\ error -> displayError (showTError error))

--- Shows a form to edit the given ModData entity.
editModDataController :: ModData -> Controller
editModDataController modDataToEdit =
  checkAuthorization (modDataOperationAllowed (UpdateEntity modDataToEdit)) $     \_ ->
   (do admin <- isAdmin
       spcats <- if admin then getStudyProgramsWithCats else return []
       allUsers <- runQ queryAllUsers
       responsibleUser <- runJustT (getResponsibleUser modDataToEdit)
       categorizingCategorys <- runJustT (getModDataCategories modDataToEdit)
       return
        (editModDataView admin (modDataToEdit,categorizingCategorys)
          responsibleUser allUsers spcats updateModDataController))

--- Persists modifications of a given ModData entity to the
--- database depending on the Boolean argument. If the Boolean argument
--- is False, nothing is changed.
updateModDataController :: Bool -> (ModData,[Category]) -> Controller
updateModDataController False (modData,_) = showModDataController modData
updateModDataController True (modData,newcats) = do
  admin <- isAdmin
  tr <- runT $
    if admin
    then updateModData modData |>>
         getModDataCategories modData |>>= \oldcats ->
         addCategorizing (filter (`notElem` oldcats) newcats) modData |>>
         removeCategorizing (filter (`notElem` newcats) oldcats) modData
    else updateModData modData
  flip either
         (\ _ -> logEvent (UpdateModData modData) >>
                 nextInProcessOr (showModDataController modData) Nothing)
         (\ error -> displayError (showTError error)) tr

--- Deletes a given ModData entity (after asking for confirmation)
--- and proceeds with the list controller.
confirmDeleteModDataController :: ModData -> Controller
confirmDeleteModDataController modData =
  confirmControllerOLD
   (h3
     [htxt
       (concat ["Really delete entity \"",modDataToShortView modData,"\"?"])])
   (\ ack -> if ack
              then deleteModDataController modData
              else showModDataController modData)

--- Deletes a given ModData entity (depending on the Boolean
--- argument) and proceeds with the list controller.
deleteModDataController :: ModData -> Controller
deleteModDataController modData =
  checkAuthorization checkAdmin $ \_->
    runT (getModDataCategories modData |>>= \oldCategorizingCategorys ->
          killCategorizing oldCategorizingCategorys |>>
          queryDescriptionOfMod (modDataKey modData) |>>= \mbdescr ->
          maybe (returnT ()) deleteModDescr mbdescr |>>
          deleteModData modData |>>
          returnT mbdescr) >>=
    flip either (\ mbdescr -> logEvent (DeleteModData modData) >>
                         maybe done (logEvent . DeleteModDescr) mbdescr >>
                         defaultController)
           (\ error -> displayError (showTError error))
 where
  killCategorizing categorys =
    mapT_ (\t -> destroyCategorizing (modDataKey modData) (categoryKey t))
          categorys



--- Controller for showing the number of students of a module in a semester:
numberModuleController :: String -> ModData -> Controller
numberModuleController sem mdata =
 checkAuthorization (modDataOperationAllowed (ShowEntity mdata)) $ \_ -> do
  nums <- getModuleStudents mdata sem
  return (numberModuleView sem mdata nums)

--- Controller for copying a module with a new code:
copyModuleController :: ModData -> Controller
copyModuleController mdata =
 checkAuthorization checkAdmin $ \_ -> do
  maybemdesc <- runQ $ queryDescriptionOfMod (modDataKey mdata)
  maybe (displayError "Illegal URL: cannot copy external module")
        (\mdesc ->
           return (copyModView mdata (storeCopiedModController mdata mdesc)))
        maybemdesc

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
          getModDataCategories mdata |>>= \oldcats ->
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
        flip either (\ (md,mi) -> logEvent (NewModData md) >>
                             logEvent (NewModDescr mi) >>
                             setPageMessage "Modul kopiert" >>
                             nextInProcessOr defaultController Nothing)
               (\ error -> displayError (showTError error))


--- Lists all ModData entities with buttons to show, delete,
--- or edit an entity.
listAllModDataController :: Controller
listAllModDataController =
  checkAuthorization (modDataOperationAllowed ListEntities) $ \_ -> do
    admin <- isAdmin
    login <- getSessionLogin
    modDatas <- runQ queryAllModDatas
    return (listModDataView admin "Alle Module"
                            (maybe (filter modDataVisible modDatas)
                                   (const modDatas)
                                   login))

--- Lists all ModData entities with buttons to show, delete,
--- or edit an entity.
listModDataController :: Controller
listModDataController =
  checkAuthorization (modDataOperationAllowed ListEntities) $ \_ -> do
    admin <- isAdmin
    login <- getSessionLogin
    args <- getControllerParams
    if null args
     then do modDatas <- runQ queryAllModDatas
             return (listModDataView admin "Alle Module"
                        (maybe (filter modDataVisible modDatas) (const modDatas)
                               login))
     else
      maybe (displayError "Illegal URL")
            (\mdkey -> runJustT (getModData mdkey) >>= showModDataController)
            (readModDataKey (head args))

--- Controller to generate the PDF of a module.
pdfModDataController :: ModData -> Controller
pdfModDataController modData =
  checkAuthorization (modDataOperationAllowed (ShowEntity modData)) $ \_ -> do
    responsibleUser <- runJustT (getResponsibleUser modData)
    categories <- runJustT (getModDataCategories modData)
    sprogs <- runQ queryAllStudyPrograms
    let mdkey = modDataKey modData
    moddesc <- runQ $ queryDescriptionOfMod mdkey
    modinsts <- runQ $ queryInstancesOfMod mdkey
    formatModuleForm modData modinsts responsibleUser sprogs categories moddesc

--- Controller to show a module with a given module code.
showModDataWithCode :: String -> Controller
showModDataWithCode mcode = do
   modDatas <- runQ $ queryModDataWithCode mcode
   if null modDatas
    then displayError "Illegal URL: illegal module code"
    else showModDataController (head modDatas)

--- Shows a ModData entity.
showModDataController :: ModData -> Controller
showModDataController modData = do
  responsibleUser <- runJustT (getResponsibleUser modData)
  categories      <- runJustT (getModDataCategories modData)
  prerequisites   <- runJustT (getModDataModDatas modData)
  sprogs <- runQ queryAllStudyPrograms
  moddesc <- runQ $ queryDescriptionOfMod (modDataKey modData)
  modinsts <- runQ $ queryInstancesOfMod (modDataKey modData)
  sinfo <- getUserSessionInfo
  return (singleModDataView sinfo
            (Just (userLogin responsibleUser) == userLoginOfSession sinfo)
            modData responsibleUser
            sprogs categories prerequisites modinsts moddesc (xmlURL modData)
            (editModDescrController (showModDataController modData))
            (addModInstController modData responsibleUser
                                  (showModDataController modData))
            (editAllModInstController modData (showModDataController modData)))


--- Associates given entities with the ModData entity.
addCategorizing :: [Category] -> ModData -> Transaction ()
addCategorizing categorys modData =
  mapM_ (\ t -> newCategorizing (modDataKey modData) (categoryKey t))
        categorys

--- Removes association to the given entities with the ModData entity.
removeCategorizing :: [Category] -> ModData -> Transaction ()
removeCategorizing categorys modData =
  mapM_ (\ t -> deleteCategorizing (modDataKey modData) (categoryKey t))
        categorys

-- Get all study programs with their categories:
getStudyProgramsWithCats :: IO [(StudyProgram,[Category])]
getStudyProgramsWithCats = runQ $ do
  sps <- liftM (mergeSortBy leqStudyProgram) queryAllStudyPrograms
  mapM (\sp -> do cs <- queryCategorysOfStudyProgram (studyProgramKey sp)
                  return (sp,cs))
       sps

-------------------------------------------------------------------------
-- A controller (and view) to send an email:
emailModuleController :: ModData -> Controller
emailModuleController mdata =
 checkAuthorization checkAdmin $ \_ -> do
  user <- runJustT (getResponsibleUser mdata)
  emailModuleMessageController (showModDataController mdata) mdata user
    "Lieber Modulverantwortlicher,\n\n\nViele Gruesse\n\n"

emailModuleMessageController :: Controller -> ModData -> User -> String
                             -> IO [HtmlExp]
emailModuleMessageController cntcontroller mdata user msg = return
   [h1 [htxt "Email an Modulverantwortlichen"],
    spTable
     [[[bold [htxt "Empfänger: "]],
       [longTextField mto (userEmail user)]],
      [[bold [htxt "Kopie an: "]],
       [longTextField mcc adminEmail]],
      [[bold [htxt "Betreff:"]],
       [longTextField msub
                     ("Modul "++modDataCode mdata++": "++modDataNameG mdata)]],
      [[bold [htxt "Inhalt:"]],
       [textarea mcnt (10,70) msg `addClass` "input-xxlarge"]]],
    par [spPrimButton "Absenden" sendTo,
         spButton "Abbrechen" (const (cancelOperation >>
                                      cntcontroller >>= getForm))]]
 where
  mto,mcc,msub,mcnt free

  longTextField ref txt = textfield ref txt `addAttr` ("size","40")
                                            `addClass` "input-xxlarge"

  sendTo env = do
    let cc = env mcc
    sendMailWithOptions adminEmail
                        (env msub)
                        (TO (env mto) : if null cc then [] else [CC cc])
                        (env mcnt)
    setPageMessage "Mail gesendet!"
    cntcontroller >>= getForm

----------------------------------------------------------------------
-- Show the permanent URL of a module
moduleUrlForm :: ModData -> IO [HtmlExp]
moduleUrlForm md = do
  sinfo <- getUserSessionInfo
  let t   = translate sinfo
      url = baseURL ++ "?mod=" ++ string2urlencoded (modDataCode md)
  return
    [h1 [htxt (t "External URL for module"++" \""++modDataNameG md++"\"")],
     par [htxt (useURLText sinfo)],
     h3 [ehref url [htxt url]]]

----------------------------------------------------------------------
-- Format a module as PDF
formatModuleForm :: ModData -> [ModInst] -> User -> [StudyProgram] -> [Category]
                 -> Maybe ModDescr -> IO [HtmlExp]
formatModuleForm md mis respuser sprogs categorys mbdesc = do
  sinfo <- getUserSessionInfo
  pid <- getPID
  let tmp = "tmp_"++show pid
  writeModulesLatexFile sinfo (tmp++".tex")
                        md mis respuser sprogs categorys mbdesc
  latexFormatForm sinfo 2.0 tmp "Formatted module description"

-- Format a list of categories containing modules as PDF
formatCatModulesForm :: [(String,[ModData])] -> IO [HtmlExp]
formatCatModulesForm catmods = do
  sprogs <- runQ queryAllStudyPrograms
  sinfo <- getUserSessionInfo
  pid <- getPID
  let tmp = "tmp_"++show pid
  mstr <- mapIO (formatCatMods sinfo sprogs) catmods
  writeStandaloneLatexFile (tmp++".tex") (concat mstr)
  latexFormatForm sinfo 10.0 tmp "Formatted module descriptions"
 where
  formatCatMods sinfo sprogs (catname,mods) = do
    mstr <- mapIO (formatModData sinfo sprogs) mods
    return ("\\modulecategory{"++catname++"}\n\n"++concat mstr)
    
  formatModData sinfo sprogs md = do
    respuser   <- runJustT (getResponsibleUser md)
    categories <- runJustT (getModDataCategories md)
    mbdesc     <- runQ $ queryDescriptionOfMod (modDataKey md)
    modinsts   <- runQ $ queryInstancesOfMod (modDataKey md)
    return (quoteUnknownLatexCmd
              (mod2latex sinfo md modinsts respuser sprogs categories mbdesc))

-- Form to format a file tmp.tex with pdflatex and show the result.
-- The second argument is the maximum formatting time in seconds,
-- otherwise the formatting process is killed (via timeout).
-- The timout is used to avoid a long blocking of the system
-- by bad latex sources.
latexFormatForm :: UserSessionInfo -> Float -> String -> String -> IO [HtmlExp]
latexFormatForm sinfo tlimit tmp title = do
  let t = translate sinfo
  system $ "/usr/bin/timeout " ++ show tlimit ++ "s " ++
           --"/usr/bin/time -p -o /tmp/xxxMH " ++
           "pdflatex \'\\nonstopmode\\input{"++tmp++".tex}\' 2>&1 > " ++
           tmp++".output"
  pdfexist <- doesFileExist (tmp++".pdf")
  if pdfexist then system ("chmod 644 "++tmp++".pdf")
              else return 0
  output <- readFile (tmp++".output")
  --system ("/bin/rm -f "++tmp++".tex "++tmp++".aux "++tmp++".log")
  system ("/bin/rm -f "++tmp++".aux "++tmp++".log")
  return
    [h1 [htxt $ t title],
     par [href (tmp++".pdf") [htxt (t title ++ " (PDF)")]],
     hrule,
     h3 [htxt $ t "LaTeX output" ++":"],
     verbatim output]

-----------------------------------------------------------------------------
-- Formatting modules as LaTeX documents:

-- Generate LaTeX document containing a detailed description of a module:
writeModulesLatexFile :: UserSessionInfo -> String -> ModData
                      -> [ModInst] -> User -> [StudyProgram]
                      -> [Category] -> Maybe ModDescr -> IO ()
writeModulesLatexFile sinfo fname md mis respuser sprogs categorys mbdesc =
  writeStandaloneLatexFile fname
   (quoteUnknownLatexCmd
      (mod2latex sinfo md mis respuser sprogs categorys mbdesc))

-- Put a latex string into a file with headers and footers
writeStandaloneLatexFile :: String -> String -> IO ()
writeStandaloneLatexFile fname latexstring = do
  writeFile fname ("\\documentclass{article}\n\\input{moddefs}\n" ++
                   "\\begin{document}\n" ++
                   latexstring ++
                   "\\end{document}\n")

mod2latex :: UserSessionInfo -> ModData -> [ModInst] -> User
          -> [StudyProgram] -> [Category]
          -> Maybe ModDescr -> String

mod2latex _ md _ _ _ _ Nothing =
    "%%%%%%%%%% "++modDataCode md++" %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n"++
    "\\importmodule{"++modDataCode md++"}{"++
    escapeLaTeXSpecials (modDataNameG md)++"}{"++
    modDataURL md++"}{"++baseName (modDataURL md)++"}\n\n"

mod2latex sinfo md mis responsibleUser sprogs categorys (Just desc) =
    "%%%%%%%%%% "++modDataCode md++" %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n"++
    "\\module{"++modDataCode md++"}{"++
    escapeLaTeXSpecials (modDataNameG md)++"}{"++
    userToShortView responsibleUser++"}{"++
    improveCycle md mis++"}{"++
    formatPresence (modDataPresence md)++"}{"++
    showDiv10 (modDataECTS md)++"}{"++
    escapeLaTeXSpecials (modDataWorkload md)++"}{"++
    showLen (modDataLength md)++" Semester}{"++
    (showStudyProgCategories sinfo sprogs categorys)++
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

-- Shows XML index of all modules (also invisible ones).
showXmlIndex :: IO HtmlForm
showXmlIndex = do
  allmods <- runQ queryAllModDatas
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
    categories <- runJustT (getModDataCategories md)
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
         (map (\key -> XElem "studiengang"
                             [("key",showStudyProgramKey key)]
                             [xtxt (showLongStudyProgramWithKey key)])
              (map categoryStudyProgramProgramCategoriesKey categorys)),
     xml "kategorien"
         (map (\c ->  XElem "kategorie"
                            [("key",showCategoryKey c)]
                            [xtxt (showLongCategory c)])
              categorys),
     xml "durchfuehrung"
      ([xml "praesenz" [xtxt (modDataPresence md)],
        xml "dauer"    [xtxt (show (modDataLength md))],
        xml "turnus"   [xtxt (modDataCycle md)]] ++
       map modinst2xml (mergeSortBy leqModInst modinsts))])
 where
   showStudyProgramKey spk =
     maybe "?" studyProgramProgKey
           (find (\p -> studyProgramKey p == spk) sprogs)

   showLongStudyProgramWithKey spk =
     maybe "?" studyProgramName (find (\p -> studyProgramKey p == spk) sprogs)

   showCategoryKey cat =
     let pkey = categoryStudyProgramProgramCategoriesKey cat
      in (maybe "?" studyProgramProgKey
                    (find (\p -> studyProgramKey p == pkey) sprogs))
         ++ "_" ++ categoryShortName cat

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
-- A controller to add a new prerequisite to module.
newPreqModDataController :: ModData -> Controller
newPreqModDataController mdata =
 checkAuthorization (modDataOperationAllowed (UpdateEntity mdata)) $ \_ -> do
  allmods <- runJustT queryAllModDataShortInfo
  let selmods = mergeSortBy (\m1 m2 -> snd m1 <= snd m2)
                            (map mod2KeyShortView allmods)
  sinfo <- getUserSessionInfo
  return $ selectPreqModuleView sinfo selmods (addPreqModDataController mdata)
 where
  mod2KeyShortView (key,code,name) = (key, code ++ ": " ++ name)

addPreqModDataController :: ModData -> Maybe ModDataID -> Controller
addPreqModDataController mdata Nothing =
  nextInProcessOr (showModDataController mdata) Nothing
addPreqModDataController mdata (Just preqmodkey) = do
  tr <- runT $ do
          prerequisites <- getModDataModDatas mdata
          if preqmodkey `elem` map modDataKey prerequisites
            then return ()
            else newPrerequisites (modDataKey mdata) preqmodkey
  either (\ error -> displayError (showTError error))
         (\ _ -> do logEvent (NewPrerequisite mdata preqmodkey)
                    setPageMessage "Neue Voraussetzung hinzugefügt"
                    nextInProcessOr (showModDataController mdata) Nothing
         )
         tr

-- A controller to delete prerequisites of a module.
editPreqModDataController :: ModData -> Controller
editPreqModDataController mdata =
 checkAuthorization (modDataOperationAllowed (UpdateEntity mdata)) $ \_ -> do
  prerequisites <- runQ $ getModDataModDatas mdata
  let preqmods = mergeSortBy (\m1 m2 -> snd m1 <= snd m2)
                             (map mod2KeyShortView prerequisites)
  sinfo <- getUserSessionInfo
  return $ deletePreqModuleView sinfo preqmods
             (deletePreqModDataController mdata)
 where
  mod2KeyShortView md =
    (modDataKey md, modDataCode md ++ ": " ++ modDataNameG md)

deletePreqModDataController :: ModData -> [(ModDataID,String,Bool)]
                            -> Controller
deletePreqModDataController mdata delmods = do
  let delkeys = map (\ (k,_,_) -> k) (filter (\ (_,_,b) -> b) delmods)
  tr <- runT $
          mapM_ (\mkey -> deletePrerequisites (modDataKey mdata) mkey) delkeys
  either (\ error -> displayError (showTError error))
         (\ _ -> do
            logEvent (DeletePrerequisites mdata delkeys)
            unless (null delkeys) $ setPageMessage "Voraussetzungen gelöscht"
            nextInProcessOr (showModDataController mdata) Nothing
         )
         tr

----------------------------------------------------------------------------
