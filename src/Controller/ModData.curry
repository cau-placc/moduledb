module Controller.ModData
  ( mainModDataController, newModDataWuiForm
  , showModDataWithCode, showXmlIndex, showXmlModule
  , formatCatModulesForm
  , emailModuleMessageForm, emailModuleStore
  , copyModuleForm
  , addModInstForm, editModInstForm, editModDataForm, editModDescrForm
  , newPreqModDataForm, deletePreqModDataForm
  ) where

import Directory
import Global
import List
import Maybe
import Sort
import System
import Time

import ConfigMDB
import System.Spicey
import HTML.Base
import HTML.Styles.Bootstrap4
import HTML.Session
import HTML.WUI
import XML
import MDB
import MDBExts
import MDB.Queries
import View.ModData
import Config.EntityRoutes
import Controller.ModDescr
import Controller.ModInst
import System.Authorization
import System.AuthorizedActions
import Config.UserProcesses
import System.Authentication
import View.StudyProgram
import System.Helpers
import View.MDBEntitiesToHtml
import View.ModDescr ( wModDescrType )
import View.ModInst
import View.User    ( leqUser )
import FileGoodies  ( baseName )
import Mail
import System.SessionInfo
import System.MultiLang
import System.StudyPlanner

import SpecialQueries ( queryStudentsOfModSemester
                      , queryStudentNumberOfModSemester )

------------------------------------------------------------------------------
-- Routing

--- Choose the controller for a ModData entity according to the URL parameter.
mainModDataController :: Controller
mainModDataController =
  do args <- getControllerParams
     case args of
      [] -> listAllModDataController
      ["list"]   -> listAllModDataController
      ["new"]    -> newModuleController False
      ["newimp"] -> newModuleController True
      ["show" ,s] -> controllerOnKey s showModDataController
      ["edit" ,s] -> controllerOnKey s editModDataController
      ["visible" ,s] -> controllerOnKey s toggleVisibilityModDataController
      ["delete" ,s] -> controllerOnKey s confirmDeleteModDataController
      ["destroy",s] -> controllerOnKey s destroyModDataController
      ["url" ,s] -> controllerOnKey s moduleUrlForm
      ["pdf" ,s] -> controllerOnKey s pdfModDataController
      ["email" ,s] -> controllerOnKey s emailModuleController
      ["copy" ,s] -> controllerOnKey s copyModuleController
      ["number",s,sem] -> controllerOnKey s (numberModuleController sem)
      ["studs",s,sem] -> controllerOnKey s (studentModuleController sem)
      ["addinst",  s] -> controllerOnKey s addInstToModDataController
      ["editinst", s] -> controllerOnKey s editInstOfModDataController
      ["editdesc", s] -> controllerOnKey s editDescrOfModDataController
      ["newpreq",  s] -> controllerOnKey s newPreqModDataController
      ["editpreq", s] -> controllerOnKey s editPreqModDataController
      _ -> displayUrlError

--- Returns the ModData entity specified by the second URL parameter.
getModDataFromURL :: IO ModData
getModDataFromURL = do
  args <- getControllerParams
  case args of
    (_:s:_) -> maybe (error "Illegal URL")
                     (\key -> runJustT (getModData key))
                     (readModDataKey s)
    _       -> error "Illegal URL"

-------------------------------------------------------------------------
--- Shows a form to create a new ModData entity.
--- The first argument is true if it should be an import module
--- (without a description)
newModuleController :: Bool -> Controller
newModuleController isimport =
  checkAuthorization (modDataOperationAllowed NewEntity) $ \sinfo -> do
    admin <- isAdmin
    spcats <- getStudyProgramsWithCats
    allUsers <- runQ queryAllUsers
    setParWuiStore wuiNewModDataWuiStore
      (sinfo,admin,isimport,allUsers,spcats)
      ("", "", "", "", "", 80, wload, 1, "", False, head allUsers, [])
    return [formElem newModDataWuiForm]
 where
  wload = "60 Std. Vorlesung, 30 Std. Präsenzübung, 150 Std. Selbststudium"


type NewModData =
 (String,String,String,String,String,Int,String,Int,String,Bool,User,[Category])

--- A WUI form to create a new ModData entity.
--- The default values for the fields are stored in the
--- `wuiNewModDataWuiStore`.
newModDataWuiForm :: HtmlFormDef
   ((UserSessionInfo, Bool, Bool, [User], [(StudyProgram,[Category])]),
    WuiStore NewModData)
newModDataWuiForm =
  pwui2FormDef "Controller.ModData.newModDataWuiForm"
    wuiNewModDataWuiStore
    (\ (sinfo,admin,_,allusers,spcats) ->
         wModData sinfo admin True allusers spcats)
    (\ (_,_,isimport,_,_) entity ->
         checkAuthorization (modDataOperationAllowed NewEntity) $ \_ ->
           createModDataController isimport entity)
    (\ (sinfo,_,isimport,_,_) ->
        renderWUI sinfo ("Neues "++if isimport then "Importmodul" else "Modul")
                  "Anlegen" "?" ())

---- The data stored for executing the WUI form.
wuiNewModDataWuiStore ::
  Global (SessionStore
            ((UserSessionInfo, Bool, Bool, [User], [(StudyProgram,[Category])]),
             WuiStore NewModData))
wuiNewModDataWuiStore =
  global emptySessionStore (Persistent (inSessionDataDir "wuiNewModDataWuiStore"))


--- Persists a new ModData entity to the database.
--- The first argument is true if it should be an import module
--- (without a description)
createModDataController
  :: Bool
  -> (String,String,String,String,String,Int,String,Int,String,Bool,User
     ,[Category])
  -> Controller
createModDataController isimport
     (code ,nameG ,nameE ,cycle ,presence ,eCTS
     ,workload ,len ,uRL ,visible ,user ,categorys) =
  runT (newModDataWithUserResponsibleKey code nameG nameE cycle presence
          (Just eCTS) workload (Just len) uRL visible (userKey user) |>>= \md ->
        addCategorizing categorys md |>>
        if isimport then returnT (md,Nothing)
         else newModDescrWithModDataDataDescKey "Deutsch" "" "" "" "" "" ""
                       "" "" "" "" (modDataKey md) |>>= \mi ->
              returnT (md,Just mi)) >>=
  either (\ error -> displayError (showTError error))
         (\ (md,mi) -> logEvent (NewModData md) >>
                       maybe done (logEvent . NewModDescr) mi >>
                       nextInProcessOr redirectToDefaultController Nothing)


-------------------------------------------------------------------------
--- Shows a form to edit the given ModData entity.
editModDataController :: ModData -> Controller
editModDataController mdata =
  checkAuthorization (modDataOperationAllowed (UpdateEntity mdata)) $
   \sinfo -> do
    admin <- isAdmin
    spcats <- if admin then getStudyProgramsWithCats else return []
    allUsers <- runQ queryAllUsers
    responsibleUser <- runJustT (getResponsibleUser mdata)
    categorizingCategorys <- runJustT (getModDataCategories mdata)
    setParWuiStore wuiEditModDataStore
      (sinfo,admin,mdata,responsibleUser,allUsers,spcats)
      (mdata, categorizingCategorys)
    return [formElem editModDataForm]

--- A WUI form to edit the given ModData entity.
editModDataForm ::
  HtmlFormDef
   ((UserSessionInfo,Bool,ModData,User,[User],[(StudyProgram,[Category])]),
    WuiStore (ModData,[Category]))
editModDataForm =
  pwui2FormDef "Controller.ModData.editModDataForm"
    wuiEditModDataStore
    (\ (sinfo,admin,mdata,relatedUser,allusers,spcats) ->
        wModDataType sinfo admin mdata relatedUser allusers spcats)
    (\_ -> updateModDataController)
    (\ (sinfo,_,mdata,_,_,_) ->
         renderWUIWithText sinfo "Moduldaten ändern" "Änderungen speichern"
           [par cmts] (showRoute mdata) ())
 where
  cmts = [htxt "Bitte auch die allgemeinen ",
          ehref "edit_infos.html" [htxt "Hinweise zu Modulbeschreibungen"],
          htxt " beachten!"]

--- The data stored for executing the WUI form.
wuiEditModDataStore ::
  Global (SessionStore
     ((UserSessionInfo,Bool,ModData,User,[User],[(StudyProgram,[Category])]),
      WuiStore (ModData,[Category])))
wuiEditModDataStore =
  global emptySessionStore (Persistent (inSessionDataDir "wuiEditModDataStore"))


--- Persists modifications of a given ModData entity.
updateModDataController :: (ModData,[Category]) -> Controller
updateModDataController (modData,newcats) =
 checkAuthorization (modDataOperationAllowed (UpdateEntity modData)) $ \_ -> do
  admin <- isAdmin
  tr <- runT $
    if admin
    then updateModData modData |>>
         getModDataCategories modData |>>= \oldcats ->
         addCategorizing (filter (`notElem` oldcats) newcats) modData |>>
         removeCategorizing (filter (`notElem` newcats) oldcats) modData
    else updateModData modData
  either (displayError . showTError)
         (\ _ -> logEvent (UpdateModData modData) >>
                 nextInProcessOr (redirectToShowModDataController modData)
                                 Nothing)
         tr

--- Toggles the visibility of the given ModData entity.
toggleVisibilityModDataController :: ModData -> Controller
toggleVisibilityModDataController mdata =
 checkAuthorization (modDataOperationAllowed (UpdateEntity mdata)) $ \_ -> do
  admin <- isAdmin
  -- Toggle visibility only if admin or not visible:
  let oldvis   = modDataVisible mdata
      newvis   = if admin || not oldvis then not oldvis else oldvis
      newmdata = setModDataVisible mdata newvis
  tr <- runT $ updateModData newmdata
  either (displayError . showTError)
         (\ _ -> logEvent (UpdateModData newmdata) >>
                 nextInProcessOr (showModDataController newmdata) Nothing)
         tr

--- Deletes a given ModData entity (after asking for confirmation)
--- and proceeds with the list controller.
confirmDeleteModDataController :: ModData -> Controller
confirmDeleteModDataController modData =
  checkAuthorization checkAdmin $ \si->
    confirmDeletionPage si $ concat
      ["Really delete entity \"",modDataToShortView modData,"\"?"]

--- Deletes a given ModData entity (depending on the Boolean
--- argument) and proceeds with the list controller.
destroyModDataController :: ModData -> Controller
destroyModDataController modData =
  checkAuthorization checkAdmin $ \_->
    runT (getModDataCategories modData |>>= \oldCategorizingCategorys ->
          killCategorizing oldCategorizingCategorys |>>
          queryDescriptionOfMod (modDataKey modData) |>>= \mbdescr ->
          maybe (returnT ()) deleteModDescr mbdescr |>>
          deleteModData modData |>>
          returnT mbdescr) >>=
    flip either (\ mbdescr -> logEvent (DeleteModData modData) >>
                         maybe done (logEvent . DeleteModDescr) mbdescr >>
                         redirectToDefaultController)
           (\ error -> displayError (showTError error))
 where
  killCategorizing categorys =
    mapM_ (\t -> destroyCategorizing (modDataKey modData) (categoryKey t))
          categorys


--- Controller to show the number of students of a module in a semester:
numberModuleController :: String -> ModData -> Controller
numberModuleController sem mdata =
 checkAuthorization (modDataOperationAllowed (ShowEntity mdata)) $ \_ -> do
  --spnum  <- getModuleStudents mdata sem
  mdbnum <- runQ $ queryStudentNumberOfModSemester mdata (readSemesterCode sem)
  return $ numberModuleView sem mdata mdbnum

--- Controller to show the students of a module registered in MDB in a semester:
studentModuleController :: String -> ModData -> Controller
studentModuleController sem mdata =
 checkAuthorization checkAdmin $ \_ -> do
   studs <- runQ $ queryStudentsOfModSemester mdata (readSemesterCode sem)
   return $ studentModuleView sem mdata studs

-------------------------------------------------------------------------
--- Controller for copying a module with a new code:
copyModuleController :: ModData -> Controller
copyModuleController mdata =
 checkAuthorization checkAdmin $ \_ -> do
  writeSessionData copyModuleStore mdata
  return [formElem copyModuleForm]

copyModuleForm :: HtmlFormDef (ModData,ModDescr)
copyModuleForm =
  formDefWithID "Controller.ModData.copyModuleForm" readData
    (\ (mdata,mdesc) -> copyModView mdata
                          (storeCopiedModController mdata mdesc))
 where
  readData = toFormReader $ do
    mdata <- fromFormReader $ getSessionData copyModuleStore failed
    maybemdesc <- runQ $ queryDescriptionOfMod (modDataKey mdata)
    maybe (error "Illegal URL: cannot copy external module")
          (\mdesc -> return (mdata, mdesc))
          maybemdesc

---- The data stored for executing the WUI form.
copyModuleStore :: Global (SessionStore ModData)
copyModuleStore =
  global emptySessionStore (Persistent (inSessionDataDir "copyModuleStore"))

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
        either (\ error -> displayError (showTError error))
               (\ (md,mi) -> do
                   logEvent (NewModData md)
                   logEvent (NewModDescr mi)
                   setPageMessage "Modul kopiert"
                   nextInProcessOr redirectToDefaultController Nothing)


-------------------------------------------------------------------------
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
      maybe displayUrlError
            (\mdkey -> runJustT (getModData mdkey) >>= showModDataController)
            (readModDataKey (head args))

--- Controller to generate the PDF of a module.
pdfModDataController :: ModData -> Controller
pdfModDataController modData =
  checkAuthorization (modDataOperationAllowed (ShowEntity modData)) $ \_ -> do
    responsibleUser <- runJustT (getResponsibleUser modData)
    categories      <- runJustT (getModDataCategories modData)
    prerequisites   <- runJustT (getModDataModDatas modData)
    sprogs <- runQ queryAllStudyPrograms
    let mdkey = modDataKey modData
    moddesc <- runQ $ queryDescriptionOfMod mdkey
    modinsts <- runQ $ queryInstancesOfMod mdkey
    formatModuleForm modData modinsts responsibleUser sprogs categories
                     prerequisites moddesc

--- Controller to show a module with a given module code.
showModDataWithCode :: String -> Controller
showModDataWithCode mcode = do
   modDatas <- runQ $ queryModDataWithCode mcode
   if null modDatas
    then displayError "Illegal URL: illegal module code"
    else showModDataController (head modDatas)

--- Redirect to a page showing the given ModData entity.
--- Useful to set the URL route correctly.
redirectToShowModDataController :: ModData -> Controller
redirectToShowModDataController = redirectController . showRoute

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
            sprogs categories prerequisites modinsts moddesc (xmlURL modData))


--- Associates given entities with the ModData entity.
addCategorizing :: [Category] -> ModData -> DBAction ()
addCategorizing categorys modData =
  mapM_ (\ t -> newCategorizing (modDataKey modData) (categoryKey t))
        categorys

--- Removes association to the given entities with the ModData entity.
removeCategorizing :: [Category] -> ModData -> DBAction ()
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
  writeSessionData emailModuleStore
    (mdata,"Lieber Modulverantwortlicher,\n\n\nViele Gruesse\n\n")
  return [formElem emailModuleMessageForm]

--- A form to send an email to the person responsible for the module
--- stored in `emailModuleStore` (which also contains the initial message).
emailModuleMessageForm :: HtmlFormDef (ModData,User,String)
emailModuleMessageForm =
  formDefWithID "Controller.ModData.emailModuleMessageForm" readData formView
 where
  readData = toFormReader $ do
    (mdata,msg) <- fromFormReader $ getSessionData emailModuleStore (failed,"")
    user <- runJustT (getResponsibleUser mdata)
    return (mdata,user,msg)

  formView (mdata,user,msg) =
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
        [textArea mcnt (10,70) msg `addClass` "input-xxlarge"]]],
     par [spPrimButton "Absenden" sendTo,
          spButton "Abbrechen" (const (cancelOperation >>
                                       cntcontroller >>= getPage))]]
   where
    mto,mcc,msub,mcnt free
  
    longTextField ref txt = textField ref txt `addAttr` ("size","40")
                                              `addClass` "input-xxlarge"
  
    sendTo env = do
      let cc = env mcc
      sendMailWithOptions adminEmail
                          (env msub)
                          (TO (env mto) : if null cc then [] else [CC cc])
                          (env mcnt)
      setPageMessage "Mail gesendet!"
      cntcontroller >>= getPage

    cntcontroller = showModDataController mdata

---- The data stored for executing the WUI form.
emailModuleStore :: Global (SessionStore (ModData,String))
emailModuleStore =
  global emptySessionStore (Persistent (inSessionDataDir "emailModuleStore"))


----------------------------------------------------------------------
-- Show the permanent URL of a module
moduleUrlForm :: ModData -> IO [BaseHtml]
moduleUrlForm md = do
  sinfo <- getUserSessionInfo
  let t     = translate sinfo
      title = (langSelect sinfo modDataNameE modDataNameG) md
      url   = baseURL ++ "?mod=" ++ string2urlencoded (modDataCode md)
  return
    [h1 [htxt (t "External URL for module" ++ " \"" ++ title ++ "\"")],
     par [htxt (useURLText sinfo)],
     h3 [ehref url [htxt url]]]

----------------------------------------------------------------------
-- Format a module as PDF
formatModuleForm :: ModData -> [ModInst] -> User -> [StudyProgram] -> [Category]
                 -> [ModData] -> Maybe ModDescr -> IO [BaseHtml]
formatModuleForm md mis respuser sprogs categorys prerequisites mbdesc = do
  sinfo <- getUserSessionInfo
  pid <- getPID
  let tmp = "tmp_"++show pid
  writeModulesLatexFile sinfo (tmp++".tex")
                        md mis respuser sprogs categorys prerequisites mbdesc
  latexFormatForm sinfo 2.0 tmp "Formatted module description"

-- Format a list of categories containing modules as PDF
formatCatModulesForm :: [(String,[ModData])] -> IO [BaseHtml]
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
    prereqs    <- runJustT (getModDataModDatas md)
    mbdesc     <- runQ $ queryDescriptionOfMod (modDataKey md)
    modinsts   <- runQ $ queryInstancesOfMod (modDataKey md)
    return $ quoteUnknownLatexCmd $
      mod2latex sinfo md modinsts respuser sprogs categories prereqs mbdesc

-- Form to format a file tmp.tex with pdflatex and show the result.
-- The second argument is the maximum formatting time in seconds,
-- otherwise the formatting process is killed (via timeout).
-- The timout is used to avoid a long blocking of the system
-- by bad latex sources.
latexFormatForm :: UserSessionInfo -> Float -> String -> String -> IO [BaseHtml]
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
    [par [hrefPrimButton (tmp ++ ".pdf") [htxt (t title ++ " (PDF)")]],
     hrule,
     h3 [htxt $ t "LaTeX output" ++":"],
     verbatim output]

-----------------------------------------------------------------------------
-- Formatting modules as LaTeX documents:

-- Generate LaTeX document containing a detailed description of a module:
writeModulesLatexFile :: UserSessionInfo -> String -> ModData
                      -> [ModInst] -> User -> [StudyProgram]
                      -> [Category] -> [ModData] -> Maybe ModDescr -> IO ()
writeModulesLatexFile sinfo fname md mis respuser sprogs categorys
                      prerequisites mbdesc =
  writeStandaloneLatexFile fname
   (quoteUnknownLatexCmd
      (mod2latex sinfo md mis respuser sprogs categorys prerequisites mbdesc))

-- Put a latex string into a file with headers and footers
writeStandaloneLatexFile :: String -> String -> IO ()
writeStandaloneLatexFile fname latexstring = do
  writeFile fname ("\\documentclass{article}\n\\input{moddefs}\n" ++
                   "\\begin{document}\n" ++
                   latexstring ++
                   "\\end{document}\n")

mod2latex :: UserSessionInfo -> ModData -> [ModInst] -> User
          -> [StudyProgram] -> [Category] -> [ModData]
          -> Maybe ModDescr -> String

mod2latex _ md _ _ _ _ _ Nothing =
  "%%%%%%%%%% "++modDataCode md++" %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n"++
  "\\importmodule{" ++ modDataCode md ++ "}{" ++
  escapeLaTeXSpecials (modDataNameG md) ++ "}{" ++
  modDataURL md ++ "}{" ++baseName (modDataURL md) ++ "}\n\n"

mod2latex sinfo md mis responsibleUser sprogs categorys prerequisites
          (Just desc) =
  "%%%%%%%%%% " ++ modDataCode md ++ " %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n" ++
  "\\module" ++ langSelect sinfo "E" "G" ++ "{" ++ modDataCode md ++ "}{" ++
  escapeLaTeXSpecials ((langSelect sinfo modDataNameE modDataNameG) md) ++
  "}{" ++
  userToShortView responsibleUser ++ "}{" ++
  t (toEnglish (improveCycle md mis)) ++ "}{" ++
  formatPresence (modDataPresence md) ++ "}{" ++
  showDiv10 (modDataECTS md) ++ "}{" ++
  escapeLaTeXSpecials (modDataWorkload md) ++ "}{" ++
  showLen (modDataLength md) ++ " " ++ t "semester" ++ "}{" ++
  escapeLaTeXSpecials (showStudyProgCategories sinfo sprogs categorys) ++
  "}\n\\descmain" ++ langSelect sinfo "E" "G" ++ "{" ++
  modDescrLanguage desc ++ "}{" ++
  docText2latex (modDescrShortDesc desc) ++ "}{" ++
  docText2latex (modDescrObjectives desc) ++ "}{" ++
  docText2latex (modDescrContents desc) ++ "}{" ++
  docText2latex
    ((if null prerequisites
        then ""
        else t "Modules" ++ ": " ++
             intercalate ", "
               (map (langSelect sinfo modDataNameE modDataNameG)
                    prerequisites) ++ "\n\n") ++
     modDescrPrereq desc) ++ "}{" ++
  docText2latex (modDescrExam desc) ++ "}{" ++
  docText2latex (modDescrMethods desc) ++ "}{" ++
  docText2latex (modDescrUse desc) ++
  "}\n\\descrest" ++ langSelect sinfo "E" "G" ++ "{" ++
  docText2latex (modDescrLiterature desc) ++ "}{" ++
  docText2latex (modDescrLinks desc) ++ "}{" ++
  docText2latex (modDescrComments desc) ++ "}\n\n"
 where
  t = translate sinfo

  showLen l | l==1      = t "one"
            | l==2      = t "two"
            | otherwise = show l

-----------------------------------------------------------------------------
-- Formatting modules as XML documents:

-- XML URL of a module:
xmlURL :: ModData -> String
xmlURL md = baseURL++"?xml="++string2urlencoded (modDataCode md)

-- Shows XML index of all modules (also invisible ones).
showXmlIndex :: IO HtmlPage
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

showXmlModule :: String -> IO HtmlPage
showXmlModule mcode = do
   modDatas <- runQ $ queryModDataWithCode mcode
   if null modDatas then displayUrlError >>= getPage else do
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
-- A controller to add module instance to the given module.
addInstToModDataController :: ModData -> Controller
addInstToModDataController mdata =
 checkAuthorization (modDataOperationAllowed (UpdateEntity mdata)) $ \sinfo-> do
   responsibleUser <- runJustT (getResponsibleUser mdata)
   allUsers <- runQ (liftM (mergeSortBy leqUser) queryAllUsers)
   (curterm,curyear) <- getCurrentSemester
   setParWuiStore wuiAddModInstStore
                  (sinfo,curyear,allUsers,mdata)
                  (curterm,curyear+1,responsibleUser)
   return [formElem addModInstForm]

--- A WUI form to create a new ModInst entity.
addModInstForm :: HtmlFormDef ((UserSessionInfo, Int, [User], ModData),
                               WuiStore (String,Int,User))
addModInstForm =
  pwui2FormDef "Controller.ModData.addModInstForm"
    wuiAddModInstStore
    (\ (_,curyear,allusers,_) -> wModInst (curyear+1) allusers)
    (\ (_,_,_,mdata) -> createModInstController mdata
                          (redirectToShowModDataController mdata))
    (\ (sinfo,_,_,mdata) ->
         renderWUI sinfo "Neues Semester hinzufügen" "Hinzufügen"
           (showRoute mdata) ())

--- The data stored for executing the WUI form.
wuiAddModInstStore ::
  Global (SessionStore ((UserSessionInfo, Int, [User], ModData),
                        WuiStore (String,Int,User)))
wuiAddModInstStore =
  global emptySessionStore (Persistent (inSessionDataDir "wuiAddModInstStore"))

-----------------------------------------------------------------------------
-- A controller to edit all module instances of the given module.
editInstOfModDataController :: ModData -> Controller
editInstOfModDataController mdata =
 checkAuthorization (modDataOperationAllowed (UpdateEntity mdata)) $ \sinfo-> do
   admin <- isAdmin
   cursem   <- getCurrentSemester
   allinsts <- runQ $ liftM
                        (mergeSortBy leqModInst . filterModInsts admin cursem)
                        (queryInstancesOfMod (modDataKey mdata))
   allmpkeys <- runQ $ getMasterProgramKeysOfModInst allinsts
   allspkeys <- runJustT $
                 mapM (\mi -> getAdvisorStudyProgramKeysOfModInst mi)
                      allinsts
   allUsers <- runQ (liftM (mergeSortBy leqUser) queryAllUsers)
   -- select instances not used in master programs:
   let editinsts = concatMap
                       (\ (mi,mks,sks) -> if (null mks && null sks) || admin
                                            then [(null mks && null sks, mi)]
                                            else [])
                       (zip3 allinsts allmpkeys allspkeys)
   setParWuiStore wuiEditModInstStore
                  (sinfo, snd cursem, admin, editinsts, allUsers, mdata)
                  (map (\ (b,i) -> if b then Left (i,False) else Right i)
                       editinsts)
   return [formElem editModInstForm]
 where
  filterModInsts admin cursem =
   -- the next semester in the future where we changes are allowed:
   -- if we are in semester n, it is not allowed to change instances
   -- in semester n, n+1, and n+2.
   let futuresem = nextSemester (nextSemester (nextSemester cursem))
   in if admin
        then id
        else filter (\mi -> leqSemester futuresem
                                        (modInstTerm mi,modInstYear mi))
 

--- A WUI form to edit the ModInst entities provided in the store
--- `wuiEditModInstStore`. If the flag for a ModInst entity is true,
--- one can complete change the entity, otherwise one can only
--- change the lecturer of this entity.
editModInstForm ::
  HtmlFormDef ((UserSessionInfo, Int, Bool, [(Bool,ModInst)],[User], ModData),
               WuiStore [Either (ModInst,Bool) ModInst])
editModInstForm = 
  pwui2FormDef "Controller.ModData.editModInstForm"
    wuiEditModInstStore
    (\ (_,curyear,admin,binsts,allusers,_) -> 
                    wModInstsType curyear admin (map snd binsts) allusers)
    (\ (_,_,_,binsts,_,mdata) ->
        updateAllModInstController mdata (map snd binsts)
                                   (redirectToShowModDataController mdata)
          . map (either id (\i -> (i,False))))
    (\ (sinfo,_,_,_,_,mdata) ->
         renderWUIWithText sinfo "Semesterangaben ändern" "Änderungen speichern"
                           [par [htxt modinstcomment]]
                           (showRoute mdata) ())
 where
  modinstcomment =
    "Anmerkung: Veranstaltungen innerhalb des Planungszeitraumes " ++
    "(bis zu den beiden nächsten Semestern) und Veranstaltungen, "++
    "die schon in Masterprogrammen eingeplant sind, können nicht "++
    "verändert werden! In Ausnahmefällen kontaktieren Sie die " ++
    "Studiengangskoordinatoren."

--- The data stored for executing the WUI form.
wuiEditModInstStore ::
  Global (SessionStore
            ((UserSessionInfo, Int, Bool, [(Bool,ModInst)],[User], ModData),
             WuiStore [Either (ModInst,Bool) ModInst]))
wuiEditModInstStore =
  global emptySessionStore (Persistent (inSessionDataDir "wuiEditModInstStore"))


-----------------------------------------------------------------------------
-- A controller to edit the description of the given module.
editDescrOfModDataController :: ModData -> Controller
editDescrOfModDataController mdata =
 checkAuthorization (modDataOperationAllowed (UpdateEntity mdata)) $ \sinfo-> do
   moddesc <- runQ $ queryDescriptionOfMod (modDataKey mdata)
   maybe displayUrlError
         (\md -> do setParWuiStore wuiEditModDescrStore (sinfo,mdata,md) md
                    return [formElem editModDescrForm])
         moddesc

--- A WUI form to edit the given ModDescr entity.
--- Takes also associated entities and a list of possible associations
--- for every associated entity type.
editModDescrForm ::
  HtmlFormDef ((UserSessionInfo,ModData,ModDescr), WuiStore ModDescr)
editModDescrForm =
  pwui2FormDef "Controller.ModData.editModDescrForm"
    wuiEditModDescrStore
    (\ (sinfo,_,moddescr) -> wModDescrType sinfo moddescr)
    (\ (_,mdata,_) -> updateModDescrController
                        (redirectToShowModDataController mdata))
    (\ (sinfo,mdata,_) ->
         renderWUIWithText sinfo "Modulbeschreibung ändern"
           "Änderungen speichern"  [par cmts]
           (showRoute mdata) ())
 where
  cmts = [htxt "Bitte auch die allgemeinen ",
          ehref "edit_infos.html" [htxt "Hinweise zu Modulbeschreibungen"],
          htxt " beachten!"]

--- The data stored for executing the WUI form.
wuiEditModDescrStore ::
  Global (SessionStore ((UserSessionInfo,ModData,ModDescr), WuiStore ModDescr))
wuiEditModDescrStore =
  global emptySessionStore (Persistent (inSessionDataDir "wuiEditModDescrStore"))


-----------------------------------------------------------------------------
-- A controller to add a new prerequisite to module.
newPreqModDataController :: ModData -> Controller
newPreqModDataController mdata =
 checkAuthorization (modDataOperationAllowed (UpdateEntity mdata)) $ \_ -> do
  return [formElem newPreqModDataForm]

-- A form to add a new prerequisite to module.
newPreqModDataForm :: HtmlFormDef
  (UserSessionInfo , [(ModDataID,String)], Maybe ModDataID -> Controller)
newPreqModDataForm =
  formDefWithID "Controller.ModData.newPreqModDataForm" readData
    selectPreqModuleFormView
 where
  readData = toFormReader $ do
    mdata <- getModDataFromURL
    allmods <- runJustT queryAllModDataShortInfo
    let selmods = mergeSortBy (\m1 m2 -> snd m1 <= snd m2)
                              (map mod2KeyShortView allmods)
    sinfo <- getUserSessionInfo
    return (sinfo, selmods, addPreqModDataController mdata)
   where
    mod2KeyShortView (key,code,name) = (key, code ++ ": " ++ name)

addPreqModDataController :: ModData -> Maybe ModDataID -> Controller
addPreqModDataController mdata Nothing =
  nextInProcessOr (redirectToShowModDataController mdata) Nothing
addPreqModDataController mdata (Just preqmodkey) =
  checkAuthorization (modDataOperationAllowed (UpdateEntity mdata)) $ \_ -> do
    tr <- runT $ do
            prerequisites <- getModDataModDatas mdata
            if preqmodkey `elem` map modDataKey prerequisites
              then return ()
              else newPrerequisites (modDataKey mdata) preqmodkey
    either (\ error -> displayError (showTError error))
           (\ _ -> do logEvent (NewPrerequisite mdata preqmodkey)
                      setPageMessage "Neue Voraussetzung hinzugefügt"
                      nextInProcessOr (redirectToShowModDataController mdata)
                                      Nothing
           )
           tr

-- A controller to delete prerequisites of a module.
editPreqModDataController :: ModData -> Controller
editPreqModDataController mdata =
 checkAuthorization (modDataOperationAllowed (UpdateEntity mdata)) $
  \sinfo -> do
    prerequisites <- runQ $ getModDataModDatas mdata
    let preqmods = mergeSortBy (\m1 m2 -> snd m1 <= snd m2)
                               (map mod2KeyShortView prerequisites)
    setParWuiStore wuiDeletePreqModulStore
                   (sinfo,mdata)
                   (map (\ (k,s) -> (k,s,False)) preqmods)
    return [formElem deletePreqModDataForm]
 where
  mod2KeyShortView md =
    (modDataKey md, modDataCode md ++ ": " ++ modDataNameG md)

-- A form to delete prerequisites of a module.
deletePreqModDataForm ::
  HtmlFormDef ((UserSessionInfo, ModData), WuiStore [(ModDataID,String,Bool)])
deletePreqModDataForm =
  pwui2FormDef "Controller.ModData.deletePreqModDataForm"
    wuiDeletePreqModulStore
    (\ (sinfo,_) -> wListDelete sinfo)
    (\ (_,mdata) -> deletePreqModDataController mdata)
    (\ (sinfo,mdata) ->
         renderWUI sinfo "Prerequisite selection" "Store"
           (showRoute mdata) ())
 where
  --- A WUI specification for a list of strings with a deletion option.
  wListDelete :: UserSessionInfo -> WuiSpec [(ModDataID,String,Bool)]
  wListDelete sinfo =
    wList (wTriple (wConstant (\_ -> bold [htxt $ t "Module: "]))
                   (wConstant htxt)
                   (wCheckBool [htxt $ t "delete as prerequisite"]))
   where t = translate sinfo

---- The data stored for executing the WUI form.
wuiDeletePreqModulStore ::
  Global (SessionStore ((UserSessionInfo, ModData),
                        WuiStore [(ModDataID,String,Bool)]))
wuiDeletePreqModulStore =
  global emptySessionStore (Persistent (inSessionDataDir "wuiDeletePreqModulStore"))

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
            nextInProcessOr (redirectToShowModDataController mdata) Nothing
         )
         tr

----------------------------------------------------------------------------
