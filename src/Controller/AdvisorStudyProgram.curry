module Controller.AdvisorStudyProgram
  ( mainAdvisorStudyProgramController, createAdvisorStudyProgramWuiForm
  , editAdvisorStudyProgramWuiForm, addCatModForm
  , showAllXmlAdvisorStudyPrograms, showXmlAdvisorStudyProgram
  )
 where

import System.PreludeHelpers

import Data.List
import Data.Time
import HTML.Base
import HTML.Styles.Bootstrap4
import HTML.Session
import HTML.WUI
import Network.URL ( string2urlencoded )

import Model.ConfigMDB ( getBaseURL )
import Config.EntityRoutes
import Controller.AdvisorModule
import System.Helpers
import System.Spicey
import Model.MDB
import Model.MDB.Queries
import View.AdvisorModule
import View.AdvisorStudyProgram
import View.StudyProgram
import System.SessionInfo
import System.Authorization
import System.AuthorizedActions
import System.MultiLang
import View.Category(leqCategory)
import Config.UserProcesses
import View.MDBEntitiesToHtml
import XML

------------------------------------------------------------------------------
-- Routing

--- Choose the controller for a AdvisorStudyProgram entity
--- according to the URL parameter.
mainAdvisorStudyProgramController :: Controller
mainAdvisorStudyProgramController =
  do args <- getControllerParams
     case args of
       [] -> listAdvisorStudyProgramController
       ["list"] -> listAdvisorStudyProgramController
       ["new"] -> newAdvisorStudyProgramController
       ["show",s] -> controllerOnKey s showAdvisorStudyProgramController
       ["edit",s] -> controllerOnKey s editAdvisorStudyProgramController
       ["visible",s] -> controllerOnKey s toggleVisibilityASPController
       ["delete",s] -> controllerOnKey s deleteAdvisorStudyProgramController
       ["destroy",s] -> controllerOnKey s destroyAdvisorStudyProgramController
       ["addcatmod",s,c] -> controllerOnKey s (addCatModKeyController c)
       ["deladvmod",s,am] -> controllerOnKey s (deleteAdvModKeyController am)
       ["destroyadvmod",s,am] ->
          controllerOnKey s (destroyAdvModKeyController am)
       _ -> displayUrlError

-------------------------------------------------------------------------
--- Shows a form to create a new AdvisorStudyProgram entity.
newAdvisorStudyProgramController :: Controller
newAdvisorStudyProgramController =
  checkAuthorization (advisorStudyProgramOperationAllowed NewEntity) $ \sinfo ->
   do allStudyPrograms <- runQ queryAllStudyPrograms
      allUsers <- runQ queryAllUsers
      csem  <- getCurrentSemester >>= return . nextSemester
      maybe
       (return [h1 [htxt "Illegal operation"]])
       (\user -> do
         setParWuiStore wuiCreateAdvisorStudyProgramStore
           (sinfo, snd csem, sortBy leqStudyProgram allStudyPrograms, allUsers)
           ("", "", snd csem, "", "", "", False,
            defaultStudyProgram allStudyPrograms, user)
         return [formElem createAdvisorStudyProgramWuiForm])
       (maybe Nothing
              (\ln -> find (\u -> userLogin u == ln) allUsers)
              (userLoginOfSession sinfo))
 where
  defaultStudyProgram allstudyprograms =
    maybe (head allstudyprograms)
          id
          -- Select MSc Inf study program as the default program:
          (find (\sp -> studyProgramProgKey sp == "MScI15") allstudyprograms)

type NewAdvisorStudyProgram =
       (String,String,Int,String,String,String,Bool,StudyProgram,User)

--- A WUI form to create a new AdvisorStudyProgram entity.
--- The default values for the fields are stored in the
--- `wuiCreateAdvisorStudyProgramStore`.
createAdvisorStudyProgramWuiForm ::
  HtmlFormDef ((UserSessionInfo,Int,[StudyProgram],[User]),
               WuiStore NewAdvisorStudyProgram)
createAdvisorStudyProgramWuiForm =
  pwui2FormDef
    "Controller.AdvisorStudyProgram.createAdvisorStudyProgramWuiForm"
    wuiCreateAdvisorStudyProgramStore
    (\ (sinfo,curyear,allstudyprograms,allusers) ->
         wAdvisorStudyProgram curyear True sinfo allstudyprograms allusers)
    (\_ entity -> checkAuthorization (advisorStudyProgramOperationAllowed NewEntity) $ \_ ->
                  transactionBindController
                    (createAdvisorStudyProgramT entity)
                    (\asp -> logEvent (NewAdvisorStudyProgram asp) >>
                             redirectToAdvisorStudyProgramController asp))
    (\ (sinfo,_,_,_) ->
         renderWUI sinfo "Neues Studienprogramm" "Studienprogramm anlegen"
                   "?" ())

---- The data stored for executing the WUI form.
wuiCreateAdvisorStudyProgramStore ::
  SessionStore ((UserSessionInfo,Int,[StudyProgram],[User]),
                        WuiStore NewAdvisorStudyProgram)
wuiCreateAdvisorStudyProgramStore =
  sessionStore "wuiCreateAdvisorStudyProgramStore"

--- Transaction to persist a new AdvisorStudyProgram entity to the database.
createAdvisorStudyProgramT ::
  NewAdvisorStudyProgram -> DBAction AdvisorStudyProgram
createAdvisorStudyProgramT
    (name,term,year,desc,prereq,comments,visible,studyProgram,user) =
  newAdvisorStudyProgramWithUserStudyAdvisingKeyWithStudyProgramStudyProgramsAdvisedKey
   name
   term
   (Just year)
   desc
   prereq
   comments
   visible
   (userKey user)
   (studyProgramKey studyProgram)

-------------------------------------------------------------------------
--- Shows a form to edit the given AdvisorStudyProgram entity.
editAdvisorStudyProgramController :: AdvisorStudyProgram -> Controller
editAdvisorStudyProgramController advisorStudyProgramToEdit =
  checkAuthorization
   (advisorStudyProgramOperationAllowed
     (UpdateEntity advisorStudyProgramToEdit)) $ \sinfo ->
     do allStudyPrograms <- runQ queryAllStudyPrograms
        allUsers <- runQ queryAllUsers
        studyProgramsAdvisedStudyProgram <- runJustT
                                             (getStudyProgramsAdvisedStudyProgram
                                               advisorStudyProgramToEdit)
        studyAdvisingUser <- runJustT
                              (getStudyAdvisingUser advisorStudyProgramToEdit)
        csem <- getCurrentSemester
        setParWuiStore wuiEditAdvisorStudyProgramStore
           (sinfo, snd csem, advisorStudyProgramToEdit,
            studyProgramsAdvisedStudyProgram,
            sortBy leqStudyProgram allStudyPrograms,
            studyAdvisingUser, allUsers)
           advisorStudyProgramToEdit
        return [formElem editAdvisorStudyProgramWuiForm]

--- A WUI form to edit a given AdvisorStudyProgram entity.
editAdvisorStudyProgramWuiForm ::
  HtmlFormDef ((UserSessionInfo,Int,AdvisorStudyProgram,
                StudyProgram,[StudyProgram],User,[User]),
               WuiStore AdvisorStudyProgram)
editAdvisorStudyProgramWuiForm =
  pwui2FormDef
    "Controller.AdvisorStudyProgram.editAdvisorStudyProgramWuiForm"
    wuiEditAdvisorStudyProgramStore
    (\ (sinfo,curyear,advisorStudyProgram,studyprogram,allstudyprograms,
        user,allusers) ->
         wAdvisorStudyProgramType sinfo curyear advisorStudyProgram
                                  studyprogram user allstudyprograms allusers)
    (\_ entity -> checkAuthorization (advisorStudyProgramOperationAllowed
                                        (UpdateEntity entity)) $ \_ ->
                  transactionController
                    (updateAdvisorStudyProgramT entity)
                    (logEvent (UpdateAdvisorStudyProgram entity) >>
                     redirectToAdvisorStudyProgramController entity))
    (\ (sinfo,_,asp,_,_,_,_) ->
         renderWUI sinfo "Studienprogramm ändern" "Change" (showRoute asp) ())

---- The data stored for executing the WUI form.
wuiEditAdvisorStudyProgramStore ::
  SessionStore ((UserSessionInfo,Int,AdvisorStudyProgram,
                         StudyProgram,[StudyProgram],User,[User]),
                        WuiStore AdvisorStudyProgram)
wuiEditAdvisorStudyProgramStore = sessionStore "wuiEditAdvisorStudyProgramStore"


--- Transaction to persist modifications of a given AdvisorStudyProgram entity
--- to the database.
updateAdvisorStudyProgramT :: AdvisorStudyProgram -> DBAction ()
updateAdvisorStudyProgramT advisorStudyProgram =
  updateAdvisorStudyProgram advisorStudyProgram

--- Toggles the visibility of the given AdvisorStudyProgram entity.
toggleVisibilityASPController :: AdvisorStudyProgram -> Controller
toggleVisibilityASPController asp =
  checkAuthorization (advisorStudyProgramOperationAllowed  (UpdateEntity asp))
   $ \_ -> do
     let newasp = setAdvisorStudyProgramVisible asp
                    (not (advisorStudyProgramVisible asp))
     tr <- runT $ updateAdvisorStudyProgram newasp
     either (displayError . showTError)
            (\ _ -> logEvent (UpdateAdvisorStudyProgram newasp) >>
                    nextInProcessOr
                      (redirectToAdvisorStudyProgramController newasp)
                      Nothing)
            tr

------------------------------------------------------------------------------
--- Deletes a given AdvisorStudyProgram entity (after asking for confirmation)
--- and proceeds with the list controller.
deleteAdvisorStudyProgramController :: AdvisorStudyProgram -> Controller
deleteAdvisorStudyProgramController asprog =
  checkAuthorization
   (advisorStudyProgramOperationAllowed (DeleteEntity asprog)) $ \si ->
     confirmDeletionPage si $ concat
       [ "Studienprogramm \""
       , advisorStudyProgramToShortView asprog
       , "\" wirklich löschen?" ]

--- Deletes a given AdvisorStudyProgram entity
--- and proceeds with the list controller.
destroyAdvisorStudyProgramController :: AdvisorStudyProgram -> Controller
destroyAdvisorStudyProgramController asprog =
  checkAuthorization
   (advisorStudyProgramOperationAllowed (DeleteEntity asprog)) $ \_ ->
     (transactionController
        (deleteAdvisorStudyProgramT asprog)
        (logEvent (DeleteAdvisorStudyProgram asprog) >>
         listAdvisorStudyProgramController))

--- Transaction to delete a given AdvisorStudyProgram entity.
deleteAdvisorStudyProgramT :: AdvisorStudyProgram -> DBAction ()
deleteAdvisorStudyProgramT advisorStudyProgram =
  deleteAdvisorStudyProgram advisorStudyProgram

------------------------------------------------------------------------------
--- Lists all AdvisorStudyProgram entities with buttons to show, delete,
--- or edit an entity.
listAdvisorStudyProgramController :: Controller
listAdvisorStudyProgramController =
  checkAuthorization (advisorStudyProgramOperationAllowed ListEntities)
   $ \sinfo -> do
       let visfilter = filter (maybe advisorStudyProgramVisible
                                     (\_ -> const True)
                                     (userLoginOfSession sinfo))
       asprogs <- runQ queryAllAdvisorStudyPrograms >>= return . visfilter
       sprogs <- runJustT (mapM getStudyProgramsAdvisedStudyProgram asprogs)
       return (listAdvisorStudyProgramView sinfo (zip asprogs sprogs))

-------------------------------------------------------------------------
--- Shows a form to add a module to a AdvisorStudyProgram entity.
addCatModKeyController :: String -> AdvisorStudyProgram -> Controller
addCatModKeyController catkey asprog =
  maybe displayUrlError
        (\catid -> runJustT (getCategory catid) >>= addCatModController asprog)
        (readCategoryKey catkey)

--- Shows a form to add a module to a AdvisorStudyProgram entity.
addCatModController :: AdvisorStudyProgram -> Category -> Controller
addCatModController asprog cat =
  checkAuthorization
    (advisorStudyProgramOperationAllowed (UpdateEntity asprog)) $ \sinfo -> do
    modinstdatas <- runJustT (getCatModInstsInSemesters cat startsem 3)
    setParWuiStore wuiAddCatModFormStore
                   (sinfo, asprog,modinstdatas)
                   (False, head modinstdatas, cat)
    return [formElem addCatModForm]
 where
  startsem = (advisorStudyProgramTerm asprog,
              advisorStudyProgramYear asprog)

--- A WUI form to edit a given AdvisorStudyProgram entity.
addCatModForm ::
  HtmlFormDef ((UserSessionInfo,AdvisorStudyProgram,[(ModInst,ModData)]),
               WuiStore (Bool,(ModInst,ModData),Category))
addCatModForm =
  pwui2FormDef
    "Controller.AdvisorStudyProgram.addCatModForm"
    wuiAddCatModFormStore
    (\ (_,_,possibleModInsts) -> wSelectAdvisorModule possibleModInsts)
    (\ (_,asp,_) entity ->
        transactionController
          (let (c1,(c2,_),c3) = entity
           in createAdvisorModuleT (c1,c2,c3,asp))
          (redirectToAdvisorStudyProgramController asp))
    (\ (sinfo,asp,_) ->
         renderWUI sinfo "Neues Modul im Studienprogramm" "Hinzufügen"
                   (showRoute asp) ())

---- The data stored for executing the WUI form.
wuiAddCatModFormStore ::
  SessionStore
            ((UserSessionInfo,AdvisorStudyProgram,[(ModInst,ModData)]),
             WuiStore (Bool,(ModInst,ModData),Category))
wuiAddCatModFormStore = sessionStore "wuiAddCatModFormStore"

-------------------------------------------------------------------------
--- Deletes an advisor module from a given AdvisorStudyProgram entity
--- (after asking for confirmation)
--- and proceeds with the show AdvisorStudyProgram controller.
deleteAdvModKeyController :: String -> AdvisorStudyProgram -> Controller
deleteAdvModKeyController _ asprog =
  checkAuthorization
    (advisorStudyProgramOperationAllowed (UpdateEntity asprog)) $ \sinfo -> do
      (entity,ctrlargs) <- getControllerURL
      case ctrlargs of
        [_,s,am] -> confirmDeletionPageWithRefs sinfo
          ("Modulempfehlung im Programm \"" ++ advisorStudyProgramName asprog ++
           "\" wirklich löschen?")
          (showControllerURL entity ["destroyadvmod",s,am])
          (showControllerURL entity ["show",s])
        _ -> displayUrlError

--- Deletes an advisor module from a given AdvisorStudyProgram entity
--- and proceeds with the show AdvisorStudyProgram controller.
destroyAdvModKeyController :: String -> AdvisorStudyProgram -> Controller
destroyAdvModKeyController amkeys asprog =
  checkAuthorization
    (advisorStudyProgramOperationAllowed (UpdateEntity asprog)) $ \_ ->
       maybe displayUrlError
             (\amkey -> transactionController (getAdvisorModule amkey >>=
                                               deleteAdvisorModuleT)
                          (redirectToAdvisorStudyProgramController asprog))
             (readAdvisorModuleKey amkeys)


-------------------------------------------------------------------------
--- Redirect to a page showing the given ModData entity.
--- Useful to set the URL route correctly.
redirectToAdvisorStudyProgramController :: AdvisorStudyProgram -> Controller
redirectToAdvisorStudyProgramController = redirectController . showRoute

--- Shows a AdvisorStudyProgram entity.
showAdvisorStudyProgramController :: AdvisorStudyProgram -> Controller
showAdvisorStudyProgramController asprog =
  checkAuthorization
   (advisorStudyProgramOperationAllowed (ShowEntity asprog))
   $ (\sinfo ->
     do studyprog <- runJustT (getStudyProgramsAdvisedStudyProgram asprog)
        categories <- runQ $ fmap (sortBy leqCategory) $
                               queryCategorysOfStudyProgram (studyProgramKey studyprog)
        amods <- runQ $ queryCondAdvisorModule
           (\am -> advisorModuleAdvisorStudyProgramAdvisorProgramModulesKey am
                                              == advisorStudyProgramKey asprog)
        amdatas <- runJustT (getAdvisorModuleData amods)
        advisor <- runJustT (getStudyAdvisingUser asprog)
        baseurl <- getBaseURL
        return
         (showAdvisorStudyProgramView
           sinfo (isAdminSession sinfo)
           (Just (userLogin advisor) == userLoginOfSession sinfo)
           addCatModRef delAdvModRef asprog (xmlURL baseurl asprog)
           studyprog amdatas categories advisor))
 where
  addCatModRef cat = hrefPrimBadge ("?AdvisorStudyProgram/addcatmod/" ++
                             showAdvisorStudyProgramKey asprog ++ "/" ++
                             showCategoryKey cat)
                            [htxt "Modulempfehlung hinzufügen"]

  delAdvModRef am  = hrefPrimBadge ("?AdvisorStudyProgram/deladvmod/" ++
                             showAdvisorStudyProgramKey asprog ++ "/" ++
                             showAdvisorModuleKey am)
                            [htxt "Modulempfehlung löschen"]

--- Gets the associated StudyProgram entity for a given AdvisorStudyProgram entity.
getStudyProgramsAdvisedStudyProgram
  :: AdvisorStudyProgram -> DBAction StudyProgram
getStudyProgramsAdvisedStudyProgram aStudyProgram =
  getStudyProgram
   (advisorStudyProgramStudyProgramStudyProgramsAdvisedKey aStudyProgram)

--- Gets the associated User entity for a given AdvisorStudyProgram entity.
getStudyAdvisingUser :: AdvisorStudyProgram -> DBAction User
getStudyAdvisingUser aUser =
  getUser (advisorStudyProgramUserStudyAdvisingKey aUser)

-------------------------------------------------------------------------
-- Formatting master programs as XML documents:

-- XML URL of an AdvisorStudyProgram:
xmlURL :: String -> AdvisorStudyProgram -> String
xmlURL baseurl asp =
  baseurl ++ "?xmlaprog=" ++ string2urlencoded (showAdvisorStudyProgramKey asp)

-- URL of an AdvisorStudyProgram:
advisorProgURL :: String -> AdvisorStudyProgram -> String
advisorProgURL baseurl asp =
  baseurl ++ "?AdvisorStudyProgram/show/" ++
  string2urlencoded (showAdvisorStudyProgramKey asp)

-- Show XML document containing all visible master programs
showAllXmlAdvisorStudyPrograms :: IO HtmlPage
showAllXmlAdvisorStudyPrograms = do
  allasprogs <- runQ $ fmap (filter advisorStudyProgramVisible)
                             queryAllAdvisorStudyPrograms
  aspxmls <- mapM getAdvisorStudyProgramAsXML allasprogs
  return (HtmlAnswer "text/xml"
                     (showXmlDoc (xml "studyprograms" aspxmls)))

showXmlAdvisorStudyProgram :: AdvisorStudyProgramID -> IO HtmlPage
showXmlAdvisorStudyProgram aspkey = do
  asprog <- runJustT $ getAdvisorStudyProgram aspkey
  xmldoc <- getAdvisorStudyProgramAsXML asprog
  return (HtmlAnswer "text/xml" (showXmlDoc xmldoc))

getAdvisorStudyProgramAsXML :: AdvisorStudyProgram -> IO XmlExp
getAdvisorStudyProgramAsXML asprog = do
  studyprog <- runJustT (getStudyProgramsAdvisedStudyProgram asprog)
  categories <- runQ $ fmap (sortBy leqCategory) $
                         queryCategorysOfStudyProgram (studyProgramKey studyprog)
  amods <- runQ $ queryCondAdvisorModule
                       (\am -> advisorModuleAdvisorStudyProgramAdvisorProgramModulesKey am
                                                          == advisorStudyProgramKey asprog)
  amdatas <- runJustT (getAdvisorModuleData amods)
  advisor <- runJustT (getStudyAdvisingUser asprog)
  baseurl <- getBaseURL
  return (asprog2xml baseurl asprog advisor studyprog categories amdatas)

asprog2xml :: String -> AdvisorStudyProgram -> User -> StudyProgram
           -> [Category] -> [(AdvisorModule,ModInst,ModData)]
           -> XmlExp
asprog2xml baseurl asprog advisor sprog cats amdatas  =
  XElem "studyprogram" [("ID",showAdvisorStudyProgramKey asprog)] $
   [xml "title"         [xtxt (advisorStudyProgramName asprog)]
   ,xml "advisor"       [xtxt (userToShortView advisor)]
   ,xml "start"         [xtxt (showSemester (startSem,startYear))]
   ,xml "url"           [xtxt (advisorProgURL baseurl asprog)]
   ,xml "description"   [xtxt (advisorStudyProgramDesc asprog)]
   ,xml "prerequisites" [xtxt (advisorStudyProgramPrereq asprog)]
   ,xml "comments"      [xtxt (advisorStudyProgramComments asprog)]
   ,XElem "degreeprogram" [("key",studyProgKey)] [xtxt (studyProgramName sprog)]
   ] ++
   concatMap (\c -> let camods = filter (isAdvisorModuleOfCat c) amdatas
                    in if null camods then []
                                      else map (advisorModuleData2xml c) camods)
             cats
 where
  studyProgKey = studyProgramProgKey sprog
  
  isAdvisorModuleOfCat cat (am,_,_) =
    advisorModuleCategoryAdvisorCategorizingKey am == categoryKey cat

  advisorModuleData2xml cat (am,modinst,md) =
    xml "lecture"
     [xml "mandatory"  [xtxt $ if advisorModuleMandatory am then "yes" else "no"]
     ,XElem "category" [("key", studyProgKey ++ "_" ++ categoryShortName cat)]
                       [xtxt (categoryName cat)]
     ,xml "code"       [xtxt (modDataCode md)]
     ,xml "semester"   [xtxt (showSemester (modInstSemester modinst))]
     ]

  startSem  = advisorStudyProgramTerm asprog
  startYear = advisorStudyProgramYear asprog

-------------------------------------------------------------------------
