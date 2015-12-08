module AdvisorStudyProgramController
        ( mainAdvisorStudyProgramController
        , showAllXmlAdvisorStudyPrograms, showXmlAdvisorStudyProgram
        )
where

import Helpers
import Spicey
import KeyDatabase
import HTML
import List(find)
import Time
import ConfigMDB(baseURL)
import MDB
import MDBExts
import AdvisorModuleController
import AdvisorStudyProgramView
import StudyProgramView
import Maybe
import SessionInfo
import Sort(mergeSort)
import Authorization
import AuthorizedControllers
import CategoryView(leqCategory)
import MDBExts
import UserProcesses
import MDBEntitiesToHtml
import XML

--- Choose the controller for a AdvisorStudyProgram entity according to the URL parameter.
mainAdvisorStudyProgramController :: Controller
mainAdvisorStudyProgramController =
  do args <- getControllerParams
     case args of
       [] -> listAdvisorStudyProgramController
       ["list"] -> listAdvisorStudyProgramController
       ["new"] -> newAdvisorStudyProgramController
       ["show",s] ->
         applyControllerOn (readAdvisorStudyProgramKey s)
          getAdvisorStudyProgram
          showAdvisorStudyProgramController
       ["edit",s] ->
         applyControllerOn (readAdvisorStudyProgramKey s)
          getAdvisorStudyProgram
          editAdvisorStudyProgramController
       ["delete",s] ->
         applyControllerOn (readAdvisorStudyProgramKey s)
          getAdvisorStudyProgram
          deleteAdvisorStudyProgramController
       _ -> displayError "Illegal URL"

--- Shows a form to create a new AdvisorStudyProgram entity.
newAdvisorStudyProgramController :: Controller
newAdvisorStudyProgramController =
  checkAuthorization (advisorStudyProgramOperationAllowed NewEntity) $ \sinfo ->
   do allStudyPrograms <- runQ queryAllStudyPrograms
      allUsers <- runQ queryAllUsers
      return $ maybe
       [h1 [htxt "Illegal operation"]]
       (\user ->
         blankAdvisorStudyProgramView sinfo
           (mergeSort leqStudyProgram allStudyPrograms)
           user allUsers
           (\entity ->
             transactionBindController
               (createAdvisorStudyProgramT entity)
               (\asp -> logEvent (NewAdvisorStudyProgram asp) >>
                        showAdvisorStudyProgramController asp))
           listAdvisorStudyProgramController)
       (maybe Nothing
              (\ln -> find (\u -> userLogin u == ln) allUsers)
              (userLoginOfSession sinfo))

--- Transaction to persist a new AdvisorStudyProgram entity to the database.
createAdvisorStudyProgramT
  :: (String,String,Int,String,String,String,Bool,StudyProgram,User)
  -> Transaction AdvisorStudyProgram
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

--- Shows a form to edit the given AdvisorStudyProgram entity.
editAdvisorStudyProgramController :: AdvisorStudyProgram -> Controller
editAdvisorStudyProgramController advisorStudyProgramToEdit =
  checkAuthorization
   (advisorStudyProgramOperationAllowed
     (UpdateEntity advisorStudyProgramToEdit))
   $ (\sinfo ->
     do allStudyPrograms <- runQ queryAllStudyPrograms
        allUsers <- runQ queryAllUsers
        studyProgramsAdvisedStudyProgram <- runJustT
                                             (getStudyProgramsAdvisedStudyProgram
                                               advisorStudyProgramToEdit)
        studyAdvisingUser <- runJustT
                              (getStudyAdvisingUser advisorStudyProgramToEdit)
        let aspkey = advisorStudyProgramKey advisorStudyProgramToEdit
            showctrl = showASPController aspkey
        return
         (editAdvisorStudyProgramView sinfo advisorStudyProgramToEdit
           studyProgramsAdvisedStudyProgram
           studyAdvisingUser
           allStudyPrograms
           allUsers
           (\entity ->
             transactionController
               (updateAdvisorStudyProgramT entity)
               (logEvent (UpdateAdvisorStudyProgram entity) >> showctrl))
           showctrl))

--- A show controller for a given AdvisorStudyProgram key:
showASPController :: AdvisorStudyProgramKey -> Controller
showASPController aspkey =
  runJustT (getAdvisorStudyProgram aspkey)
                       >>= showAdvisorStudyProgramController

--- Transaction to persist modifications of a given AdvisorStudyProgram entity
--- to the database.
updateAdvisorStudyProgramT :: AdvisorStudyProgram -> Transaction ()
updateAdvisorStudyProgramT advisorStudyProgram =
  updateAdvisorStudyProgram advisorStudyProgram

--- Deletes a given AdvisorStudyProgram entity (after asking for confirmation)
--- and proceeds with the list controller.
deleteAdvisorStudyProgramController :: AdvisorStudyProgram -> Controller
deleteAdvisorStudyProgramController asprog =
  checkAuthorization
   (advisorStudyProgramOperationAllowed (DeleteEntity asprog))
   $ (\_ ->
     confirmController
      [h3
        [htxt
          (concat
            ["Studienprogramm \""
            ,advisorStudyProgramToShortView asprog
            ,"\" wirklich löschen?"])]]
      (transactionController
        (deleteAdvisorStudyProgramT asprog)
        (logEvent (DeleteAdvisorStudyProgram asprog) >>
         listAdvisorStudyProgramController))
      (showAdvisorStudyProgramController asprog))

--- Transaction to delete a given AdvisorStudyProgram entity.
deleteAdvisorStudyProgramT :: AdvisorStudyProgram -> Transaction ()
deleteAdvisorStudyProgramT advisorStudyProgram =
  deleteAdvisorStudyProgram advisorStudyProgram

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
       sprogs <- runJustT (mapT getStudyProgramsAdvisedStudyProgram asprogs)
       return (listAdvisorStudyProgramView sinfo (zip asprogs sprogs))

--- Shows a AdvisorStudyProgram entity.
addCatModController :: AdvisorStudyProgram -> Controller
                    -> Category -> Controller
addCatModController asprog nextctrl cat = do
  modinstdatas <- runJustT (getCatModInstsInSemesters cat startsem 3)
  selectAdvisorModuleController asprog cat modinstdatas nextctrl
 where
  startsem = (advisorStudyProgramTerm asprog,
              advisorStudyProgramYear asprog)

--- Shows a AdvisorStudyProgram entity.
showAdvisorStudyProgramController :: AdvisorStudyProgram -> Controller
showAdvisorStudyProgramController asprog =
  checkAuthorization
   (advisorStudyProgramOperationAllowed (ShowEntity asprog))
   $ (\sinfo ->
     do studyprog <- runJustT (getStudyProgramsAdvisedStudyProgram asprog)
        categories <- runQ $ transformQ (mergeSort leqCategory) $
                               queryCategorysOfStudyProgram (studyProgramKey studyprog)
        amods <- runQ $ queryCondAdvisorModule
           (\am -> advisorModuleAdvisorStudyProgramAdvisorProgramModulesKey am
                                              == advisorStudyProgramKey asprog)
        amdatas <- runJustT (getAdvisorModuleData amods)
        advisor <- runJustT (getStudyAdvisingUser asprog)
        let showcontroller =
              showASPController (advisorStudyProgramKey asprog)
        return
         (showAdvisorStudyProgramView
           sinfo
           (isAdminSession sinfo)
           (Just (userLogin advisor) == userLoginOfSession sinfo)
           editAdvisorStudyProgramController
           showcontroller
           (addCatModController asprog showcontroller)
           (deleteAdvisorModuleController showcontroller)
           asprog (xmlURL asprog)
           studyprog
           amdatas
           categories
           advisor))

--- Gets the associated StudyProgram entity for a given AdvisorStudyProgram entity.
getStudyProgramsAdvisedStudyProgram
  :: AdvisorStudyProgram -> Transaction StudyProgram
getStudyProgramsAdvisedStudyProgram aStudyProgram =
  getStudyProgram
   (advisorStudyProgramStudyProgramStudyProgramsAdvisedKey aStudyProgram)

--- Gets the associated User entity for a given AdvisorStudyProgram entity.
getStudyAdvisingUser :: AdvisorStudyProgram -> Transaction User
getStudyAdvisingUser aUser =
  getUser (advisorStudyProgramUserStudyAdvisingKey aUser)

-------------------------------------------------------------------------
-- Formatting master programs as XML documents:

-- XML URL of an AdvisorStudyProgram:
xmlURL :: AdvisorStudyProgram -> String
xmlURL asp = baseURL++"?xmlaprog="++string2urlencoded (showAdvisorStudyProgramKey asp)

-- URL of an AdvisorStudyProgram:
advisorProgURL :: AdvisorStudyProgram -> String
advisorProgURL asp =
  baseURL++"?AdvisorStudyProgram/show/"++string2urlencoded (showAdvisorStudyProgramKey asp)

-- Show XML document containing all visible master programs
showAllXmlAdvisorStudyPrograms :: IO HtmlForm
showAllXmlAdvisorStudyPrograms = do
  allasprogs <- runQ $ transformQ (filter advisorStudyProgramVisible)
                                 queryAllAdvisorStudyPrograms
  aspxmls <- mapIO getAdvisorStudyProgramAsXML allasprogs
  return (HtmlAnswer "text/xml"
                     (showXmlDoc (xml "studyprograms" aspxmls)))

showXmlAdvisorStudyProgram :: AdvisorStudyProgramKey -> IO HtmlForm
showXmlAdvisorStudyProgram aspkey = do
  asprog <- runJustT $ getAdvisorStudyProgram aspkey
  xmldoc <- getAdvisorStudyProgramAsXML asprog
  return (HtmlAnswer "text/xml" (showXmlDoc xmldoc))

getAdvisorStudyProgramAsXML :: AdvisorStudyProgram -> IO XmlExp
getAdvisorStudyProgramAsXML asprog = do
  studyprog <- runJustT (getStudyProgramsAdvisedStudyProgram asprog)
  categories <- runQ $ transformQ (mergeSort leqCategory) $
                         queryCategorysOfStudyProgram (studyProgramKey studyprog)
  amods <- runQ $ queryCondAdvisorModule
                       (\am -> advisorModuleAdvisorStudyProgramAdvisorProgramModulesKey am
                                                          == advisorStudyProgramKey asprog)
  amdatas <- runJustT (getAdvisorModuleData amods)
  advisor <- runJustT (getStudyAdvisingUser asprog)
  return (asprog2xml asprog advisor studyprog categories amdatas)

asprog2xml :: AdvisorStudyProgram -> User -> StudyProgram -> [Category]
           -> [(AdvisorModule,ModInst,ModData)]
           -> XmlExp
asprog2xml asprog advisor sprog cats amdatas  =
  XElem "studyprogram" [("ID",showAdvisorStudyProgramKey asprog)] $
   [xml "title"         [xtxt (advisorStudyProgramName asprog)]
   ,xml "advisor"       [xtxt (userToShortView advisor)]
   ,xml "start"         [xtxt (showSemester (startSem,startYear))]
   ,xml "url"           [xtxt (advisorProgURL asprog)]
   ,xml "description"   [xtxt (advisorStudyProgramDesc asprog)]
   ,xml "prerequisites" [xtxt (advisorStudyProgramPrereq asprog)]
   ,xml "comments"      [xtxt (advisorStudyProgramComments asprog)]
   ,XElem "degreeprogram" [("key",studyProgKey)] [xtxt (studyProgramName sprog)]
   ] ++
   concatMap (\c -> let camods = filter (isAdvisorModuleOfCat c) amdatas
                    in  if null camods then [] else map (advisorModuleData2xml c) camods)
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
