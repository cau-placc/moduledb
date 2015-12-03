module AdvisorStudyProgramController ( mainAdvisorStudyProgramController )
where

import Spicey
import KeyDatabase
import HTML
import List(find)
import Time
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
             transactionBindController (createAdvisorStudyProgramT entity)
                                       showAdvisorStudyProgramController)
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
             transactionController (updateAdvisorStudyProgramT entity)
                                   showctrl)
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
deleteAdvisorStudyProgramController advisorStudyProgram =
  checkAuthorization
   (advisorStudyProgramOperationAllowed (DeleteEntity advisorStudyProgram))
   $ (\_ ->
     confirmController
      [h3
        [htxt
          (concat
            ["Studienprogramm \""
            ,advisorStudyProgramToShortView advisorStudyProgram
            ,"\" wirklich löschen?"])]]
      (transactionController (deleteAdvisorStudyProgramT advisorStudyProgram)
        listAdvisorStudyProgramController)
      (showAdvisorStudyProgramController advisorStudyProgram))

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
     do studyprog <- runJustT (getStudyProgramsAdvisedStudyProgram
                                               asprog)
        categories <- runQ $ transformQ (mergeSort leqCategory) $
                               queryCategorysOfStudyProgram
                                           (studyProgramKey studyprog)
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
           asprog
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