module AuthorizedControllers where

import Authorization
import MDB
import MDBExts
import KeyDatabase
import Authentication
import SessionInfo

--- Grants access for the administrator.
checkAdmin :: UserSessionInfo -> IO AccessResult
checkAdmin sinfo =
  return $ if isAdminSession sinfo
             then AccessGranted
             else AccessDenied "Operation only allowed for admin!"

--- Grants access if somebody is logged in.
isLoggedIn :: IO AccessResult
isLoggedIn =
  getSessionLogin >>=
  return . maybe (AccessDenied "Operation not allowed!") (const AccessGranted)

--- Checks whether the application of an operation to a StudyProgram
--- entity is allowed.
studyProgramOperationAllowed :: AccessType StudyProgram -> UserSessionInfo -> IO AccessResult
studyProgramOperationAllowed at sinfo =
  case at of
   ListEntities   -> return AccessGranted
   (ShowEntity _) -> return AccessGranted
   _              -> checkAdmin sinfo

--- Checks whether the application of an operation to a Category
--- entity is allowed.
categoryOperationAllowed :: AccessType Category -> UserSessionInfo -> IO AccessResult
categoryOperationAllowed at sinfo =
  case at of
   ListEntities   -> return AccessGranted
   (ShowEntity _) -> return AccessGranted
   _              -> checkAdmin sinfo

--- Checks whether the application of an operation to a MasterCoreArea
--- entity is allowed.
masterCoreAreaOperationAllowed :: AccessType MasterCoreArea -> UserSessionInfo -> IO AccessResult
masterCoreAreaOperationAllowed at sinfo =
  case at of
   ListEntities   -> return AccessGranted
   (ShowEntity _) -> return AccessGranted
   _              -> checkAdmin sinfo

--- Checks whether the application of an operation to a User
--- entity is allowed.
userOperationAllowed :: AccessType User -> UserSessionInfo -> IO AccessResult
userOperationAllowed _ = checkAdmin

--- Checks whether the application of an operation to a ModData
--- entity is allowed.
modDataOperationAllowed :: AccessType ModData -> UserSessionInfo -> IO AccessResult
modDataOperationAllowed at sinfo = do
  case at of
   ListEntities -> return AccessGranted
   NewEntity -> checkAdmin sinfo
   (ShowEntity _) -> return AccessGranted
   (DeleteEntity _) -> checkAdmin sinfo
   (UpdateEntity md) -> isAdminOrOwner md

isAdminOrOwner :: ModData -> IO AccessResult
isAdminOrOwner mdata = do
  admin <- isAdmin
  responsibleUser <- runJustT (getResponsibleUser mdata)
  lname <- getSessionLogin >>= return . maybe "" id
  return $ if admin || userLogin responsibleUser == lname
           then AccessGranted
           else AccessDenied "Operation not allowed!"


--- Checks whether the application of an operation to a ModDescr
--- entity is allowed.
modDescrOperationAllowed :: AccessType ModDescr -> UserSessionInfo -> IO AccessResult
modDescrOperationAllowed at _ =
  case at of
   ListEntities -> return AccessGranted
   NewEntity -> return AccessGranted
   (ShowEntity _) -> return AccessGranted
   (DeleteEntity _) -> return AccessGranted
   (UpdateEntity _) -> return AccessGranted

--- Checks whether the application of an operation to a ModInst
--- entity is allowed.
modInstOperationAllowed :: AccessType ModInst -> UserSessionInfo -> IO AccessResult
modInstOperationAllowed at _ =
  case at of
   ListEntities -> return AccessGranted
   (ShowEntity _) -> return AccessGranted
   NewEntity -> return AccessGranted
   (DeleteEntity _) -> return AccessGranted
   (UpdateEntity _) -> return AccessGranted

--- Checks whether the application of an operation to a AdvisorStudyProgram
--- entity is allowed.
advisorStudyProgramOperationAllowed
  :: AccessType AdvisorStudyProgram -> UserSessionInfo -> IO AccessResult
advisorStudyProgramOperationAllowed at sinfo =
  case at of
   ListEntities -> return AccessGranted
   NewEntity -> isLoggedIn
   (ShowEntity _) -> return AccessGranted
   (DeleteEntity _) -> checkAdmin sinfo
   (UpdateEntity asp) -> isAdminOrProgAdvisor asp
 where
  isAdminOrProgAdvisor asp = do
    responsibleUser <- runJustT (getStudyProgAdvisorUser asp)
    lname <- getSessionLogin >>= return . maybe "" id
    return $ if isAdminSession sinfo || userLogin responsibleUser == lname
             then AccessGranted
             else AccessDenied "Operation not allowed!"

--- Checks whether the application of an operation to a AdvisorModule
--- entity is allowed.
advisorModuleOperationAllowed
  :: AccessType AdvisorModule -> UserSessionInfo -> IO AccessResult
advisorModuleOperationAllowed _ _ = isLoggedIn

--- Checks whether the application of an operation to a MasterProgram
--- entity is allowed.
masterProgramOperationAllowed :: AccessType MasterProgram -> UserSessionInfo
                              -> IO AccessResult
masterProgramOperationAllowed at sinfo =
  case at of
   ListEntities -> return AccessGranted
   NewEntity -> isLoggedIn
   (ShowEntity _) -> return AccessGranted
   (DeleteEntity _) -> checkAdmin sinfo
   (UpdateEntity mp) -> isAdminOrAdvisor mp

isAdminOrAdvisor :: MasterProgram -> IO AccessResult
isAdminOrAdvisor mprog = do
  admin <- isAdmin
  responsibleUser <- runJustT (getAdvisingUser mprog)
  lname <- getSessionLogin >>= return . maybe "" id
  return $ if admin || userLogin responsibleUser == lname
           then AccessGranted
           else AccessDenied "Operation not allowed!"


--- Checks whether the application of an operation to a MasterProgInfo
--- entity is allowed.
masterProgInfoOperationAllowed :: AccessType MasterProgInfo -> UserSessionInfo -> IO AccessResult
masterProgInfoOperationAllowed at sinfo =
  case at of
   ListEntities -> return AccessGranted
   NewEntity -> isLoggedIn
   (ShowEntity _) -> return AccessGranted
   (DeleteEntity _) -> checkAdmin sinfo
   (UpdateEntity _) -> return AccessGranted

--- Checks whether the application of an operation to a UnivisInfo
--- entity is allowed.
univisInfoOperationAllowed :: AccessType UnivisInfo -> UserSessionInfo -> IO AccessResult
univisInfoOperationAllowed _ = checkAdmin
