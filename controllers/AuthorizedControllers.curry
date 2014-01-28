module AuthorizedControllers where

import Authorization
import MDB
import MDBExts
import KeyDatabase
import Authentication

--- Grants access for the administrator.
checkAdmin :: IO AccessResult
checkAdmin = do
  admin <- isAdmin
  return $ if admin then AccessGranted
                    else AccessDenied "Operation only allowed for admin!"

--- Grants access if somebody is logged in.
isLoggedIn :: IO AccessResult
isLoggedIn =
  getSessionLogin >>=
  return . maybe (AccessDenied "Operation only allowed!") (const AccessGranted)

--- Checks whether the application of an operation to a StudyProgram
--- entity is allowed.
studyProgramOperationAllowed :: AccessType StudyProgram -> IO AccessResult
studyProgramOperationAllowed at =
  case at of
   ListEntities   -> return AccessGranted
   (ShowEntity _) -> return AccessGranted
   _              -> checkAdmin

--- Checks whether the application of an operation to a Category
--- entity is allowed.
categoryOperationAllowed :: AccessType Category -> IO AccessResult
categoryOperationAllowed at =
  case at of
   ListEntities   -> return AccessGranted
   (ShowEntity _) -> return AccessGranted
   _              -> checkAdmin

--- Checks whether the application of an operation to a MasterCoreArea
--- entity is allowed.
masterCoreAreaOperationAllowed :: AccessType MasterCoreArea -> IO AccessResult
masterCoreAreaOperationAllowed at =
  case at of
   ListEntities   -> return AccessGranted
   (ShowEntity _) -> return AccessGranted
   _              -> checkAdmin

--- Checks whether the application of an operation to a User
--- entity is allowed.
userOperationAllowed :: AccessType User -> IO AccessResult
userOperationAllowed _ = checkAdmin

--- Checks whether the application of an operation to a ModData
--- entity is allowed.
modDataOperationAllowed :: AccessType ModData -> IO AccessResult
modDataOperationAllowed at = do
  case at of
   ListEntities -> return AccessGranted
   NewEntity -> checkAdmin
   (ShowEntity _) -> return AccessGranted
   (DeleteEntity _) -> checkAdmin
   (UpdateEntity md) -> isAdminOrOwner md

isAdminOrOwner :: ModData -> IO AccessResult
isAdminOrOwner mdata = do
  admin <- isAdmin
  responsibleUser <- runJustT (getResponsibleUser mdata)
  lname <- getSessionLogin >>= return . maybe "" id
  return $ if admin || userLogin responsibleUser == lname
           then AccessGranted
           else AccessDenied "Operation only allowed!"


--- Checks whether the application of an operation to a ModDescr
--- entity is allowed.
modDescrOperationAllowed :: AccessType ModDescr -> IO AccessResult
modDescrOperationAllowed at =
  case at of
   ListEntities -> return AccessGranted
   NewEntity -> return AccessGranted
   (ShowEntity _) -> return AccessGranted
   (DeleteEntity _) -> return AccessGranted
   (UpdateEntity _) -> return AccessGranted

--- Checks whether the application of an operation to a ModInst
--- entity is allowed.
modInstOperationAllowed :: AccessType ModInst -> IO AccessResult
modInstOperationAllowed at =
  case at of
   ListEntities -> return AccessGranted
   (ShowEntity _) -> return AccessGranted
   NewEntity -> return AccessGranted
   (DeleteEntity _) -> return AccessGranted
   (UpdateEntity _) -> return AccessGranted

--- Checks whether the application of an operation to a MasterProgram
--- entity is allowed.
masterProgramOperationAllowed :: AccessType MasterProgram -> IO AccessResult
masterProgramOperationAllowed at =
  case at of
   ListEntities -> return AccessGranted
   NewEntity -> isLoggedIn
   (ShowEntity _) -> return AccessGranted
   (DeleteEntity _) -> checkAdmin
   (UpdateEntity mp) -> isAdminOrAdvisor mp

isAdminOrAdvisor :: MasterProgram -> IO AccessResult
isAdminOrAdvisor mprog = do
  admin <- isAdmin
  responsibleUser <- runJustT (getAdvisingUser mprog)
  lname <- getSessionLogin >>= return . maybe "" id
  return $ if admin || userLogin responsibleUser == lname
           then AccessGranted
           else AccessDenied "Operation only allowed!"


--- Checks whether the application of an operation to a MasterProgInfo
--- entity is allowed.
masterProgInfoOperationAllowed :: AccessType MasterProgInfo -> IO AccessResult
masterProgInfoOperationAllowed at =
  case at of
   ListEntities -> return AccessGranted
   NewEntity -> isLoggedIn
   (ShowEntity _) -> return AccessGranted
   (DeleteEntity _) -> checkAdmin
   (UpdateEntity _) -> return AccessGranted

--- Checks whether the application of an operation to a UnivisInfo
--- entity is allowed.
univisInfoOperationAllowed :: AccessType UnivisInfo -> IO AccessResult
univisInfoOperationAllowed _ = checkAdmin
