module View.User (
   wUser, tuple2User, user2Tuple, wUserType, blankUserView, createUserView
 , editUserView, showUserView, listUserView, leqUser
 , loginView, sendLoginDataView, changePasswordView
 ) where

import Mail ( sendMail )
import WUI
import HTML.Base
import Time
import Sort
import System.Authentication
import System.MultiLang
import System.SessionInfo
import System.Spicey
import System.Helpers
import MDB
import ConfigMDB ( adminEmail )
import View.MDBEntitiesToHtml

--- The WUI specification for the entity type User.
wUser
 :: WuiSpec (String,String,String,String,String,String,String,ClockTime)
wUser =
  withRendering
   (w8Tuple wMediumRequiredString wMediumRequiredString wMediumString
            wMediumString wMediumRequiredString wMediumString
            (wConstant htxt) (wConstant (htxt . calendarTimeToString . toUTCTime )))
   (renderLabels userLabelList)

--- Transformation from data of a WUI form to entity type User.
tuple2User
 :: User -> (String,String,String,String,String,String,String,ClockTime)
  -> User
tuple2User userToUpdate (login ,name ,first ,title ,email ,url ,password
                         ,lastLogin) =
  setUserLogin
   (setUserName
     (setUserFirst
       (setUserTitle
         (setUserEmail
           (setUserUrl
             (setUserPassword (setUserLastLogin userToUpdate lastLogin)
               password)
             url)
           email)
         title)
       first)
     name)
   login

--- Transformation from entity type User to a tuple
--- which can be used in WUI specifications.
user2Tuple
 :: User -> (String,String,String,String,String,String,String,ClockTime)
user2Tuple user =
  (userLogin user,userName user,userFirst user,userTitle user,userEmail user
  ,userUrl user,userPassword user,userLastLogin user)

--- WUI Type for editing or creating User entities.
--- Includes fields for associated entities.
wUserType :: User -> WuiSpec User
wUserType user = transformWSpec (tuple2User user,user2Tuple) wUser

--- Supplies a WUI form to create a new User entity.
--- The fields of the entity have some default values.
blankUserView
 :: ClockTime
  -> (Bool -> (String,String,String,String,String,String,String,ClockTime)
       -> Controller)
  -> [HtmlExp]
blankUserView ctime controller =
  createUserView [] [] [] [] [] [] [] ctime controller

--- Supplies a WUI form to create a new User entity.
--- Takes default values to be prefilled in the form fields.
createUserView
 :: String -> String -> String -> String -> String -> String -> String
  -> ClockTime
  -> (Bool -> (String,String,String,String,String,String,String,ClockTime)
       -> Controller)
  -> [HtmlExp]
createUserView defaultLogin defaultName defaultFirst defaultTitle defaultEmail
               defaultUrl defaultPassword defaultLastLogin controller =
  let initdata = (defaultLogin,defaultName,defaultFirst,defaultTitle
                 ,defaultEmail,defaultUrl,defaultPassword,defaultLastLogin)
      
      wuiframe = wuiEditForm "Neuen Benutzer anlegen" "Anlegen"
                             (controller False initdata)
      
      (hexp ,handler) = wuiWithErrorForm wUser initdata
                         (nextControllerForData (controller True))
                         (wuiFrameToForm wuiframe)
   in wuiframe hexp handler

--- Supplies a WUI form to edit the given User entity.
--- Takes also associated entities and a list of possible associations
--- for every associated entity type.
editUserView :: User -> (Bool -> User -> Controller) -> [HtmlExp]
editUserView user controller =
  let initdata = user
      
      wuiframe = wuiEditForm "Benutzerdaten ändern"
                             "Speichern" (controller False initdata)
      
      (hexp ,handler) = wuiWithErrorForm (wUserType user) initdata
                         (nextControllerForData (controller True))
                         (wuiFrameToForm wuiframe)
   in wuiframe hexp handler

--- Supplies a view to show the details of a User.
--- Shows also buttons to show, delete, or edit entries.
--- The arguments are the list of User entities
--- and the controller functions to show, delete and edit entities.
showUserView :: User -> [HtmlExp]
showUserView user =
  [h1 [htxt $ "Benutzer " ++ userLogin user]] ++
  userToDetailsView user ++
  [par [spHref ("?User/edit/"++showUserKey user) [htxt "Ändern"], nbsp
       ,spHref ("?User/delete/"++showUserKey user) [htxt "Löschen"], nbsp
       ,spHref ("?User/login/"++showUserKey user) [htxt "Anmelden"], nbsp
       ,spHref ("?User/modules/"++showUserKey user) [htxt "Module"]]
  ]

--- Compares two User entities. This order is used in the list view.
leqUser :: User -> User -> Bool
leqUser x1 x2 =
  (userName x1,userFirst x1,userLogin x1,userTitle x1,userEmail x1) <=
   (userName x2,userFirst x2,userLogin x2,userTitle x2,userEmail x2)

--- Supplies a list view for a given list of User entities.
listUserView :: [User] -> [HtmlExp]
listUserView users =
  [h1 [htxt "Liste aller Benutzer"]
  ,spTable ([take 3 userLabelList ++ [userLabelList!!7]] ++
            map listUser (sortBy leqUser users))]
  where
   listUser :: User -> [[HtmlExp]]
   listUser user =
      userToListView user ++
      [[spHref ("?User/show/"++showUserKey user) [htxt "Anzeigen"]],
       [spHref ("?User/edit/"++showUserKey user) [htxt "Ändern"]],
       [spHref ("?User/delete/"++showUserKey user) [htxt "Löschen"]],
       [spHref ("?User/loginAs/"++showUserKey user) [htxt "Anmelden"]],
       [spHref ("?User/modules/"++showUserKey user) [htxt "Module"]]]

-----------------------------------------------------------------------------
--- View to login.
loginView :: Controller -> UserSessionInfo -> [HtmlExp]
loginView controller sinfo =
  [h3 [htxt $ t "Login as lecturer"],
   spTable [[[htxt $ t "Login name:"], [textfield loginfield ""]],
            [[htxt $ t "Password:"],   [password passfield]]],
   par [spPrimButton (t "Login") loginHandler],
   hrule,
   par [hrefPrimButton "?User/sendlogin" [htxt $ t "Forgot your login data?"]]
  ]
 where
  loginfield,passfield free

  t = translate sinfo

  loginHandler env = do
    let loginname = env loginfield
    hashpass <- getUserHash loginname (env passfield)
    users <- runQ $ queryUserByLoginPass loginname hashpass
    if null users
      then do setPageMessage $ t "Wrong login data!"
              nextInProcessOr controller Nothing >>= getForm
      else do loginToSession loginname
              ctime <- getClockTime
              runT (updateUser (setUserLastLogin (head users) ctime))
              setPageMessage (t "Logged in as: " ++ loginname)
              urls <- getLastUrls
              getForm $
               [h1 [htxt $ t "Login successful"],
                par [htxt $ loginText sinfo loginname],
                par [htxt $ timeoutText sinfo]] ++
                if length urls > 1
                then [] --[par [href ('?':urls!!1) [htxt "Back to last page"]]]
                else []

------------------------------------------------------------------------
-- send login data to a new user
sendLoginDataView :: Controller -> UserSessionInfo -> [HtmlExp]
sendLoginDataView controller sinfo =
  [h1 [htxt $ t "Send login data"],
   par [htxt $ sendPasswordCmt sinfo],
   par [htxt $ t "Your email address: ", textfield emailref ""],
   hrule,
   spPrimButton (t "Send new password") sendHandler,
   spButton (t "Cancel")
            (const (cancelOperation >> controller >>= getForm))]
 where
  emailref free

  t = translate sinfo

  sendHandler env = do
    users <- runQ $ queryCondUser (\u -> userEmail u == env emailref)
    if null users
     then displayError (unknownUser sinfo) >>= getForm
     else sendNewEmail (head users)

  sendNewEmail user = do
    newpass <- randomPassword 10
    hashpass <- getUserHash (userLogin user) newpass
    sendMail adminEmail
             (userEmail user)
             (t "Login data for module database")
             (loginEmailText sinfo (userLogin user) newpass)
    runT $ updateUser (setUserPassword user hashpass)
    getForm [h1 [htxt $ t "Acknowledgment"],
             h3 [htxt $ t "Your new password has been sent"]]

-----------------------------------------------------------------------------
--- View to change the password if a user is logged in.
changePasswordView :: Controller -> UserSessionInfo -> [HtmlExp]
changePasswordView controller sinfo =
  case userLoginOfSession sinfo of
   Nothing -> [h3 [htxt $ "Operation not allowed!"]]
   Just ln ->
    [h1 [htxt $ t "Change password"],
     spTable [[[htxt $ t "Old password:"],        [password oldpass]],
              [[htxt $ t "New password:"],        [password newpass1]],
              [[htxt $ t "Repeat new password:"], [password newpass2]]],
     hrule,
     spPrimButton (t "Change password")
                  (\env -> passwdHandler ln env >>= getForm),
     nbsp,
     spButton (t "Cancel")
              (const (cancelOperation >> controller >>= getForm))]
 where
  t = translate sinfo

  oldpass,newpass1,newpass2 free
  
  passwdHandler lname env = do
    hashpass <- getUserHash lname (env oldpass)
    users <- runQ $ queryUserByLoginPass lname hashpass
    if null users
     then displayError $ t "Wrong password!"
     else do let u = head users
             newhashpass <- getUserHash lname (env newpass1)
             if env newpass1 /= env newpass2
              then displayError $ t "New passwords are different!"
              else do runT (updateUser (setUserPassword u newhashpass))
                      setPageMessage $ t "Passwort changed"
                      controller

--- Gets a user entry with a given login name and (hashed) password:
queryUserByLoginPass :: String -> String -> DBAction [User]
queryUserByLoginPass login pass = seq login $ seq pass $
  queryCondUser (\u -> userLogin u == login && userPassword u == pass)

------------------------------------------------------------------------
