module UserView (
 wUser, tuple2User, user2Tuple, wUserType, blankUserView, createUserView,
 editUserView, showUserView, listUserView, leqUser
 ) where

import WUI
import HTML.Base
import Time
import Sort
import Spicey
import Helpers
import MDB
import MDBEntitiesToHtml

--- The WUI specification for the entity type User.
wUser
 :: WuiSpec (String,String,String,String,String,String,String,CalendarTime)
wUser =
  withRendering
   (w8Tuple wMediumRequiredString wMediumRequiredString wMediumString
            wMediumString wMediumRequiredString wMediumString
            (wConstant htxt) (wConstant (htxt . calendarTimeToString)))
   (renderLabels userLabelList)

--- Transformation from data of a WUI form to entity type User.
tuple2User
 :: User -> (String,String,String,String,String,String,String,CalendarTime)
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
 :: User -> (String,String,String,String,String,String,String,CalendarTime)
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
 :: CalendarTime
  -> (Bool -> (String,String,String,String,String,String,String,CalendarTime)
       -> Controller)
  -> [HtmlExp]
blankUserView ctime controller =
  createUserView [] [] [] [] [] [] [] ctime controller

--- Supplies a WUI form to create a new User entity.
--- Takes default values to be prefilled in the form fields.
createUserView
 :: String -> String -> String -> String -> String -> String -> String
  -> CalendarTime
  -> (Bool -> (String,String,String,String,String,String,String,CalendarTime)
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
  (userName x1,userFirst x1,userLogin x1,userTitle x1,userEmail x1,userUrl x1
  ,userPassword x1,userLastLogin x1) <=
   (userName x2,userFirst x2,userLogin x2,userTitle x2,userEmail x2,userUrl x2
   ,userPassword x2,userLastLogin x2)

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
       [spHref ("?User/login/"++showUserKey user) [htxt "Anmelden"]],
       [spHref ("?User/modules/"++showUserKey user) [htxt "Module"]]]