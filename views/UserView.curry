module UserView (
 wUser, tuple2User, user2Tuple, wUserType, blankUserView, createUserView,
 editUserView, showUserView, listUserView, leqUser
 ) where

import WUI
import HTML
import Time
import Sort
import Spicey
import MDB
import MDBEntitiesToHtml

--- The WUI specification for the entity type User.
wUser
 :: WuiSpec (String,String,String,String,String,String,String,CalendarTime)
wUser =
  withRendering
   (w8Tuple wReqStr wReqStr wStr wStr wReqStr wStr
            (wConstant htxt) (wConstant (htxt . calendarTimeToString)))
   (renderLabels userLabelList)
 where
  wReqStr = wRequiredStringSize 40
  wStr    = wStringSize 40

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
      
      wuiframe = wuiEditForm "new User" "create" (controller False initdata)
      
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
      
      wuiframe = wuiEditForm "edit User" "change" (controller False initdata)
      
      (hexp ,handler) = wuiWithErrorForm (wUserType user) initdata
                         (nextControllerForData (controller True))
                         (wuiFrameToForm wuiframe)
   in wuiframe hexp handler

--- Supplies a view to show the details of a User.
showUserView :: User -> Controller -> [HtmlExp]
showUserView user controller =
  userToDetailsView user ++
   [button "back to User list" (nextController controller)]

--- Compares two User entities. This order is used in the list view.
leqUser :: User -> User -> Bool
leqUser x1 x2 =
  (userLogin x1,userName x1,userFirst x1,userTitle x1,userEmail x1,userUrl x1
  ,userPassword x1,userLastLogin x1) <=
   (userLogin x2,userName x2,userFirst x2,userTitle x2,userEmail x2,userUrl x2
   ,userPassword x2,userLastLogin x2)

--- Supplies a list view for a given list of User entities.
--- Shows also buttons to show, delete, or edit entries.
--- The arguments are the list of User entities
--- and the controller functions to show, delete and edit entities.
listUserView
 :: [User] -> (User -> Controller) -> (User -> Controller)
  -> (User -> Bool -> Controller) -> [HtmlExp]
listUserView users showUserController editUserController
             deleteUserController =
  [h1 [htxt "User list"]
  ,table ([take 3 userLabelList ++ [userLabelList!!7]] ++
          map listUser (mergeSort leqUser users))]
  where listUser :: User -> [[HtmlExp]]
        listUser user =
          userToListView user ++
           [[button "show" (nextController (showUserController user))
            ,button "edit" (nextController (editUserController user))
            ,button "delete"
              (confirmNextController
                (h3
                  [htxt
                    (concat
                      ["Really delete entity \"",userToShortView user
                      ,"\"?"])])
                (deleteUserController user))]]
