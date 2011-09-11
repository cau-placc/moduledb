--------------------------------------------------------------------------
--- This module implements some auxiliary operations to support the
--- generic implementation of the Spicey entities.
--------------------------------------------------------------------------

module Spicey (
  module System, 
  module HTML, 
  module ReadNumeric, 
  Controller,
  nextController, nextControllerForData, confirmNextController,
  getControllerURL,getControllerParams, showControllerURL,
  getForm, wDateType, wBoolean, wUncheckMaybe,
  displayError, cancelOperation,
  wuiEditForm, wuiEditFormWithText, wuiFrameToForm, nextInProcessOr,
  renderLabels,
  stringToHtml, maybeStringToHtml,
  intToHtml,maybeIntToHtml, floatToHtml, maybeFloatToHtml,
  boolToHtml, maybeBoolToHtml, calendarTimeToHtml, maybeCalendarTimeToHtml,
  setPageMessage, getPageMessage,
  saveLastUrl, getLastUrl, getLastUrls
  ) where

import System
import HTML
import ReadNumeric
import KeyDatabase
import WUI
import Time
import Routes
import Processes
import UserProcesses
import Session
import Global
import Authentication
import Helpers

---------------- vvvv -- Framework functions -- vvvv -----------------------

-- a viewable can be turned into a representation which can be displayed
-- as interface
-- here: a representation of a HTML page
type Viewable = HtmlPage

type ViewBlock = [HtmlExp]

--- Controllers contains all logic and their result should be a Viewable.
--- if the behavior of controller should depend on URL parameters
--- (following the first name specifying the controller), one
--- can access these URL parameters by using the operation
--- Spicey.getControllerParams inside the controller.
type Controller = IO ViewBlock

nextController :: Controller -> _ -> IO HtmlForm
nextController controller _ = do
  view <- controller
  getForm view

-- for WUIs
nextControllerForData :: (a -> Controller) -> a -> IO HtmlForm
nextControllerForData controller param = do
  view <- controller param
  getForm view

--- Call the next controller after a user confirmation.
--- The Boolean user answer is passed as an argument to the controller.
confirmNextController :: HtmlExp -> (Bool -> Controller) -> _ -> IO HtmlForm
confirmNextController question controller _ = do
  getForm [question,
           button "Ja"   (nextController (controller True)),
           button "Nein" (nextController (controller False))]

--- If we are in a process, execute the next process depending on
--- the provided information passed in the second argument,
--- otherwise execute the given controller (first argument).
nextInProcessOr :: Controller -> Maybe ControllerResult -> Controller
nextInProcessOr controller arg = do
  isproc <- isInProcess
  if isproc then advanceInProcess arg >> return [htxt ""] -- triggers redirect
            else controller


--------------------------------------------------------------------------
-- Operations for handling URL parameters

--- Parse the URL parameter passed to the main script. The result is a pair
--- consisting of the route and the list of parameters separated by '/'.
parseUrl :: String -> (String, [String])
parseUrl urlparam =
  let (url:ctrlparams) = splitUrl urlparam
  in  (url,ctrlparams)
  
--- Splits the URL parameter passed to the main script into a list of
--- strings. The strings are separated in the URL by '/'.
splitUrl :: String -> [String]
splitUrl url =
  let (ys,zs) = break (== '/') url
   in if null zs then [ys]
                 else ys : splitUrl (tail zs)

--- Gets the controller URL and the control parameters (separated by '/').
--- For instance, if the spicey script is called with the URL
--- "show.cgi?listEntity/arg1/arg2", this operation returns
--- ("listEntity",["arg1","arg2"]).
getControllerURL :: IO (String, [String])
getControllerURL = getUrlParameter >>= return . parseUrl

--- Gets the control parameters from the current URL.
getControllerParams :: IO [String]
getControllerParams = getUrlParameter >>= return . snd . parseUrl

--- Shows the URL corresponding to the control parameters.
--- The first argument is the URL of the controller (e.g., "listEntity")
--- and the second argument is the list of control parameters.
showControllerURL :: String -> [String] -> String
showControllerURL ctrlurl params = '?' : ctrlurl ++ concatMap ('/':) params

--------------------------------------------------------------------------
-- A standard HTML frame for editing data with WUIs.
wuiEditForm :: String -> String -> Controller
           -> HtmlExp -> WuiHandler -> [HtmlExp]
wuiEditForm title buttontag controller hexp handler =
  [h1 [htxt title],
   blockstyle "editform" [hexp],
   par [wuiHandler2button buttontag handler,
        button "Abbrechen" (nextController (cancelOperation >> controller))]]

-- A standard HTML frame for editing data with WUIs where some
-- additional text explanations are included at the top.
wuiEditFormWithText :: String -> String -> [HtmlExp] -> Controller
           -> HtmlExp -> WuiHandler -> [HtmlExp]
wuiEditFormWithText title buttontag cmts controller hexp handler =
  [h1 [htxt title]] ++ cmts ++
  [blockstyle "editform" [hexp],
   par [wuiHandler2button buttontag handler,
        button "Abbrechen" (nextController (cancelOperation >> controller))]]

--- Transforms a WUI frame into a standard form.
wuiFrameToForm :: (HtmlExp -> WuiHandler -> [HtmlExp])
               -> HtmlExp -> WuiHandler -> IO HtmlForm
wuiFrameToForm wframe hexp wuihandler = getForm (wframe hexp wuihandler)


--- A WUI for manipulating CalendarTime entities.
--- It is based on a WUI for dates, i.e., the time is ignored.
wDateType :: WuiSpec CalendarTime
wDateType = transformWSpec (tuple2date,date2tuple) wDate
 where
  tuple2date :: (Int, Int, Int) -> CalendarTime
  tuple2date (day, month, year) = CalendarTime year month day 0 0 0 0

  date2tuple :: CalendarTime -> (Int, Int, Int)
  date2tuple( CalendarTime year month day _ _ _ _) = (day, month, year)

--- A WUI for manipulating date entities.
wDate :: WuiSpec (Int, Int, Int)
wDate = wTriple (wSelectInt [1..31])
                (wSelectInt [1..12])
                (wSelectInt [1950..2050])

--- A WUI for manipulating Boolean entities. In general, this view should
--- be specialized by replacing true and false by more comprehensible strings.
wBoolean :: WuiSpec Bool
wBoolean = wSelectBool "True" "False"

--- A WUI transformer to map WUIs into WUIs for corresponding Maybe types.
wUncheckMaybe :: a -> WuiSpec a -> WuiSpec (Maybe a)
wUncheckMaybe defval wspec =
  wMaybe (adaptWSpec not (wCheckBool [htxt "No value"])) wspec defval

--- The standard menu for all users.
getUserMenu :: IO HtmlExp
getUserMenu = do
  login <- getSessionLogin
  return $ blockstyle "menu"
    [ulist $
      [[href "?main" [htxt "Haupseite"]],
       [href "?listStudyProgram" [htxt "Studiengänge"]],
       [href "?listMasterProgram" [htxt "Masterprogramme"]],
       [href "?search" [htxt "Modulsuche"]]] ++
      (maybe []
         (\lname -> [[href ("?listModData/"++"user="++lname)
                           [htxt "Eigene Module"]],
                     [href "?newMasterProgram" [htxt "Neues Masterprogram"]]])
         login) ++
      [[href "?login" [htxt (maybe "An" (const "Ab") login ++ "melden")]]]]

--- Adds the basic page layout to a view.
addLayout :: ViewBlock -> IO ViewBlock
addLayout viewblock = do
  usermenu   <- getUserMenu
  routemenus <- getRouteMenus
  msg        <- getPageMessage
  admin      <- isAdmin
  return $
    [blockstyle "header" [mdbHeader],
     blockstyle "pagemessage" [nbsp, htxt msg],
     usermenu] ++
    viewblock ++
    (if admin then [fst routemenus,h2 [nbsp],-- to enforce line break
                    snd routemenus]
              else []) ++
    [blockstyle "footer"
      [par [htxt "powered by",
            href "http://www.informatik.uni-kiel.de/~pakcs/spicey"
                 [image "images/spicey-logo.png" "Spicey"]
                 `addAttr` ("target","_blank"),
            htxt "Framework"]]]

getForm :: ViewBlock -> IO HtmlForm
getForm viewBlock =
  if viewBlock == [HtmlText ""]
  then return $ HtmlForm "forward to Spicey"
                  [HeadInclude (HtmlStruct "meta"
                                 [("http-equiv","refresh"),
                                  ("content","1; url=show.cgi")] [])]
                  [par [htxt "You will be forwarded..."]]
  else do
    cookie  <- sessionCookie
    --lasturl <- getLastUrl
    login   <- getSessionLogin
    body    <- addLayout ([blockstyle "debug"
                             [par [htxt ("login: "++maybe "" id login)]],
                                   --htxt ("last page: "++lasturl)]],
                           blockstyle "contents" viewBlock])
    return $ HtmlForm "Moduldatenbank"
                      [cookie, FormCSS "css/style.css",icon,MultipleHandlers]
                      body
 where
   icon = HeadInclude (HtmlStruct "link"
                                  [("rel","shortcut icon"),
                                   ("href","favicon.ico")] [])

-- standard header for master management pages:
mdbHeader =
  table [map (\i->[i]) headerItems]
 where
  headerItems =
   [href "show.cgi"
      [imageNB "images/MDB_Logo_small.gif" "Moduldatenbank"],
    blockstyle "headertitle"
      [nbsp, href "show.cgi" [htxt "Modulinformationssystem Informatik"], nbsp],
    ehref "http://www.informatik.uni-kiel.de"
      [imageNB "images/ifilogo.gif" "Institut für Informatik"],
    ehref "http://www.uni-kiel.de"
      [imageNB "images/caulogo.gif" "Christian-Albrechts-Universität zu Kiel"]]

-------------------------------------------------------------------------
-- Action performed when a "cancel" button is pressed.
-- In this case, a message is shown.
cancelOperation :: IO ()
cancelOperation = do
  inproc <- isInProcess
  if inproc then removeCurrentProcess else done
  setPageMessage $ (if inproc then "Process" else "Operation") ++ " cancelled"

-- dummy-controller to display an error
displayError :: String -> Controller
displayError msg = do
  inproc <- isInProcess
  if inproc then removeCurrentProcess else done
  setPageMessage ("Error occurred!" ++
                  if inproc then " Process terminated!" else "")
  return
    [h1 [htxt $ if null msg
                then "General error (shown by function Spicey.displayError)"
                else msg]]

-- like renderTaggedTuple from WUI Library but takes list of HtmlExp instead of list of strings
renderLabels :: [[HtmlExp]] -> Rendering
renderLabels labels hexps =
 table (map (\(l, h) -> [l, [h]]) (zip labels hexps))
    
-- convert standard-datatype-values to html representation
stringToHtml :: String -> HtmlExp
stringToHtml s = textstyle "type_string" s

maybeStringToHtml :: Maybe String -> HtmlExp
maybeStringToHtml s = textstyle "type_string" (maybe "" id s)

intToHtml :: Int -> HtmlExp
intToHtml i = textstyle "type_int" (show i)

maybeIntToHtml :: Maybe Int -> HtmlExp
maybeIntToHtml i = textstyle "type_int" (maybe "" show i)

floatToHtml :: Float -> HtmlExp
floatToHtml i = textstyle "type_float" (show i)

maybeFloatToHtml :: Maybe Float -> HtmlExp
maybeFloatToHtml i = textstyle "type_float" (maybe "" show i)

boolToHtml :: Bool -> HtmlExp
boolToHtml b = textstyle "type_bool" (show b)

maybeBoolToHtml :: Maybe Bool -> HtmlExp
maybeBoolToHtml b = textstyle "type_bool" (maybe "" show b)

calendarTimeToHtml :: CalendarTime -> HtmlExp
calendarTimeToHtml ct = textstyle "type_calendartime" (toDayString ct)

maybeCalendarTimeToHtml :: Maybe CalendarTime -> HtmlExp
maybeCalendarTimeToHtml ct =
  textstyle "type_calendartime" (maybe "" toDayString ct)

--------------------------------------------------------------------------
-- The page messages are implemented by a session store.
-- We define a global variable to store a message which is shown
-- in the next HTML page of a session.

--- Definition of the session state to store the page message (a string).
pageMessage :: Global (SessionStore String)
pageMessage = global emptySessionStore Temporary

--- Gets the page message and delete it.
getPageMessage :: IO String
getPageMessage = do
  msg <- getSessionData pageMessage
  removeSessionData pageMessage
  return (maybe "" id msg)

--- Set the page message of the current session.
setPageMessage :: String -> IO ()
setPageMessage msg = putSessionData msg pageMessage

--------------------------------------------------------------------------
-- Another example for using sessions.
-- We store the list of selected URLs into  the current session.

--- Definition of the session state to store the last URL (as a string).
lastUrls :: Global (SessionStore [String])
lastUrls = global emptySessionStore Temporary

--- Gets the list of URLs of the current session.
getLastUrls :: IO [String]
getLastUrls = getSessionData lastUrls >>= return . maybe [] id

--- Gets the last URL of the current session (or "?").
getLastUrl :: IO String
getLastUrl = do urls <- getLastUrls
                return (if null urls then "?" else head urls)

--- Saves the last URL of the current session.
saveLastUrl :: String -> IO ()
saveLastUrl url = do
  urls <- getLastUrls
  putSessionData (url:urls) lastUrls

--------------------------------------------------------------------------
