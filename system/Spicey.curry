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
  mainContentsWithSideMenu,
  displayError, cancelOperation,
  wuiEditForm, wuiEditFormWithText, wuiFrameToForm, nextInProcessOr,
  renderLabels,
  stringToHtml, maybeStringToHtml,
  intToHtml,maybeIntToHtml, floatToHtml, maybeFloatToHtml,
  boolToHtml, maybeBoolToHtml, calendarTimeToHtml, maybeCalendarTimeToHtml,
  hrefStudyProgram, hrefCategory, smallHrefCategory,
  hrefModule, smallHrefModule, hrefExtModule, hrefModInst, hrefUnivis,
  spHref, spButton, spPrimButton, spSmallButton, spTable, spHeadedTable,
  spShortSelectionInitial,
  setPageMessage, getPageMessage,
  saveLastUrl, getLastUrl, getLastUrlParameters, getLastUrls
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
import Distribution
import UserPreferences

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
           par [spButton "Ja"   (nextController (controller True)),
                spButton "Nein" (nextController (controller False))]]

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
   wuiHandler2button buttontag handler `addClass` "btn btn-primary",
   spButton "Abbrechen" (nextController (cancelOperation >> controller))]

-- A standard HTML frame for editing data with WUIs where some
-- additional text explanations are included at the top.
wuiEditFormWithText :: String -> String -> [HtmlExp] -> Controller
           -> HtmlExp -> WuiHandler -> [HtmlExp]
wuiEditFormWithText title buttontag cmts controller hexp handler =
  [h1 [htxt title]] ++ cmts ++
  [blockstyle "editform" [hexp],
   par [wuiHandler2button buttontag handler `addClass` "btn btn-primary",
        spButton "Abbrechen" (nextController (cancelOperation >> controller))]]

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
  wMaybe (transformWSpec (not,not) (wCheckBool [htxt "No value"]))
         wspec
         defval

--- The standard menu for all users.
getUserMenu :: Maybe String -> UserPrefs -> IO HtmlExp
getUserMenu login prefs = do
  let t = translate prefs
  return $
     ulist $
      [--[href "?main" [htxt "Hauptseite"]],
       [href "?listStudyProgram" [htxt $ t "Study programs"]],
       [href "?listMasterProgram" [htxt $ t "Master programs"]],
       [href "?search" [htxt $ t "Search modules"]]] ++
      (maybe []
        (\lname -> [[href ("?listCategory/"++"user="++lname)
                           [htxt $ t "My modules"]],
                    [href "?newMasterProgram" [htxt $ t "New master program"]]])
        login) ++
      [[href "?login" [htxt $ t ("Log" ++ maybe "in" (const "out") login)]]]

--- The title of this application (shown in the header).
spiceyTitle :: String
spiceyTitle = "Modulinformationssystem Informatik"

--- Adds the basic page layout to a view.
addLayout :: ViewBlock -> IO ViewBlock
addLayout viewblock = do
  login      <- getSessionLogin
  prefs      <- getSessionUserPrefs
  usermenu   <- getUserMenu login prefs
  (routemenu1,routemenu2) <- getRouteMenus
  msg        <- getPageMessage
  admin      <- isAdmin
  return $
    stdNavBar usermenu login prefs ++
    [blockstyle "container-fluid" $
      [HtmlStruct "header" [("class","hero-unit")] [h1 mainTitle],
       if null msg
        then HtmlStruct "header" [("class","pagemessage pagemessage-empty")]
                        [nbsp]
        else HtmlStruct "header" [("class","pagemessage")] [htxt msg],
       blockstyle "row-fluid"
        [blockstyle "span12" mainDoc]] ++
       (if admin then adminNavBar routemenu1 ++ adminNavBar routemenu2
                 else []) ++
      [hrule,
       HtmlStruct "footer" []
        [par [htxt "powered by",
              href "http://www.informatik.uni-kiel.de/~pakcs/spicey"
                   [image "images/spicey-logo.png" "Spicey"]
                 `addAttr` ("target","_blank"),
              htxt "Framework"]]]]
 where
  (mainTitle,mainDoc) =
    case viewblock of
      (HtmlStruct "h1" [] t : hexps) -> (t,hexps)
      _ -> ([htxt spiceyTitle], viewblock)

--- Create a side menu containing a (possibly empty) title and a list of items:
titledSideMenu :: String -> [[HtmlExp]] -> [HtmlExp]
titledSideMenu title items =
  (if null title
   then []
   else [HtmlStruct "small" [] [htxt title]]) ++
  [ulist items `addClass` "nav nav-list"]

--- Create contents in the main page area with a side menu.
mainContentsWithSideMenu :: [[HtmlExp]] -> [HtmlExp] -> [HtmlExp]
mainContentsWithSideMenu menuitems contents =
  [blockstyle "row-fluid"
    [blockstyle "span3"
      [blockstyle "well sidebar-nav" (titledSideMenu "" menuitems)],
       blockstyle "span9" contents]]

-- Standard navigation bar at the top.
-- The first argument is the route menu (a ulist).
-- The second argument is the possible login name.
stdNavBar :: HtmlExp -> Maybe String -> UserPrefs -> [HtmlExp]
stdNavBar routemenu login prefs =
  [blockstyle "navbar navbar-inverse navbar-fixed-top"
    [blockstyle "navbar-inner"
      [blockstyle "container-fluid"
         [addDropdownItem routemenu `addClass` "nav",
          par [bold [if preferredLanguage prefs == English
                     then href "?langDE" [htxt "[Deutsch]"]
                     else href "?langEN" [htxt "[English]"]], nbsp, nbsp, nbsp,
               userIcon, nbsp, htxt $ maybe (t "not logged in") id login]
            `addClass` "navbar-text pull-right"]
      ]
    ]
  ]
 where
  t = translate prefs

  userIcon = HtmlText "<i class=\"icon-user icon-white\"></i>"

  addDropdownItem (HtmlStruct tag ats items) =
    HtmlStruct tag ats (dropDownMenuItem : items)

  dropDownMenuItem =
    HtmlStruct "li" [("class","dropdown")]
     [href "#" [htxt $ t "Go to", bold [htxt " "] `addClass` "caret"]
       `addAttrs` [("class","dropdown-toggle"),("data-toggle","dropdown")],
      HtmlStruct "ul" []
        (litem [href "?main"
                     [htxt $ t "Main page of the module information system"]]
           : extUrls)
        `addClass` "dropdown-menu"]

  extUrls =
   [toEHref "http://www.informatik.uni-kiel.de" "Institut für Informatik"
   ,toEHref "http://www.uni-kiel.de" "CAU Kiel"
   ,litem [htxt " "] `addClass` "divider"
   ,litem [htxt $ t "Supported by:"] `addClass` "nav-header"
   ,toEHref "http://www.curry-language.org"
            ("Curry ("++t "programming language"++")")
   ,toEHref "http://www.informatik.uni-kiel.de/~pakcs/spicey"
            "Spicey (Web Framework)"
   ,toEHref "http://twitter.github.com/bootstrap/"
            "Twitter Bootstrap (Style Sheets)"
   ]

  toEHref url s = litem [ehref url [arrowIcon, nbsp, htxt s]]

  arrowIcon = HtmlText "<i class=\"icon-arrow-right\"></i>"

-- Admin navigation bar at the bottom.
-- The first argument is the menu (a ulist).
adminNavBar :: HtmlExp -> [HtmlExp]
adminNavBar routemenu =
  [blockstyle "navbar navbar-inverse"
    [blockstyle "navbar-inner"
      [blockstyle "container-fluid"
         [routemenu `addClass` "nav"]
      ]
    ]
  ]

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
    body    <- addLayout viewBlock
    return $ HtmlForm spiceyTitle
                     ([responsiveView, cookie, icon, MultipleHandlers] ++
                      map (\f -> FormCSS $ "css/"++f++".css")
                          ["bootstrap","bootstrap-responsive","style"] ++
                      map (\f -> FormJScript $ "js/"++f++".js")
                          ["jquery","bootstrap-dropdown"])
                     body
 where
   responsiveView =
     HeadInclude (HtmlStruct "meta"
                    [("name","viewport"),
                     ("content","width=device-width, initial-scale=1.0")] [])

   icon = HeadInclude (HtmlStruct "link"
                                  [("rel","shortcut icon"),
                                   ("href","favicon.ico")] [])

-- standard header for master management pages:
mdbHeader =
  table [map (\i->[i]) headerItems]
 where
  headerItems =
   [href "?"
      [imageNB "images/MDB_Logo_small.gif" "Moduldatenbank"],
    blockstyle "headertitle"
      [nbsp, htxt "Modulinformationssystem Informatik", nbsp],
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

-- like renderTaggedTuple from WUI Library but takes list of HtmlExp
-- instead of list of strings
renderLabels :: [[HtmlExp]] -> Rendering
renderLabels labels hexps =
  spTable (map (\(l, h) -> [l, [h]]) (zip labels hexps))

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
-- Auxiliary HTML items:

--- Hypertext reference to a study program:
hrefStudyProgram :: String -> [HtmlExp] -> HtmlExp
hrefStudyProgram ref hexps =
  href ref hexps `addClass` "btn btn-info btn-block btn-left"

--- Hypertext reference to a category:
hrefCategory :: String -> [HtmlExp] -> HtmlExp
hrefCategory ref hexps =
  href ref hexps `addClass` "btn btn-info btn-block btn-left"

--- Small hypertext reference to a category:
smallHrefCategory :: String -> [HtmlExp] -> HtmlExp
smallHrefCategory = spEHref

--- Hypertext reference to a module:
hrefModule :: String -> [HtmlExp] -> HtmlExp
hrefModule ref hexps =
  href ref hexps `addClass` "btn btn-link"

--- Small hypertext reference to a module:
smallHrefModule :: String -> [HtmlExp] -> HtmlExp
smallHrefModule = spEHref

--- Hypertext reference to an external module:
hrefExtModule :: String -> [HtmlExp] -> HtmlExp
hrefExtModule ref hexps =
  hrefModule ref hexps `addAttr` ("target","_blank")

--- Hypertext reference to a module instance:
hrefModInst :: String -> [HtmlExp] -> HtmlExp
hrefModInst = spEHrefBlock

--- Hypertext reference to a UnivIS information:
hrefUnivis :: String -> [HtmlExp] -> HtmlExp
hrefUnivis = spEHrefBlock

--- Hypertext reference in Spicey (rendered as a block button):
spHref :: String -> [HtmlExp] -> HtmlExp
spHref ref hexps =
  href ref hexps `addClass` "btn btn-small"

--- Hypertext reference in Spicey (rendered as a block button):
spHrefBlock :: String -> [HtmlExp] -> HtmlExp
spHrefBlock ref hexps =
  href ref hexps `addClass` "btn btn-small btn-block"

--- Hypertext reference in Spicey (rendered as an info block button):
spHrefInfoBlock :: String -> [HtmlExp] -> HtmlExp
spHrefInfoBlock ref hexps =
  href ref hexps `addClass` "btn btn-info btn-block"

--- External hypertext reference in Spicey (rendered as a block button):
spEHref :: String -> [HtmlExp] -> HtmlExp
spEHref ref hexps = spHref ref hexps `addAttr` ("target","_blank")

--- External hypertext reference in Spicey (rendered as a block button):
spEHrefBlock :: String -> [HtmlExp] -> HtmlExp
spEHrefBlock ref hexps = spHrefBlock ref hexps `addAttr` ("target","_blank")

--- External hypertext reference in Spicey (rendered as an info block button):
spEHrefInfoBlock :: String -> [HtmlExp] -> HtmlExp
spEHrefInfoBlock ref hexps =
  spHrefInfoBlock ref hexps `addAttr` ("target","_blank")

--- Input button in Spicey (rendered as a default button):
spButton :: String -> HtmlHandler -> HtmlExp
spButton label handler =
  button label handler `addClass` "btn"

--- Primary input button in Spicey (rendered as a default primary button):
spPrimButton :: String -> HtmlHandler -> HtmlExp
spPrimButton label handler =
  button label handler `addClass` "btn btn-primary"

--- Small input button in Spicey (rendered as a small button):
spSmallButton :: String -> HtmlHandler -> HtmlExp
spSmallButton label handler =
  button label handler `addClass` "btn btn-small"

--- Short selectionInitial input field:
spShortSelectionInitial :: CgiRef -> [(String,String)] -> Int -> HtmlExp
spShortSelectionInitial cref sellist sel =
  selectionInitial cref sellist sel `addClass` "shorttextinput"

--- Standard table in Spicey. Visualize it with a grid or with
--- a table if there are too many columns.
spTable :: [[[HtmlExp]]] -> HtmlExp
spTable items = table items  `addClass` "table table-hover table-condensed"

--- Headed table in Spicey.
spHeadedTable :: [[[HtmlExp]]] -> HtmlExp
spHeadedTable items =
  headedTable items  `addClass` "table table-hover table-condensed"


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
-- We store the list of last selected URLs (maximal 3)
-- into  the current session.

--- Definition of the session state to store the last URL (as a string).
lastUrls :: Global (SessionStore [String])
lastUrls = global emptySessionStore (Persistent "sessionUrls")

--- Gets the list of URLs of the current session.
getLastUrls :: IO [String]
getLastUrls = getSessionData lastUrls >>= return . maybe [] id

--- Gets the last URL of the current session (or "?").
getLastUrl :: IO String
getLastUrl = do urls <- getLastUrls
                return (if null urls then "?" else head urls)

--- Gets the parameters of the last URL of the current session.
getLastUrlParameters :: IO (String,[String])
getLastUrlParameters = do
  urls <- getLastUrls
  return $ if null urls
           then ("",[])
           else parseUrl (head urls)

--- Saves the last URL of the current session.
saveLastUrl :: String -> IO ()
saveLastUrl url = do
  urls <- getLastUrls
  putSessionData (url : take 2 urls) lastUrls

--------------------------------------------------------------------------
