--------------------------------------------------------------------------
--- This module implements some auxiliary operations to support the
--- generic implementation of the Spicey entities.
--------------------------------------------------------------------------

module System.Spicey (
  module System, 
  module HTML.Base, 
  module ReadNumeric, 
  Controller, applyControllerOn,
  nextController, nextControllerForData, confirmNextController,
  confirmControllerOLD, confirmController, transactionController,
  transactionBindController,
  getControllerURL,getControllerParams, showControllerURL,
  getForm, wDateType, wBoolean, wUncheckMaybe,
  mainContentsWithSideMenu,
  displayError, displayHtmlError, cancelOperation,
  wuiEditForm, wuiEditFormWithText, wuiFrameToForm,
  renderWuiForm, renderLabels,
  nextInProcessOr,
  stringToHtml, maybeStringToHtml,
  intToHtml,maybeIntToHtml, floatToHtml, maybeFloatToHtml,
  boolToHtml, maybeBoolToHtml, dateToHtml, maybeDateToHtml,
  userDefinedToHtml, maybeUserDefinedToHtml,
  hrefStudyProgram, hrefCategory, smallHrefCategory,
  hrefModule, smallHrefModule, hrefExtModule, hrefModInst,
  hrefUnivisInfo, hrefUnivisDanger,
  spHref, spHrefBlock, spEHref, hrefPrimButton,
  spButton, spPrimButton, spSmallButton, spSmallPrimaryButton,
  spTable, spHeadedTable, addTitle,
  spShortSelectionInitial,
  setPageMessage, getPageMessage,
  saveLastUrl, getLastUrl, getLastUrlParameters, getLastUrls,
  continueOrError
  ) where

import System
import HTML.Base
import ReadNumeric
import WUI
import Time
import System.Routes
import System.Processes
import Config.UserProcesses
import System.Session
import Global
import System.Authentication
import System.Helpers
import Distribution
import System.MultiLang
import System.SessionInfo
import List(last)

import Database.CDBI.ER
import MDB (runT, runJustT)

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

--- Reads an entity for a given key and applies a controller to it.
applyControllerOn :: Maybe enkey -> (enkey -> DBAction en)
                  -> (en -> Controller) -> Controller
applyControllerOn Nothing _ _ = displayError "Illegal URL"
applyControllerOn (Just userkey) getuser usercontroller =
  runJustT (getuser userkey) >>= usercontroller

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

--- Call the next controller after a user confirmation.
--- The Boolean user answer is passed as an argument to the controller.
confirmControllerOLD :: HtmlExp -> (Bool -> Controller) -> Controller
confirmControllerOLD question controller = do
  return [question,
           par [spButton "Ja"   (nextController (controller True)),
                spButton "Nein" (nextController (controller False))]]

--- Ask the user for a confirmation and call the corresponding controller.
--- @param question - a question asked
--- @param yescontroller - the controller used if the answer is "yes"
--- @param nocontroller  - the controller used if the answer is "no"
confirmController :: [HtmlExp] -> Controller -> Controller -> Controller
confirmController question yescontroller nocontroller = do
  return $ question ++
           [par [spButton "Ja"   (nextController yescontroller),
                 spButton "Nein" (nextController nocontroller )]]

--- A controller to execute a transaction and proceed with a given
--- controller if the transaction succeeds. Otherwise, the
--- transaction error is shown.
--- @param trans - the transaction to be executed
--- @param controller - the controller executed in case of success
transactionController :: (DBAction _) -> Controller -> Controller
transactionController trans controller = do
  transResult <- runT trans
  either (\error -> displayError (show error))
         (\_     -> controller)
         transResult

--- A controller to execute a transaction and proceed with a given
--- controller if the transaction succeeds. Otherwise, the
--- transaction error is shown.
--- @param trans - the transaction to be executed
--- @param controller - the controller executed on the result of a successful
---                     transaction
transactionBindController :: (DBAction a) -> (a -> Controller) -> Controller
transactionBindController trans controller = do
  transResult <- runT trans
  either (\error -> displayError (show error))
         controller
         transResult

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

--- Standard rendering for WUI forms to edit data.
--- @param wuispec    - the associated WUI specification
--- @param initdata   - initial data to be prefilled in the form
--- @param ctrl       - the controller that handles the submission of the data
--- @param cancelctrl - the controller called if submission is cancelled
--- @param title      - the title of the WUI form
--- @param buttontag  - the text on the submit button
renderWuiForm :: WuiSpec a -> a -> (a -> Controller) -> Controller
              -> String -> String -> [HtmlExp]
renderWuiForm wuispec initdata controller cancelcontroller title buttontag =
  wuiframe hexp handler
 where
  wuiframe wuihexp hdlr =
    [h1 [htxt title],
     blockstyle "editform" [wuihexp],
     wuiHandler2button buttontag hdlr `addClass` "btn btn-primary",
     spButton "Abbrechen" (nextController (cancelOperation >> cancelcontroller))]
      
  (hexp,handler) = wuiWithErrorForm wuispec
                     initdata
                     (nextControllerForData controller)
                     (\he whdlr -> getForm (wuiframe he whdlr))

--- A WUI for manipulating CalendarTime entities.
--- It is based on a WUI for dates, i.e., the time is ignored.
wDateType :: WuiSpec ClockTime
wDateType = transformWSpec (tuple2date,date2tuple) wDate
 where
  tuple2date :: (Int, Int, Int) -> ClockTime
  tuple2date (day, month, year) =
    toClockTime (CalendarTime year month day 0 0 0 0)

  date2tuple :: ClockTime -> (Int, Int, Int)
  date2tuple ct = let CalendarTime year month day _ _ _ _ = toUTCTime ct
                  in (day, month, year)

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
wUncheckMaybe :: Eq a => a -> WuiSpec a -> WuiSpec (Maybe a)
wUncheckMaybe defval wspec =
  wMaybe (transformWSpec (not,not) (wCheckBool [htxt "No value"]))
         wspec
         defval

--- The standard menu for all users.
getUserMenu :: Maybe String -> UserSessionInfo -> IO HtmlExp
getUserMenu login sinfo = do
  let t = translate sinfo
  return $
     ulist $
      [[href "?StudyProgram/list"
             [stprogIcon, nbsp, htxt $ t "Degree programs"]],
       [href "?AdvisorStudyProgram/list"
             [adprogIcon, nbsp, htxt $ t "Master programs"]],
       [href "?search" [searchIcon, nbsp, htxt $ t "Search modules"]]] ++
      (if login==Nothing
       then []
       else [[href ("?Category/user")   [htxt $ t "My modules"]]
            ,[href "?AdvisorStudyProgram/new" [htxt $ t "New master program"]]
            ])

--- The title of this application (shown in the header).
spiceyTitle :: String
spiceyTitle = "Module Information System"

--- Adds the basic page layout to a view.
addLayout :: ViewBlock -> IO ViewBlock
addLayout viewblock = do
  login      <- getSessionLogin
  sinfo      <- getUserSessionInfo
  usermenu   <- getUserMenu login sinfo
  msg        <- getPageMessage
  admin      <- isAdmin
  (routemenu1,routemenu2) <- getRouteMenus
  let adminmenu =  HtmlStruct "ul" [] $
                     map litem routemenu2 ++
                     [litem [htxt " "] `addClass` "divider"] ++
                     map litem routemenu1
  let (mainTitle,mainDoc) =
          case viewblock of
            (HtmlStruct "h1" [] t : hexps) -> (t,hexps)
            _ -> ([htxt (translate sinfo spiceyTitle)], viewblock)
  return $
    stdNavBar usermenu (if admin then Just adminmenu else Nothing)
              login sinfo ++
    [blockstyle "container-fluid" $
      [HtmlStruct "header" [("class","jumbotron")] [h1 mainTitle],
       if null msg
        then HtmlStruct "header" [("class","pagemessage pagemessage-empty")]
                        [nbsp]
        else HtmlStruct "header" [("class","pagemessage")] [htxt msg],
       blockstyle "row"
        [blockstyle "col-md-12" mainDoc]] ++
      [hrule,
       HtmlStruct "footer" []
        [par [htxt "powered by",
              withELink $
                href "http://www.informatik.uni-kiel.de/~pakcs/spicey"
                     [image "images/spicey-logo.png" "Spicey"],
              htxt "Framework"]]]]

--- Create a side menu containing a (possibly empty) title and a list of items:
titledSideMenu :: String -> [[HtmlExp]] -> [HtmlExp]
titledSideMenu title items =
  (if null title
   then []
   else [HtmlStruct "small" [] [htxt title]]) ++
  [ulist items `addClass` "nav nav-sidebar"]

--- Create contents in the main page area with a side menu.
mainContentsWithSideMenu :: [[HtmlExp]] -> [HtmlExp] -> [HtmlExp]
mainContentsWithSideMenu menuitems contents =
  [blockstyle "row"
    [blockstyle "col-sm-3 col-md-3"
      [blockstyle "well nav-sidebar" (titledSideMenu "" menuitems)],
       blockstyle "col-sm-9 col-md-9" contents]]

-- Standard navigation bar at the top.
-- The first argument is the route menu (a ulist).
-- The second argument is the possible admin menu (a ulist).
-- The third argument is the possible login name.
stdNavBar :: HtmlExp -> Maybe HtmlExp -> Maybe String -> UserSessionInfo
          -> [HtmlExp]
stdNavBar routemenu adminmenu login sinfo =
  [blockstyle "navbar navbar-inverse navbar-fixed-top"
    [blockstyle "container-fluid"
      [navBarHeaderItem,
       HtmlStruct "div" [("id","topnavbar"),
                         ("class","navbar-collapse collapse")]
         [routemenu `addClass` "nav navbar-nav",
          appendDropdownItem
            (ulist
              [maybe [href "?login" [loginIcon, nbsp, htxt (t "Login")]]
                     (\n -> [style "navbar-text"
                              [href "?login"
                                    [logoutIcon, nbsp, htxt (t "Logout")]
                              ,htxt $ " (", style "text-success" [userIcon]
                              ,htxt $ " "++n++")"
                              ]
                            ])
                     login
              ,[if languageOfSession sinfo == English
                  then href "?langDE" [htxt "[Deutsch]"]
                  else href "?langEN" [htxt "[English]"]]]
             `addClass` "nav navbar-nav navbar-right")]]
    ]
  ]
 where
  t = translate sinfo

  navBarHeaderItem =
    blockstyle "navbar-header"
      [HtmlStruct "button"
        [("type","button"),("class","navbar-toggle collapsed"),
         ("data-toggle","collapse"),("data-target","#topnavbar"),
         ("aria-expanded","false"),("aria-controls","navbar")]
        [textstyle "sr-only" "Toggle navigation",
         textstyle "icon-bar" "",
         textstyle "icon-bar" "",
         textstyle "icon-bar" ""],
       href "?" [homeIcon, htxt " MDB"] `addClass` "navbar-brand"]
       
  appendDropdownItem (HtmlStruct tag ats items) =
    HtmlStruct tag ats
      (take (length items - 1) items ++
       maybe [] (\m -> [adminDropDownMenu m]) adminmenu ++
       [gotoDropDownMenu] ++
       [last items])

  -- The admin menu as a dropdown menu (represented as a HTML list item).
  -- The first argument is the admin menu (a ulist).
  adminDropDownMenu :: HtmlExp -> HtmlExp
  adminDropDownMenu adminmenu =
    HtmlStruct "li" [("class","dropdown")]
       [href "#" [htxt $ "Administrator", bold [htxt " "] `addClass` "caret"]
         `addAttrs` [("class","dropdown-toggle"),("data-toggle","dropdown")],
        adminmenu `addClass` "dropdown-menu"]

  gotoDropDownMenu =
    HtmlStruct "li" [("class","dropdown")]
     [href "#" [htxt $ t "Go to", bold [htxt " "] `addClass` "caret"]
       `addAttrs` [("class","dropdown-toggle"),("data-toggle","dropdown")],
      HtmlStruct "ul" []
        (litem [href "?main"
                     [htxt $ t "Main page of the module information system"]]
           : extUrls)
        `addClass` "dropdown-menu"]

  extUrls =
   [toEHref "http://www.inf.uni-kiel.de"
            (t "Department of Computer Science")
   ,toEHref "http://www.uni-kiel.de" "CAU Kiel"
   ,toEHref "http://univis.uni-kiel.de/" "UnivIS"
   ,litem [htxt " "] `addClass` "divider"
   ,litem [htxt $ t "Supported by:"] `addClass` "dropdown-header"
   ,toEHref "http://www.curry-language.org"
            ("Curry ("++t "programming language"++")")
   ,toEHref "http://www.informatik.uni-kiel.de/~pakcs/spicey"
            "Spicey (Web Framework)"
   ,toEHref "http://getbootstrap.com/"
            "Twitter Bootstrap (Style Sheets)"
   ]

  toEHref url s = litem [ehref url [arrowIcon, nbsp, htxt s]]


getForm :: ViewBlock -> IO HtmlForm
getForm viewBlock = case viewBlock of
  [HtmlText ""] ->
       return $ HtmlForm "forward to Spicey"
                  [formMetaInfo [("http-equiv","refresh"),
                                 ("content","1; url=show.cgi")]]
                  [par [htxt "You will be forwarded..."]]
  _ -> do
    cookie  <- sessionCookie
    body    <- addLayout viewBlock
    return $ HtmlForm spiceyTitle
                ([responsiveView, cookie, icon, MultipleHandlers] ++
                 map (\f -> FormCSS $ "css/"++f++".css")
                     ["bootstrap.min","spicey"])
                (body ++
                 map (\f -> HtmlStruct "script" [("src","js/"++f++".js")] [])
                     ["jquery.min","bootstrap.min"])
 where
   responsiveView =
     formMetaInfo [("name","viewport"),
                   ("content","width=device-width, initial-scale=1.0")]

   icon = HeadInclude (HtmlStruct "link"
                                  [("rel","shortcut icon"),
                                   ("href","favicon.ico")] [])


-------------------------------------------------------------------------
-- Action performed when a "cancel" button is pressed.
-- In this case, a message is shown.
cancelOperation :: IO ()
cancelOperation = do
  inproc <- isInProcess
  if inproc then removeCurrentProcess else done
  setPageMessage $ (if inproc then "Process" else "Operation") ++ " cancelled"

-- dummy-controller to display an error string
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

-- dummy-controller to display an error in HTML format
displayHtmlError :: [HtmlExp] -> Controller
displayHtmlError hexps = do
  inproc <- isInProcess
  if inproc then removeCurrentProcess else done
  setPageMessage ("Error occurred!" ++
                  if inproc then " Process terminated!" else "")
  return hexps

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

dateToHtml :: ClockTime -> HtmlExp
dateToHtml ct = textstyle "type_calendartime" (toDayString (toUTCTime ct))

maybeDateToHtml :: Maybe ClockTime -> HtmlExp
maybeDateToHtml ct =
  textstyle "type_calendartime" (maybe "" (toDayString . toUTCTime) ct)

userDefinedToHtml :: Show a => a -> HtmlExp
userDefinedToHtml ud = textstyle "type_string" (show ud)

maybeUserDefinedToHtml :: Show a => Maybe a -> HtmlExp
maybeUserDefinedToHtml ud = textstyle "type_string" (maybe "" show ud)

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
hrefExtModule ref hexps = withELink $ hrefModule ref hexps

--- Hypertext reference to a module instance:
hrefModInst :: String -> [HtmlExp] -> HtmlExp
hrefModInst = spEHrefBlock

--- Hypertext reference to a UnivIS information:
hrefUnivisInfo :: String -> [HtmlExp] -> HtmlExp
hrefUnivisInfo = spEHrefInfoBlock

--- Hypertext reference to a UnivIS information with "danger" rendering:
hrefUnivisDanger :: String -> [HtmlExp] -> HtmlExp
hrefUnivisDanger = spEHrefDangerBlock

--- Hypertext reference in Spicey (rendered as a block button):
spHref :: String -> [HtmlExp] -> HtmlExp
spHref ref hexps =
  href ref hexps `addClass` "btn btn-sm btn-default"

--- Hypertext reference in Spicey (rendered as a primary button):
hrefPrimButton :: String -> [HtmlExp] -> HtmlExp
hrefPrimButton ref hexps =
  href ref hexps `addClass` "btn btn-sm btn-primary"

--- Hypertext reference in Spicey (rendered as a block button):
spHrefBlock :: String -> [HtmlExp] -> HtmlExp
spHrefBlock ref hexps =
  href ref hexps `addClass` "btn btn-sm btn-default btn-block"

--- Hypertext reference in Spicey (rendered as an info block button):
spHrefInfoBlock :: String -> [HtmlExp] -> HtmlExp
spHrefInfoBlock ref hexps =
  href ref hexps `addClass` "btn btn-info btn-block"

--- Hypertext reference in Spicey (rendered as an info block button):
spHrefDangerBlock :: String -> [HtmlExp] -> HtmlExp
spHrefDangerBlock ref hexps =
  href ref hexps `addClass` "btn btn-danger btn-block"

--- External hypertext reference in Spicey (rendered as a block button):
spEHref :: String -> [HtmlExp] -> HtmlExp
spEHref ref hexps = withELink $ spHref ref hexps

--- External hypertext reference in Spicey (rendered as a block button):
spEHrefBlock :: String -> [HtmlExp] -> HtmlExp
spEHrefBlock ref hexps = withELink $ spHrefBlock ref hexps

--- External hypertext reference in Spicey (rendered as an info block button):
spEHrefInfoBlock :: String -> [HtmlExp] -> HtmlExp
spEHrefInfoBlock ref hexps = withELink $ spHrefInfoBlock ref hexps

--- External hypertext reference in Spicey (rendered as a danger block button):
spEHrefDangerBlock :: String -> [HtmlExp] -> HtmlExp
spEHrefDangerBlock ref hexps = withELink $ spHrefDangerBlock ref hexps

--- Input button in Spicey (rendered as a default button):
spButton :: String -> HtmlHandler -> HtmlExp
spButton label handler =
  button label handler `addClass` "btn btn-default"

--- Primary input button in Spicey (rendered as a default primary button):
spPrimButton :: String -> HtmlHandler -> HtmlExp
spPrimButton label handler =
  button label handler `addClass` "btn btn-primary"

--- Small input button in Spicey (rendered as a small button):
spSmallButton :: String -> HtmlHandler -> HtmlExp
spSmallButton label handler =
  button label handler `addClass` "btn btn-sm btn-default"

--- Small input button in Spicey (rendered as a small primary button):
spSmallPrimaryButton :: String -> HtmlHandler -> HtmlExp
spSmallPrimaryButton label handler =
  button label handler `addClass` "btn btn-sm btn-primary"

--- Short selectionInitial input field:
spShortSelectionInitial :: CgiRef -> [(String,String)] -> Int -> HtmlExp
spShortSelectionInitial cref sellist sel =
  selectionInitial cref sellist sel `addClass` "shorttextinput"

--- Standard table in Spicey.
spTable :: [[[HtmlExp]]] -> HtmlExp
spTable items = table items  `addClass` "table table-hover table-condensed"

--- Headed table in Spicey.
spHeadedTable :: [[[HtmlExp]]] -> HtmlExp
spHeadedTable items =
  headedTable items  `addClass` "table table-hover table-condensed"

--- Makes a link to an external link.
withELink :: HtmlExp -> HtmlExp
withELink hexp = hexp `addAttr` ("target","_blank")

--- Adds a title attribute to an HTML element.
addTitle :: HtmlExp -> String -> HtmlExp
addTitle hexp title = hexp `addAttr` ("title",title)

--------------------------------------------------------------------------
-- Icons:

homeIcon :: HtmlExp
homeIcon   = glyphicon "home"

userIcon :: HtmlExp
userIcon   = glyphicon "user"

loginIcon :: HtmlExp
loginIcon  = glyphicon "log-in"

logoutIcon :: HtmlExp
logoutIcon = glyphicon "log-out"

searchIcon :: HtmlExp
searchIcon = glyphicon "search"

arrowIcon :: HtmlExp
arrowIcon  = glyphicon "arrow-right"

stprogIcon :: HtmlExp
stprogIcon = glyphicon "book"

adprogIcon :: HtmlExp
adprogIcon = glyphicon "road"

glyphicon :: String -> HtmlExp
glyphicon n = textstyle ("glyphicon glyphicon-"++n) ""

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
--- If the SQLResult is an error, display it, otherwise apply the
--- controller (first argument) to the result.
continueOrError :: (a -> Controller) -> SQLResult a -> Controller
continueOrError = either (displayError . show)

--------------------------------------------------------------------------
