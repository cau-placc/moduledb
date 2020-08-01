--------------------------------------------------------------------------
--- This module implements some auxiliary operations to support the
--- generic implementation of the Spicey entities.
--------------------------------------------------------------------------

module System.Spicey (
  Controller, EntityController(..), showRoute, editRoute, deleteRoute,
  applyControllerOn,
  redirectController, redirectToDefaultController,
  nextController, nextControllerForData,
  confirmDeletionPage, confirmDeletionPageWithRefs,
  transactionController, transactionBindController,
  getControllerURL,getControllerParams, showControllerURL,
  getPage, wDateType, wBoolean, wUncheckMaybe,
  mainContentsWithSideMenu,
  displayError, displayUrlError, displayHtmlError, cancelOperation,
  renderWUI, renderWUIWithText,
  renderLabels,
  nextInProcessOr,
  selectSemesterFormView,
  stringToHtml, maybeStringToHtml,
  intToHtml,maybeIntToHtml, floatToHtml, maybeFloatToHtml,
  boolToHtml, maybeBoolToHtml, dateToHtml, maybeDateToHtml,
  userDefinedToHtml, maybeUserDefinedToHtml,
  hrefStudyProgram, hrefCategory, smallHrefCategory,
  hrefModule, smallHrefModule, hrefExtModule, hrefModInst,
  hrefUnivisInfo, hrefUnivisDanger,
  withELink,
  spHrefBlock,
  spButton, spPrimButton, spSmallButton, spSmallPrimaryButton,
  spTable, spHeadedTable, addTitle, textWithInfoIcon,
  spShortSelectionInitial,
  setPageMessage, getPageMessage,
  saveLastUrl, getLastUrl, getLastUrlParameters, getLastUrls,
  continueOrError
  ) where

import FilePath         ( (</>) )
import List             ( findIndex, init, last )
import Time

import ConfigMDB        ( baseCGI )
import Config.UserProcesses
import HTML.Base
import HTML.WUI
import HTML.Session
import HTML.Styles.Bootstrap4
import Global
import System.Authentication
import System.Helpers
import System.MultiLang
import System.SessionInfo
import System.Routes
import System.Processes

import Database.CDBI.ER
import MDB              ( runT, runJustT )

--------------------------------------------------------------------------

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


--- The type class `EntityController` contains:
--- * the application of a controller to some entity identified by a key string
--- * an operation to construct a URL route for an entity w.r.t. to a route
---   string
class EntityController a where
  controllerOnKey :: String -> (a -> Controller) -> Controller

  entityRoute :: String -> a -> String
  entityRoute _ _ = error "entityRoute: implementation missing"

--- Returns the URL route to show a given entity.
showRoute :: EntityController a => a -> String
showRoute = entityRoute "show"

--- Returns the URL route to edit a given entity.
editRoute :: EntityController a => a -> String
editRoute = entityRoute "edit"

--- Returns the URL route to delete a given entity.
deleteRoute :: EntityController a => a -> String
deleteRoute = entityRoute "delete"


--- Reads an entity for a given key and applies a controller to it.
applyControllerOn :: Maybe enkey -> (enkey -> DBAction en)
                  -> (en -> Controller) -> Controller
applyControllerOn Nothing _ _ = displayError "Illegal URL"
applyControllerOn (Just userkey) getuser usercontroller =
  runJustT (getuser userkey) >>= usercontroller

--- A controller to redirect to an URL starting with "?"
--- (see implementation of `getPage`).
redirectController :: String -> Controller
redirectController url = return [HtmlText url]

--- Redirect to a page of the default controller.
--- Useful to set the URL route correctly.
redirectToDefaultController :: Controller
redirectToDefaultController = redirectController "?"

nextController :: Controller -> _ -> IO HtmlPage
nextController controller _ = do
  view <- controller
  getPage view

-- for WUIs
nextControllerForData :: (a -> Controller) -> a -> IO HtmlPage
nextControllerForData controller param = do
  view <- controller param
  getPage view

------------------------------------------------------------------------------
--- Generates a page to ask the user for a confirmation to delete an entity
--- specified in the controller URL (of the form "entity/delete/key/...").
--- The yes/no answers are references derived from the controller URL
--- where the second argument is replaced by "destroy"/"show".
--- @param sinfo    - the current info about the user session
--- @param question - a question asked
confirmDeletionPage :: UserSessionInfo -> String -> Controller
confirmDeletionPage sinfo question = do
  (entity,ctrlargs) <- getControllerURL
  case ctrlargs of
    (_:args) -> confirmDeletionPageWithRefs sinfo question
                  (showControllerURL entity ("destroy":args))
                  (showControllerURL entity ("show" : args))
    _ -> displayUrlError

--- Generates a page to ask the user for a confirmation to delete an entity.
--- The yes/no are href buttons where the URLs are provided as arguments.
--- @param sinfo    - the current info about the user session
--- @param question - a question asked
--- @param yesref   - the reference used in the "yes" button
--- @param noref    - the reference used in the "no" button
confirmDeletionPageWithRefs :: UserSessionInfo -> String -> String -> String
                            -> Controller
confirmDeletionPageWithRefs sinfo question yesref noref = return $
  [h3 [htxt question],
   par [hrefPrimSmButton yesref [htxt $ t "Yes"], nbsp,
        hrefPrimSmButton noref  [htxt $ t "No"]]]
 where
  t = translate sinfo

------------------------------------------------------------------------------
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
--- Standard rendering for WUI forms to edit data.
--- @param sinfo      - the UserSessionInfo to select the language
--- @param title      - the title of the WUI form
--- @param buttontag  - the text on the submit button
--- @param cancelurl  - the URL selected if submission is cancelled
--- @param envpar     - environment parameters (e.g., user session data)
--- @param hexp       - the HTML expression representing the WUI form
--- @param handler    - the handler for submitting data
renderWUI :: UserSessionInfo -> String -> String -> String
          -> a -> HtmlExp -> (CgiEnv -> Controller) -> [HtmlExp]
renderWUI sinfo title buttontag cancelurl _ hexp handler =
  [h1 [htxt $ t title],
   blockstyle "editform" [hexp],
   breakline,
   primSmButton (t buttontag) (\env -> handler env >>= getPage), nbsp,
   hrefScndSmButton cancelurl [htxt $ t "Cancel"]]
 where t = translate sinfo

--- Standard rendering for WUI forms to edit data where some
-- additional text explanations are included at the top.
--- @param sinfo      - the UserSessionInfo to select the language
--- @param title      - the title of the WUI form
--- @param buttontag  - the text on the submit button
--- @param cmts       - the explanations to be included at the top
--- @param cancelurl  - the URL selected if submission is cancelled
--- @param envpar     - environment parameters (e.g., user session data)
--- @param hexp       - the HTML expression representing the WUI form
--- @param handler    - the handler for submitting data
renderWUIWithText :: UserSessionInfo -> String -> String -> [HtmlExp]
  -> String -> a -> HtmlExp -> (CgiEnv -> Controller) -> [HtmlExp]
renderWUIWithText sinfo title buttontag cmts cancelurl _ hexp handler =
  [h1 [htxt $ t title]] ++ cmts ++
  [blockstyle "editform" [hexp],
   breakline,
   primSmButton (t buttontag) (\env -> handler env >>= getPage),
   hrefScndSmButton cancelurl [htxt $ t "Cancel"]]
 where t = translate sinfo

--------------------------------------------------------------------------
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
standardMenu :: UserSessionInfo -> [[HtmlExp]]
standardMenu sinfo =
  [[hrefNav "?StudyProgram/list" [htxt $ t "Degree programs"]],
   [hrefNav "?AdvisorStudyProgram/list" [htxt $ t "Master programs"]],
   [rawSearchForm sinfo],
   [hrefNav "?search" [searchIcon, htxt $ t " Search modules"]]
  ]
 where t = translate sinfo


-- This is the translation of the search form produced by
-- `Controller.Search.searchModuleForm`. To avoid cyclic module
-- dependencies, we have to include the generated form here.
-- Be careful with changes in the original form!
rawSearchForm :: UserSessionInfo -> HtmlExp
rawSearchForm sinfo = 
  HtmlStruct "form" [("method","post"), ("action","?search"),
                     ("class","form-inline my-2 my-lg-0"),
                     ("title", searchToolTip sinfo)]
    [HtmlStruct "input" [("type","hidden"), ("name","FORMID"),
                         ("value","Controller.Search.searchModuleForm")] [],
     HtmlStruct "input" [("type","text"), ("class","form-control mr-sm-2"),
                         ("placeholder", t "Quick search"),
                         ("name","FIELD_0")] [],
     HtmlStruct "input" [("type","submit"),
                         ("class","btn btn-outline-success my-2 my-sm-0"),
                         ("name","FIELD_1"), ("value", t "Search!")] []
    ]
 where t = translate sinfo

--- The menu for a user if it he is logged in.
userMenu :: UserSessionInfo -> [(String,[HtmlExp])]
userMenu sinfo =
  maybe (maybe loginMenu
               (const studentMenu)
               (studentLoginOfSession sinfo))
        (const cuserMenu)
        (userLoginOfSession sinfo)
 where
  t = translate sinfo

  loginMenu   = [ ("?User/login",    [htxt (t "...as lecturer")])
                , ("?Student/login", [htxt (t "...as student")])
                ]

  cuserMenu   = [ ("?Category/user",     [htxt $ t "My modules"])
                , ("?Category/lecturer", [htxt $ t "Taught modules"])
                , ("?AdvisorStudyProgram/new",  [htxt $ t "New master program"])
                , ("?User/passwd",       [htxt (t "Change password")])
                , ("?User/logout",       [htxt (t "Logout")])
                ]

  studentMenu = [ ("?Student/showcourses", [htxt (t "Show selected modules")])
                , ("?Student/select",      [htxt (t "Select/change modules")])
                , ("?Student/logout",      [htxt (t "Logout")])
                ]

--- The title of this application (shown in the header).
spiceyTitle :: String
spiceyTitle = "Module Information System"

--- The home URL and brand shown at the left top of the main page.
spiceyHomeBrand :: (String, [HtmlExp])
spiceyHomeBrand = ("?", [mdbHomeIcon, htxt " MDB"])

--- The standard footer of the Spicey page.
spiceyFooter :: [HtmlExp]
spiceyFooter =
  [par [htxt "Version of July 14, 2020, powered by",
        href "http://www.informatik.uni-kiel.de/~pakcs/spicey"
             [image "bt4/img/spicey-logo.png" "Spicey"]
          `addAttr` ("target","_blank"),
        htxt "Framework"]]

--- Create contents in the main page area with a side menu.
mainContentsWithSideMenu :: [[HtmlExp]] -> [HtmlExp] -> [HtmlExp]
mainContentsWithSideMenu menuitems contents =
  [blockstyle "row"
    [blockstyle "col-sm-3 col-md-3"
      [blockstyle "card" (titledSideMenu "" menuitems)],
       blockstyle "col-sm-9 col-md-9" contents]]

getPage :: ViewBlock -> IO HtmlPage
getPage viewBlock = case viewBlock of
  [HtmlText ""]          -> return $ redirectPage baseCGI
  [HtmlText ('?':route)] -> return $ redirectPage ('?':route)
  _ -> do
    hassession <- doesSessionExist
    urlparam   <- getUrlParameter
    sinfo      <- getUserSessionInfo
    msg        <- getPageMessage
    lasturl    <- getLastUrl
    admin      <- isAdmin
    (routemenunews,routemenuothers) <- getRouteMenus
    let adminmenu = map addDropDownItemClass
                        (routemenuothers ++
                         [href "?StudentCourse/conflicts"
                               [htxt "Zeige Studienkonflikte"]]) ++
                    [blockstyle "dropdown-divider" []] ++
                    map addDropDownItemClass routemenunews
        body      = if hassession then viewBlock
                                  else cookieInfo urlparam
        title     = translate sinfo spiceyTitle
    withSessionCookie $ bootstrapPage2 favIcon cssIncludes jsIncludes
      title spiceyHomeBrand
      (addNavItemClass $ standardMenu sinfo)
      (rightTopMenu sinfo admin adminmenu)
      0 []  [h1 [htxt title]]
      (messageLine msg lasturl : body) spiceyFooter
 where
  addNavItemClass = map (\i -> ("nav-item", i))
  addDropDownItemClass he = he `addClass` "dropdown-item"

  messageLine msg _ =
    if null msg
      then HtmlStruct "header" [("class","pagemessage pagemessage-empty")]
                      [nbsp] --[htxt ("Last page: "++lasturl)]
      else HtmlStruct "header" [("class","pagemessage")] [htxt msg]
        
  cookieInfo urlparam =
    [ par [ htxt $ "This web site uses cookies for navigation and user " ++
                   "inputs and preferences. In order to proceed, "
          , hrefPrimButton ('?' : urlparam) [htxt "please click here."]] ]
        
  rightTopMenu sinfo admin adminmenu =
    [dropDownMenu
       (maybe [htxt $ t "Login" ++ " ", dropDownIcon]
              (\n -> [userWhiteIcon, htxt $ " " ++ n, dropDownIcon])
              (loginNameOfSession sinfo))
       (map (\ (hr,he) -> href hr he `addClass` "dropdown-item")
            (userMenu sinfo))] ++
    (if admin then [dropDownMenu [htxt $ "Administrator", dropDownIcon]
                                 adminmenu]
              else []) ++
    [gotoDropDownMenu t,
     ("nav-item",
      [if languageOfSession sinfo == English
         then hrefNav "?langDE" [htxt "[Deutsch]"]
         else hrefNav "?langEN" [htxt "[English]"]])]
   where t = translate sinfo

  -- A dropdown menu (represented as a HTML list item).
  -- The first argument is title (as HTML expressions) and
  -- the second argument is the actual menu (a list of elements with
  -- class "dropdown-item").
  dropDownMenu :: [HtmlExp] -> [HtmlExp] -> (String,[HtmlExp])
  dropDownMenu title ddmenu =
    ("nav-item dropdown",
     [hrefNav "#" title
      `addAttrs` [("class","dropdown-toggle"),
                  ("id", "dropdownuser"),
                  ("data-toggle","dropdown"),
                  ("aria-haspopup", "true"),
                  ("aria-expanded", "false")],
     blockstyle "dropdown-menu" ddmenu
       `addAttr` ("area-labelledby", "dropdownuser")])

  gotoDropDownMenu t =
    ("nav-item dropdown",
     [hrefNav "#" [htxt $ t "Go to", dropDownIcon]
      `addAttrs` [("class","dropdown-toggle"),
                  ("id", "dropdowngoto"),
                  ("data-toggle","dropdown"),
                  ("aria-haspopup", "true"),
                  ("aria-expanded", "false")],
      blockstyle "dropdown-menu dropdown-menu-right"
       ((href "?main" [htxt $ t "Main page of the module information system"]
          `addClass` "dropdown-item") : extUrls t)
       `addAttr` ("area-labelledby", "dropdowngoto")])

  extUrls t =
   [ toEHref "http://www.inf.uni-kiel.de"
             [htxt $ t "Department of Computer Science"]
   , toEHref "http://www.uni-kiel.de" [htxt "CAU Kiel"]
   , toEHref "http://univis.uni-kiel.de/" [htxt "UnivIS"]
   , blockstyle "dropdown-divider" []
   , h5 [htxt $ t "Supported by:"] `addClass` "dropdown-header"
   , toEHref "http://www.curry-lang.org"
             [image "bt4/img/curry.svg" "Curry"
                `addAttrs` [("width","24"), ("height","24")],
              htxt $ " Curry (" ++ t "programming language" ++ ")"]
   , toEHref "http://www.informatik.uni-kiel.de/~pakcs/spicey"
             [image "bt4/img/spicey-logo.png" "Spicey"
                `addAttrs` [("height","24")],
              htxt $ t " Spicey (Web Framework)"]
   , toEHref "http://getbootstrap.com/"
             [image "bt4/img/bootstrap.svg" "Bootstrap",
              htxt " Bootstrap (Style Sheets)"]
   ]

  toEHref url he = ehref url he `addClass` "dropdown-item"


favIcon :: String
favIcon = "bt4" </> "img" </> "favicon.ico"

cssIncludes :: [String]
cssIncludes =
  map (\n -> "bt4" </> "css" </> n ++ ".css")
      ["bootstrap.min", "spicey"]

jsIncludes :: [String]
jsIncludes = 
  map (\n -> "bt4" </> "js" </> n ++ ".js")
      ["jquery.slim.min", "bootstrap.bundle.min"]

--------------------------------------------------------------------------
--- A form view to select a semester with a controller for this semester
selectSemesterFormView :: ((String,Int) -> Controller) -> String
                       -> (UserSessionInfo, (String,Int)) -> [HtmlExp]
selectSemesterFormView selcontroller buttontxt (sinfo,cursem) =
  [ spShortSelectionInitial insem semSelection
                            (findSemesterSelection cursem cursem)
  , spPrimButton (t buttontxt) selectHandler ]
 where
  insem free

  t = translate sinfo

  semSelection = map (\(s,i) -> (showSemester s,show i))
                     (zip (semesterSelection cursem) [0..])

  selectHandler env =
    let semi = maybe 0 id (findIndex (\(_,i) -> i==(env insem)) semSelection)
    in selcontroller (semesterSelection cursem !! semi) >>= getPage

-------------------------------------------------------------------------
-- Action performed when a "cancel" button is pressed.
-- In this case, a message is shown.
cancelOperation :: IO ()
cancelOperation = do
  inproc <- isInProcess
  if inproc then removeCurrentProcess else done
  setPageMessage $ (if inproc then "Process" else "Operation") ++ " cancelled"

-- A controller to display an URL error.
displayUrlError :: Controller
displayUrlError = displayError "Illegal URL"

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

-- Convert standard datatype values to HTML representation
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
smallHrefCategory = ehrefInfoBadge

--- Hypertext reference to a module:
hrefModule :: String -> [HtmlExp] -> HtmlExp
hrefModule ref hexps = hrefInfoSmButton ref hexps

--- Small hypertext reference to a module:
smallHrefModule :: String -> [HtmlExp] -> HtmlExp
smallHrefModule = ehrefInfoBadge

--- Hypertext reference to an external module:
hrefExtModule :: String -> [HtmlExp] -> HtmlExp
hrefExtModule ref hexps = withELink $ hrefModule ref hexps

--- Hypertext reference to a module instance:
hrefModInst :: String -> [HtmlExp] -> HtmlExp
hrefModInst = spEHrefBlock

--- Hypertext reference to a UnivIS information:
hrefUnivisInfo :: String -> [HtmlExp] -> HtmlExp
hrefUnivisInfo = ehrefInfoBadge

--- Hypertext reference to a UnivIS information with "danger" rendering:
hrefUnivisDanger :: String -> [HtmlExp] -> HtmlExp
hrefUnivisDanger = ehrefDangBadge

--- Hypertext reference in Spicey (rendered as a block button):
spHref :: String -> [HtmlExp] -> HtmlExp
spHref ref hexps =
  href ref hexps `addClass` "btn btn-sm btn-default"

--- Hypertext reference in Spicey (rendered as a block button):
spHrefBlock :: String -> [HtmlExp] -> HtmlExp
spHrefBlock ref hexps =
  href ref hexps `addClass` "btn btn-sm btn-default btn-block"

--- Hypertext reference in Spicey (rendered as an info block button):
spHrefInfoBlock :: String -> [HtmlExp] -> HtmlExp
spHrefInfoBlock ref hexps =
  href ref hexps `addClass` "btn btn-info btn-block"

--- Hypertext reference in Spicey (rendered as a danger block button):
spHrefDangerBlock :: String -> [HtmlExp] -> HtmlExp
spHrefDangerBlock ref hexps =
  href ref hexps `addClass` "btn btn-danger btn-block"

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
spButton = scndSmButton

--- Primary input button in Spicey (rendered as a default primary button):
spPrimButton :: String -> HtmlHandler -> HtmlExp
spPrimButton = primSmButton

--- Small input button in Spicey (rendered as a small button):
spSmallButton :: String -> HtmlHandler -> HtmlExp
spSmallButton = scndSmButton

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

--- Makes a link to an external reference.
withELink :: HtmlExp -> HtmlExp
withELink hexp = hexp `addAttr` ("target","_blank")

--- Adds a title attribute to an HTML element.
addTitle :: HtmlExp -> String -> HtmlExp
addTitle hexp title = hexp `addAttr` ("title",title)

--------------------------------------------------------------------------
-- Icons:

--- User (white) icon:
mdbHomeIcon :: HtmlExp
mdbHomeIcon =
  image "bt4/img/book.svg" "MDB"
    `addAttrs` [("width","32"), ("height","32")]

--- User (white) icon:
userWhiteIcon :: HtmlExp
userWhiteIcon =
  image "bt4/img/user-white.svg" "User"
    `addAttrs` [("width","32"), ("height","32")]

--- Drowdown icon:
dropDownIcon :: HtmlExp
dropDownIcon = image "bt4/img/caret-down-white.svg" "Open"

--- Search icon:
searchIcon :: HtmlExp
searchIcon = image "bt4/img/search.svg" "Search"

--- Info icon:
infoIcon :: HtmlExp
infoIcon = image "bt4/img/info-circle-fill.svg" "Info"

--- Some additional information under an info icon.
textWithInfoIcon :: String -> HtmlExp
textWithInfoIcon s = infoIcon `addTitle` s

--------------------------------------------------------------------------
-- The page messages are implemented by a session store.
-- We define a global variable to store a message which is shown
-- in the next HTML page of a session.

--- Definition of the session state to store the page message (a string).
pageMessage :: Global (SessionStore String)
pageMessage =
  global emptySessionStore (Persistent (inSessionDataDir "pageMessage"))

--- Gets the page message and delete it.
getPageMessage :: IO String
getPageMessage = do
  msg <- getSessionData pageMessage ""
  removeSessionData pageMessage
  return msg

--- Set the page message of the current session.
setPageMessage :: String -> IO ()
setPageMessage msg = putSessionData pageMessage msg

--------------------------------------------------------------------------
-- Another example for using sessions.
-- We store the list of last selected URLs (maximal 3)
-- into  the current session.

--- Definition of the session state to store the last URL (as a string).
lastUrls :: Global (SessionStore [String])
lastUrls =
  global emptySessionStore (Persistent (inSessionDataDir "sessionUrls"))

--- Gets the list of URLs of the current session.
getLastUrls :: IO [String]
getLastUrls = getSessionData lastUrls []

--- Gets the last URL of the current session (or "?").
getLastUrl :: IO String
getLastUrl = do urls <- getLastUrls
                return (if null urls then "?" else '?' : head urls)

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
  putSessionData lastUrls (url : take 2 urls)

--------------------------------------------------------------------------
--- If the SQLResult is an error, display it, otherwise apply the
--- controller (first argument) to the result.
continueOrError :: (a -> Controller) -> SQLResult a -> Controller
continueOrError = either (displayError . show)

--------------------------------------------------------------------------
