module View.UnivisInfo (
 wUnivisInfo, tuple2UnivisInfo, univisInfo2Tuple, wUnivisInfoType,
 showUnivisInfoView, listUnivisInfoView, showUnivisLinks,
 loadUnivisView, missingMDBMessage, missingUnivISMessage
 ) where

import HTML.WUI
import HTML.Base
import HTML.Styles.Bootstrap4
import Time
import Sort
import System.Spicey
import MDB
import View.MDBEntitiesToHtml
import System.Helpers
import List

--- The WUI specification for the entity type UnivisInfo.
wUnivisInfo :: WuiSpec (String,String,Int,String)
wUnivisInfo =
  withRendering (w4Tuple wRequiredString wRequiredString wInt wRequiredString)
   (renderLabels univisInfoLabelList)

--- Transformation from data of a WUI form to entity type UnivisInfo.
tuple2UnivisInfo :: UnivisInfo -> (String,String,Int,String) -> UnivisInfo
tuple2UnivisInfo univisInfoToUpdate (code ,term ,year ,uRL) =
  setUnivisInfoCode
   (setUnivisInfoTerm
     (setUnivisInfoYear (setUnivisInfoURL univisInfoToUpdate uRL) year) term)
   code

--- Transformation from entity type UnivisInfo to a tuple
--- which can be used in WUI specifications.
univisInfo2Tuple :: UnivisInfo -> (String,String,Int,String)
univisInfo2Tuple univisInfo =
  (univisInfoCode univisInfo,univisInfoTerm univisInfo
  ,univisInfoYear univisInfo,univisInfoURL univisInfo)

--- WUI Type for editing or creating UnivisInfo entities.
--- Includes fields for associated entities.
wUnivisInfoType :: UnivisInfo -> WuiSpec UnivisInfo
wUnivisInfoType univisInfo =
  transformWSpec (tuple2UnivisInfo univisInfo,univisInfo2Tuple) wUnivisInfo

------------------------------------------------------------------------
--- Supplies a view to show the details of a UnivisInfo.
showUnivisInfoView :: UnivisInfo -> Controller -> [HtmlExp]
showUnivisInfoView univisInfo controller =
  univisInfoToDetailsView univisInfo ++
   [spButton "back to UnivisInfo list" (nextController controller)]

--- Compares two UnivisInfo entities. This order is used in the list view.
leqUnivisInfo :: UnivisInfo -> UnivisInfo -> Bool
leqUnivisInfo x1 x2 =
  (univisInfoCode x1,univisInfoTerm x1,univisInfoYear x1,univisInfoURL x1) <=
   (univisInfoCode x2,univisInfoTerm x2,univisInfoYear x2,univisInfoURL x2)

--- Supplies a list view for a given list of UnivisInfo entities.
--- Shows also buttons to show, delete, or edit entries.
--- The arguments are the list of UnivisInfo entities
--- and the controller functions to show, delete and edit entities.
listUnivisInfoView :: [UnivisInfo] -> [HtmlExp]
listUnivisInfoView univisInfos =
  [h1 [htxt "UnivisInfo list"]
  ,spTable
    ([take 4 univisInfoLabelList] ++
     map listUnivisInfo (mergeSortBy leqUnivisInfo univisInfos))]
  where listUnivisInfo :: UnivisInfo -> [[HtmlExp]]
        listUnivisInfo univisInfo = univisInfoToListView univisInfo


------------------------------------------------------------------------
-- Show a web page with the UnivIS links of a module in a given semester.

showUnivisLinks :: ModData -> (String,Int) -> Maybe User -> [String] -> Bool
                -> [HtmlExp]
showUnivisLinks md sem@(term,year) lecturer urls admin =
  [h1 [htxt $ "Modul "++modDataCode md++" im "++showSemester sem]] ++
  maybe [] (\u -> [par [htxt "Dozent: ", userToHtmlView u]]) lecturer ++
  if null urls
   then [par [htxt "Keine Einträge im UnivIS gefunden."]] ++
        if admin then [par [mailButton]] else []
   else [h3 [htxt "Links zu Einträgen im UnivIS:"],
         ulist (map (\url -> [ehref url [htxt url]]) urls)] ++
        if admin && lecturer==Nothing then [par [mailButton]] else []
 where
  mailButton = 
   hrefPrimSmButton ("?UnivisInfo/email/" ++ showModDataKey md ++ "/"
                                          ++ term ++ "/" ++ show year)
     [htxt "Mail mit Korrekturbitte an Modulverantwortlichen senden"]

missingUnivISMessage :: ModData -> (String,Int) -> String
missingUnivISMessage md sem =
     "Lieber Modulverantwortlicher,\n\n"++
     "das Modul "++modDataCode md++" ist fuer das "++showSemester sem++"\n"++
     "noch nicht im UnivIS angekuendigt, obwohl es in der Planung\n"++
     "der Moduldatenbank aufgelistet ist. Bitte ueberpruefen und korrigieren\n"++
     "Sie dies im UnivIS oder der Moduldatenbank, damit die Angaben\n"++
     "im UnivIS und der Moduldatenbank konsistent sind.\n\n"++
     "Viele Gruesse vom Moduldatenbankadministrator"

missingMDBMessage :: ModData -> (String,Int) -> String
missingMDBMessage md sem =
     "Lieber Modulverantwortlicher,\n\n"++
     "das Modul "++modDataCode md++" ist fuer das "++showSemester sem++"\n"++
     "im UnivIS angekuendigt, obwohl es in der Planung der Moduldatenbank\n"++
     "nicht aufgelistet ist. Bitte ueberpruefen und korrigieren\n"++
     "Sie dies im UnivIS oder der Moduldatenbank, damit die Angaben\n"++
     "im UnivIS und der Moduldatenbank konsistent sind.\n\n"++
     "Viele Gruesse vom Moduldatenbankadministrator"

------------------------------------------------------------------------
--- A view to load data from a particular semester from UnivIS.
loadUnivisView :: ((String,Int) -> Controller) -> (String,Int) -> [HtmlExp]
loadUnivisView loadcontroller cursem =
    [h1 [htxt "Daten aus dem UnivIS der CAU laden"],
     par [htxt "Daten aus dem UnivIS für das Semester ",
          spShortSelectionInitial insem semSelection
                                  (findSemesterSelection cursem cursem),
          spButton "jetzt übernehmen" loadData,
          htxt " (dauert etwas länger!)"]]
 where
  insem free

  semSelection = map (\(s,i) -> (showSemester s,show i))
                     (zip (semesterSelection cursem) [0..])

  loadData env =
    let semi = maybe 0 id (findIndex (\(_,i) -> i==(env insem)) semSelection)
     in loadcontroller (semesterSelection cursem !! semi) >>= getPage


------------------------------------------------------------------------
