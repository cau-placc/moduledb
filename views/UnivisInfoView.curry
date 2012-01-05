module UnivisInfoView (
 wUnivisInfo, tuple2UnivisInfo, univisInfo2Tuple, wUnivisInfoType,
 blankUnivisInfoView, createUnivisInfoView, editUnivisInfoView,
 showUnivisInfoView, listUnivisInfoView, showUnivisLinks,
 loadUnivisView
 ) where

import WUI
import HTML
import Time
import Sort
import Spicey
import MDB
import MDBEntitiesToHtml
import Helpers
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

--- Supplies a WUI form to create a new UnivisInfo entity.
--- The fields of the entity have some default values.
blankUnivisInfoView
 :: (Bool -> (String,String,Int,String) -> Controller) -> [HtmlExp]
blankUnivisInfoView controller = createUnivisInfoView [] [] 0 [] controller

--- Supplies a WUI form to create a new UnivisInfo entity.
--- Takes default values to be prefilled in the form fields.
createUnivisInfoView
 :: String -> String -> Int -> String
  -> (Bool -> (String,String,Int,String) -> Controller) -> [HtmlExp]
createUnivisInfoView defaultCode defaultTerm defaultYear defaultURL
                     controller =
  let initdata = (defaultCode,defaultTerm,defaultYear,defaultURL)
      
      wuiframe = wuiEditForm "new UnivisInfo" "create"
                  (controller False initdata)
      
      (hexp ,handler) = wuiWithErrorForm wUnivisInfo initdata
                         (nextControllerForData (controller True))
                         (wuiFrameToForm wuiframe)
   in wuiframe hexp handler

--- Supplies a WUI form to edit the given UnivisInfo entity.
--- Takes also associated entities and a list of possible associations
--- for every associated entity type.
editUnivisInfoView
 :: UnivisInfo -> (Bool -> UnivisInfo -> Controller) -> [HtmlExp]
editUnivisInfoView univisInfo controller =
  let initdata = univisInfo
      
      wuiframe = wuiEditForm "edit UnivisInfo" "change"
                  (controller False initdata)
      
      (hexp ,handler) = wuiWithErrorForm (wUnivisInfoType univisInfo) initdata
                         (nextControllerForData (controller True))
                         (wuiFrameToForm wuiframe)
   in wuiframe hexp handler

--- Supplies a view to show the details of a UnivisInfo.
showUnivisInfoView :: UnivisInfo -> Controller -> [HtmlExp]
showUnivisInfoView univisInfo controller =
  univisInfoToDetailsView univisInfo ++
   [button "back to UnivisInfo list" (nextController controller)]

--- Compares two UnivisInfo entities. This order is used in the list view.
leqUnivisInfo :: UnivisInfo -> UnivisInfo -> Bool
leqUnivisInfo x1 x2 =
  (univisInfoCode x1,univisInfoTerm x1,univisInfoYear x1,univisInfoURL x1) <=
   (univisInfoCode x2,univisInfoTerm x2,univisInfoYear x2,univisInfoURL x2)

--- Supplies a list view for a given list of UnivisInfo entities.
--- Shows also buttons to show, delete, or edit entries.
--- The arguments are the list of UnivisInfo entities
--- and the controller functions to show, delete and edit entities.
listUnivisInfoView
 :: [UnivisInfo] -> (UnivisInfo -> Controller) -> (UnivisInfo -> Controller)
  -> (UnivisInfo -> Bool -> Controller) -> [HtmlExp]
listUnivisInfoView univisInfos showUnivisInfoController
                   editUnivisInfoController deleteUnivisInfoController =
  [h1 [htxt "UnivisInfo list"]
  ,table
    ([take 4 univisInfoLabelList] ++
     map listUnivisInfo (mergeSort leqUnivisInfo univisInfos))]
  where listUnivisInfo :: UnivisInfo -> [[HtmlExp]]
        listUnivisInfo univisInfo =
          univisInfoToListView univisInfo ++
           if True then [] else -- only show list, no changes allowed
           [[button "show"
              (nextController (showUnivisInfoController univisInfo))
            ,button "edit"
              (nextController (editUnivisInfoController univisInfo))
            
            ,button "delete"
              (confirmNextController
                (h3
                  [htxt
                    (concat
                      ["Really delete entity \""
                      ,univisInfoToShortView univisInfo,"\"?"])])
                (deleteUnivisInfoController univisInfo))]]


------------------------------------------------------------------------
-- Show a web page with the UnivIS links of a module in a given semester.

showUnivisLinks :: ModData -> (String,Int) -> Maybe User -> [String]
                -> Bool ->  (String -> Controller)
                -> [HtmlExp]
showUnivisLinks md sem lecturer urls admin emailcontroller =
  [h1 [htxt $ "Modul "++modDataCode md++" im "++showSemester sem]] ++
  maybe [] (\u -> [par [htxt "Dozent: ", userToHtmlView u]]) lecturer ++
  if null urls
   then [par [htxt "Keine Einträge im UnivIS gefunden."]] ++
        if admin
        then [par [button mailButtonTitle
                     (nextController (emailcontroller missingMDBMessage))]]
        else []
   else [h3 [htxt "Links zu Einträgen im UnivIS:"],
         ulist (map (\url -> [ehref url [htxt url]]) urls)] ++
        if admin && lecturer==Nothing
        then [par [button mailButtonTitle
                     (nextController (emailcontroller missingUnivISMessage))]]
        else []
 where
   mailButtonTitle = "Mail mit Korrekturbitte an Modulverantwortlichen senden"

   missingMDBMessage =
     "Lieber Modulverantwortlicher,\n\n"++
     "das Modul "++modDataCode md++" ist fuer das "++showSemester sem++"\n"++
     "noch nicht im UnivIS angekuendigt, obwohl es in der Planung\n"++
     "der Moduldatenbank aufgelistet ist. Bitte ueberpruefen und korrigieren\n"++
     "Sie dies im UnivIS oder der Moduldatenbank, damit die Angaben\n"++
     "im UnivIS und der Moduldatenbank konsistent sind.\n\n"++
     "Viele Gruesse vom Moduldatenbankadministrator"

   missingUnivISMessage =
     "Lieber Modulverantwortlicher,\n\n"++
     "das Modul "++modDataCode md++" ist fuer das "++showSemester sem++"\n"++
     "im UnivIS angekuendigt, obwohl es in der Planung der Moduldatenbank\n"++
     "nicht aufgelistet ist. Bitte ueberpruefen und korrigieren\n"++
     "Sie dies im UnivIS oder der Moduldatenbank, damit die Angaben\n"++
     "im UnivIS und der Moduldatenbank konsistent sind.\n\n"++
     "Viele Gruesse vom Moduldatenbankadministrator"

------------------------------------------------------------------------
--- Supplies a WUI form to create a new UnivisInfo entity.
--- The fields of the entity have some default values.
loadUnivisView :: ((String,Int) -> Controller) -> [HtmlExp]
loadUnivisView loadcontroller =
    [h1 [htxt "Daten aus dem UnivIS der CAU laden"],
     par [htxt "Daten aus dem UnivIS für das Semester ",
          selectionInitial insem semSelection lowerSemesterSelection,
          button "jetzt übernehmen" loadData,
          htxt " (dauert etwas länger!)"]]
 where
  insem free

  semSelection = map (\(s,i) -> (showSemester s,show i))
                     (zip semesterSelection [0..])

  loadData env =
    let semi = maybe 0 id (findIndex (\(_,i) -> i==(env insem)) semSelection)
     in loadcontroller (semesterSelection!!semi) >>= getForm


