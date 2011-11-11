module CategoryView (
 wCategory, tuple2Category, category2Tuple, wCategoryType, blankCategoryView,
 createCategoryView, editCategoryView, showCategoryView, listCategoryView,
 leqCategory
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
import ModDataView
import ConfigMDB

--- The WUI specification for the entity type Category.
--- It also includes fields for associated entities.
wCategory :: [StudyProgram] -> WuiSpec (String,String,String,Int,StudyProgram)
wCategory studyProgramList =
  withRendering
   (w5Tuple wRequiredString wRequiredString wRequiredString wInt
     (wSelect studyProgramToShortView studyProgramList))
   (renderLabels categoryLabelList)

--- Transformation from data of a WUI form to entity type Category.
tuple2Category
 :: Category -> (String,String,String,Int,StudyProgram) -> Category
tuple2Category categoryToUpdate (name ,shortName ,catKey ,position
                                 ,studyProgram) =
  setCategoryName
   (setCategoryShortName
     (setCategoryCatKey
       (setCategoryPosition
         (setCategoryStudyProgramProgramCategoriesKey categoryToUpdate
           (studyProgramKey studyProgram))
         position)
       catKey)
     shortName)
   name

--- Transformation from entity type Category to a tuple
--- which can be used in WUI specifications.
category2Tuple
 :: StudyProgram -> Category -> (String,String,String,Int,StudyProgram)
category2Tuple studyProgram category =
  (categoryName category,categoryShortName category,categoryCatKey category
  ,categoryPosition category,studyProgram)

--- WUI Type for editing or creating Category entities.
--- Includes fields for associated entities.
wCategoryType
 :: Category -> StudyProgram -> [StudyProgram] -> WuiSpec Category
wCategoryType category studyProgram studyProgramList =
  transformWSpec (tuple2Category category,category2Tuple studyProgram)
   (wCategory studyProgramList)

--- Supplies a WUI form to create a new Category entity.
--- The fields of the entity have some default values.
blankCategoryView
 :: [StudyProgram]
  -> (Bool -> (String,String,String,Int,StudyProgram) -> Controller)
  -> [HtmlExp]
blankCategoryView possibleStudyPrograms controller =
  createCategoryView [] [] [] 0 (head possibleStudyPrograms)
   possibleStudyPrograms controller

--- Supplies a WUI form to create a new Category entity.
--- Takes default values to be prefilled in the form fields.
createCategoryView
 :: String -> String -> String -> Int -> StudyProgram -> [StudyProgram]
  -> (Bool -> (String,String,String,Int,StudyProgram) -> Controller)
  -> [HtmlExp]
createCategoryView defaultName defaultShortName defaultCatKey defaultPosition
                   defaultStudyProgram possibleStudyPrograms controller =
  let initdata = (defaultName,defaultShortName,defaultCatKey,defaultPosition
                 ,defaultStudyProgram)
      
      wuiframe = wuiEditForm "Neue Kategorie" "Anlegen"
                  (controller False initdata)
      
      (hexp ,handler) = wuiWithErrorForm (wCategory possibleStudyPrograms)
                         initdata (nextControllerForData (controller True))
                         (wuiFrameToForm wuiframe)
   in wuiframe hexp handler

--- Supplies a WUI form to edit the given Category entity.
--- Takes also associated entities and a list of possible associations
--- for every associated entity type.
editCategoryView
 :: Category -> StudyProgram -> [StudyProgram]
  -> (Bool -> Category -> Controller) -> [HtmlExp]
editCategoryView category relatedStudyProgram possibleStudyPrograms
                 controller =
  let initdata = category
      
      wuiframe = wuiEditForm "edit Category" "change"
                  (controller False initdata)
      
      (hexp ,handler) = wuiWithErrorForm
                         (wCategoryType category relatedStudyProgram
                           possibleStudyPrograms)
                         initdata (nextControllerForData (controller True))
                         (wuiFrameToForm wuiframe)
   in wuiframe hexp handler

--- Supplies a view to show the details of a Category.
showCategoryView :: Category -> StudyProgram -> Controller -> [HtmlExp]
showCategoryView category relatedStudyProgram controller =
  categoryToDetailsView category relatedStudyProgram ++
   [button "back to Category list" (nextController controller)]

--- Compares two Category entities. This order is used in the list view.
leqCategory :: Category -> Category -> Bool
leqCategory x1 x2 =
  categoryPosition x1 <= categoryPosition x2

--- Supplies a list view for a given list of Category entities.
--- Shows also buttons to show, delete, or edit entries.
--- The arguments are the list of Category entities
--- and the controller functions to show, delete and edit entities.
listCategoryView
 :: Bool -> Maybe String -> Either StudyProgram String
  -> [(Either Category String,[(ModData,[Maybe ModInst],[Bool])])]
  -> [(String,Int)] -> [User]
  -> (Category -> Controller) -> (Category -> Controller)
  -> (Category -> Bool -> Controller)
  -> (Either StudyProgram String -> [(Either Category String,[ModData])]
        -> (String,Int) -> (String,Int) -> Bool -> Controller)
  -> ([ModData] -> Controller)
  -> [HtmlExp]
listCategoryView admin login mbsprog catmods semperiod users
                 showCategoryController
                 editCategoryController deleteCategoryController
                 showCategoryPlanController formatModsController =
  [h1 [htxt $ either studyProgramName id mbsprog],
   table (if admin && null (concatMap snd catmods)
          then [take 4 categoryLabelList] ++
               map listCategory
                    (mergeSort leqCategory
                     (concatMap (\ (c,_) -> either (:[]) (const []) c) catmods))
          else concatMap
                 (\ (mbcat,mods) ->
                    (either (\c->[style "category" (head (listCategory c))])
                            (\s->[style "category" [htxt s]])
                            mbcat :
                     if null mods then []
                     else map (\s -> [style "category" [stringToHtml s]])
                              ("ECTS":map showSemester semperiod)) :
                     map (\ (md,mis,univs) -> modDataToCompactListView md ++
                            if null univs
                            then map (maybe [] showModInst) mis
                            else map (showUnivisInst md)
                                     (zip3 semperiod mis univs))
                         (mergeSort (\ (m1,_,_) (m2,_,_) -> leqModData m1 m2)
                                    mods))
                 catmods)] ++
   (if null (concatMap snd catmods)
    then either
          (\sprog ->
            [par [style "buttonhref"
                   [href ("?listCategory/"++showStudyProgramKey sprog++"/all")
                     [htxt "Alle Module in diesem Studienprogramm anzeigen"]]]])
          (const [])
          mbsprog
    else
     [par $
       maybe
         [bold [htxt $ "Semesterplanung von " ++
                       showSemester currentSemester ++ " bis " ++
                       showSemester currentUpperSemester ++ ": ",
                button "Anzeigen" (showFixedPlan False mbsprog)]]
         (\_ -> [bold [htxt "Semesterplanung"], htxt " von ",
                 selectionInitial fromsem semSelection lowerSemesterSelection,
                 htxt " bis ",
                 selectionInitial tosem semSelection  upperSemesterSelection,
                 htxt ": ",
                 button "Anzeigen" (showPlan False mbsprog),
                 button "Anzeigen mit UnivIS-Abgleich"
                                   (showPlan True mbsprog)])
         login]) ++
   (if admin
    then [par [button "Alle Module formatieren"
                      (nextController
                         (formatModsController
                            ((map (\ (md,_,_)->md)
                                  (concatMap snd catmods)))))]]
    else [])
  where
   fromsem,tosem free

   -- show UnivIS instance of a semester
   showUnivisInst md ((term,year),mbmi,hasinst)
     | hasinst && mbmi/=Nothing = [univisRef [italic [htxt "UnivIS"]]]
     | hasinst                  = [univisRef [textstyle "alertentry" "!UnivIS!"]]
     | mbmi/=Nothing            = [univisRef [italic [htxt "???"]]]
     | otherwise                = [nbsp]
    where univisRef = ehref ("?listUnivisInfo/"++showModDataKey md++"/"
                                               ++term++"/"++show year)

   showModInst mi =
     let miuserkey = modInstUserLecturerModsKey mi
         showUser u = let name = userName u
                       in if length name > 6 then take 5 name ++ "." else name
      in [italic
           [ehref ("?listModInst/"++showModInstKey mi)
                  [htxt (maybe "???" showUser
                               (find (\u -> userKey u == miuserkey) users))]]]

   semSelection = map (\(s,i) -> (showSemester s,show i))
                      (zip semesterSelection [0..])

   showPlan withunivis sprog env = do
    let start = maybe 0 id (findIndex (\(_,i) -> i==(env fromsem)) semSelection)
        stop  = maybe 0 id (findIndex (\(_,i) -> i==(env tosem  )) semSelection)
    showCategoryPlanController sprog
     (map (\ (c,cmods) -> (c,map (\ (m,_,_)->m) cmods)) catmods)
     (semesterSelection!!start) (semesterSelection!!stop) withunivis >>= getForm

   showFixedPlan withunivis sprog _  =
    showCategoryPlanController sprog
     (map (\ (c,cmods) -> (c,map (\ (m,_,_)->m) cmods)) catmods)
     currentSemester currentUpperSemester withunivis >>= getForm

   listCategory :: Category -> [[HtmlExp]]
   listCategory category =
      categoryToListView category ++
       [[button "show" (nextController (showCategoryController category)),
         button "edit" (nextController (editCategoryController category)),
         button "delete"
          (confirmNextController
            (h3
              [htxt
                (concat
                  ["Really delete entity \"",categoryToShortView category
                  ,"\"?"])])
            (deleteCategoryController category))]]
