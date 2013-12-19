module CategoryView (
 wCategory, tuple2Category, category2Tuple, wCategoryType, blankCategoryView,
 createCategoryView, editCategoryView, showCategoryView, listCategoryView,
 leqCategory, listEmailCorrectionView
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
import UnivisInfoView
import Mail
import System(sleep)
import UserPreferences

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
showCategoryView :: Category -> StudyProgram -> [HtmlExp]
showCategoryView category relatedStudyProgram =
  categoryToDetailsView category relatedStudyProgram ++
   [spHref "?Category/list" [htxt "back to Category list"]]

--- Compares two Category entities. This order is used in the list view.
leqCategory :: Category -> Category -> Bool
leqCategory x1 x2 =
  categoryPosition x1 <= categoryPosition x2

--- Supplies a list view for a given list of Category entities.
--- Shows also buttons to show, delete, or edit entries.
--- Various controller functions to show, delete, edit, and format entities
--- as well as sending emails to request corrections are passed as arguments.
--- The third argument is the current study program or a header string.
--- The fourth argument is the real data to be shown:
--- a list of category information where each info contains
--- a category (or a header string) together with a list of triples containing
--- the modules in this category. Each triple contains:
--- * the module data
--- * a list of possible module instances for the semester period
--    (passed as the fifth argument)
--- * a list of Booleans indicating whether the corresponding module instance
---   has a UnivIS entry (if this list is empty, UnivIS entries should not
---   be shown)
listCategoryView
 :: Bool -> Maybe String -> UserPrefs -> Either StudyProgram String
  -> [(Either Category String,[(ModData,[Maybe (ModInst,Int)],[Bool])])]
  -> [(String,Int)] -> [User]
  -> (Either StudyProgram String -> [(Either Category String,[ModData])]
        -> (String,Int) -> (String,Int) -> Bool -> Bool -> Controller)
  -> ([(String,[ModData])] -> Controller)
  -> (Either StudyProgram String -> [(Either Category String,[ModData])]
        -> (String,Int) -> (String,Int) -> Controller)
  -> [HtmlExp]
listCategoryView admin login prefs mbsprog catmods semperiod users
                 showCategoryPlanController formatCatModsController
                 showEmailCorrectionController =
  [h1 [htxt $ either studyProgramName id mbsprog],
   spTable
    (if admin && null (concatMap snd catmods)
     then [take 4 categoryLabelList] ++
          map listCategory
              (mergeSort leqCategory
               (concatMap (\ (c,_) -> either (:[]) (const []) c) catmods))
     else
      concatMap
        (\ (mbcat,mods) ->
           (either (\c->[style "category" (head (listCategory c))])
                   (\s->[style "category" [htxt s]])
                   mbcat
            : if null mods then []
              else map (\s -> [style "category" [stringToHtml s]])
                       ("ECTS":map showSemester semperiod)) :
           map (\ (md,mis,univs) -> modDataToCompactListView prefs md ++
                  if null univs
                  then map (maybe [] showModInst) mis
                  else map (showUnivisInst md)
                           (zip3 semperiod mis univs))
               (mergeSort (\ (m1,_,_) (m2,_,_) -> leqModData m1 m2) mods))
        catmods)] ++
   (if null (concatMap snd catmods)
    then either
          (\sprog ->
            [par [spHref ("?listCategory/"++showStudyProgramKey sprog++"/all")
                    [htxt $ t "Show all modules in this study program"]]])
          (const [])
          mbsprog
    else
     [par ([bold [htxt $ t "Semester planning"], htxt $ t " from ",
            spShortSelectionInitial fromsem semSelection lowerSemesterSelection,
            htxt $ t " to ",
            spShortSelectionInitial tosem semSelection upperSemesterSelection,
            htxt ": ",
            spSmallButton (t "Show") (showPlan False False mbsprog)] ++
           (maybe []
                  (\_ -> [spSmallButton (t "with UnivIS comparison")
                                   (showPlan True False mbsprog),
                          spSmallButton (t "with master program usage")
                                   (showPlan False True mbsprog)])
                  login) ++
           (if admin
            then [spSmallButton "UnivIS-Abgleich Emails senden"
                           (showCorrectionEmails mbsprog)]
            else []))]) ++
   (if admin
    then [par [spSmallButton "Alle Module formatieren"
                      (nextController
                         (formatCatModsController
                            (map (\ (cat,mods) -> 
                                      (either categoryName id cat,
                                       map (\ (md,_,_)->md) mods))
                                 catmods))),
               spSmallButton "Alle sichtbaren Module formatieren"
                      (nextController
                         (formatCatModsController
                            (map (\ (cat,mods) -> 
                                      (either categoryName id cat,
                                       filter modDataVisible
                                              (map (\ (md,_,_)->md) mods)))
                                 catmods)))]]
    else [])
  where
   fromsem,tosem free

   t = translate prefs

   -- show UnivIS instance of a semester
   showUnivisInst md ((term,year),mbmi,hasinst)
     | hasinst && mbmi/=Nothing = [univisRef [italic [htxt "UnivIS"]]]
     | hasinst                  = [univisRef [textstyle "alertentry" "!UnivIS!"]]
     | mbmi/=Nothing            = [univisRef [italic [htxt "???"]]]
     | otherwise                = [nbsp]
    where univisRef = hrefUnivis ("?listUnivisInfo/"++showModDataKey md++"/"
                                                    ++term++"/"++show year)

   showModInst (mi,num) =
     let miuserkey = modInstUserLecturerModsKey mi
         showUser u = let name = userName u
                       in if length name > 6 then take 5 name ++ "." else name
      in [italic
           [hrefModInst ("?listModInst/"++showModInstKey mi)
                  [htxt (maybe "???" showUser
                               (find (\u -> userKey u == miuserkey) users)),
                   htxt (if num==0 then "" else '(':show num++")")]]]

   semSelection = map (\(s,i) -> (showSemester s,show i))
                      (zip semesterSelection [0..])

   showPlan withunivis withmprogs sprog env = do
    let start = maybe 0 id (findIndex (\(_,i) -> i==(env fromsem)) semSelection)
        stop  = maybe 0 id (findIndex (\(_,i) -> i==(env tosem  )) semSelection)
    showCategoryPlanController sprog
     (map (\ (c,cmods) -> (c,map (\ (m,_,_)->m) cmods)) catmods)
     (semesterSelection!!start) (semesterSelection!!stop)
     withunivis withmprogs >>= getForm

   showCorrectionEmails sprog env = do
    let start = maybe 0 id (findIndex (\(_,i) -> i==(env fromsem)) semSelection)
        stop  = maybe 0 id (findIndex (\(_,i) -> i==(env tosem  )) semSelection)
    showEmailCorrectionController sprog
     (map (\ (c,cmods) -> (c,map (\ (m,_,_)->m) cmods)) catmods)
     (semesterSelection!!start) (semesterSelection!!stop) >>= getForm

   listCategory :: Category -> [[HtmlExp]]
   listCategory category =
      categoryToListView category ++
      [[spHref ("?Category/show/"++showCategoryKey category) [htxt "Anzeigen"]]
      ,[spHref ("?Category/edit/"++showCategoryKey category) [htxt "Ändern"]]
      ,[spHref ("?Category/delete/"++showCategoryKey category) [htxt "Löschen"]]
      ]


--- Supplies a list view for a given list of Category entities.
--- Shows also buttons to show, delete, or edit entries.
--- The arguments are the list of Category entities
--- and the controller functions to show, delete and edit entities.
listEmailCorrectionView
 :: Either StudyProgram String
  -> [(ModData,[Maybe ModInst],[Bool])]
  -> [(String,Int)] -> [User]
  -> [HtmlExp]
listEmailCorrectionView mbsprog modinsts semperiod users =
  [h1 [htxt $ either studyProgramName id mbsprog],
   spTable (map showUnivisInst problemmods),
   spButton "UnivIS-Korrektur-Emails jetzt an alle versenden" sendMails]
  where
   -- show UnivIS instance of a semester
   showUnivisInst (md,sem,reason) =
     [[htxt (modDataCode md)],
      [htxt (showSemester sem)],
      [htxt (if reason=="NOMDB" then "Kein Eintrag in MDB"
                                else "Kein Eintrag in UnivIS")],
      [htxt ("TO: "++ getResponsibleEmail md)]]

   sendMails _ = do
     mailresults <- mapIO sendSingleMail problemmods
     getForm ([h1 [htxt "Mails gesendet!"]] ++
              map (\t -> par [verbatim t]) mailresults)

   sendSingleMail (md,sem,reason) = do
     let to      = getResponsibleEmail md
         from    = adminEmail
         subject = "Modul "++modDataCode md++": "++modDataNameG md
         contents = (if reason=="NOMDB"
                     then missingMDBMessage
                     else missingUnivISMessage) md sem
     sendMail from to subject contents
     sleep 1
     return ("=======\nTO: " ++ to ++ "\n" ++
             "FROM: " ++ from ++ "\n" ++
             "SUBJECT: " ++ subject ++ "\n" ++
             contents)

   -- list of "problematic" modules together with their reason:
   -- "NOMDB" = missing instance in module database
   -- "NOUNIVIS" = missing instance in UnivIS
   problemmods = concatMap (\ (md,mis,univs) ->
                                concatMap (analyzeUnivisInst md)
                                          (zip3 semperiod mis univs))
                           modinsts
     where
      analyzeUnivisInst md (sem,mbmi,hasinst)
        | hasinst && mbmi/=Nothing = []
        | hasinst                  = [(md,sem,"NOMDB")]
        | mbmi/=Nothing            = [(md,sem,"NOUNIVIS")]
        | otherwise                = []

   getResponsibleEmail md =
     let mduserkey = modDataUserResponsibleKey md
      in maybe adminEmail userEmail (find (\u -> userKey u == mduserkey) users)

