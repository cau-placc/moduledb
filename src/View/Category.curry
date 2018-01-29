module View.Category (
 wCategory, tuple2Category, category2Tuple, wCategoryType, blankCategoryView,
 createCategoryView, editCategoryView, showCategoryView, listCategoryView,
 leqCategory, showCategoryInfo, listEmailCorrectionView
 ) where

import Either
import Maybe ( isJust )
import WUI
import HTML.Base
import Time
import Sort
import System.Spicey
import MDB
import View.MDBEntitiesToHtml
import System.Helpers
import List
import View.ModData
import ConfigMDB
import View.UnivisInfo
import Mail
import System(sleep)
import System.SessionInfo
import System.MultiLang

--- The WUI specification for the entity type Category.
--- It also includes fields for associated entities.
wCategory
  :: [StudyProgram]
  -> WuiSpec (String,String,String,String,Int,Int,Int,StudyProgram)
wCategory studyProgramList =
   w8Tuple wRequiredString wString wRequiredString
     (wTextArea (6,70))
     wECTS
     wECTS
     wInt
     (wSelect studyProgramToShortView studyProgramList)
   `withCondition`
       (\ (_,_,_,_,mine,maxe,_,_) -> mine <= maxe)
   `withError`
      "Minimale ECTS-Punkte müssen kleiner als maximale ECTS-Punkte sein!"
   `withRendering`
      (renderLabels categoryLabelList)
 where
  wECTS = wSelect showDiv10 [0,5..1800]
             `withRendering` numwidthRendering

  numwidthRendering [s] = inline [s `addClass` "numwidth"]

--- Transformation from data of a WUI form to entity type Category.
tuple2Category
  :: Category
  -> (String,String,String,String,Int,Int,Int,StudyProgram) -> Category
tuple2Category
    categoryToUpdate
    (name,nameE,shortName,comment,minECTS,maxECTS,position,studyProgram) =
  setCategoryName
   (setCategoryNameE
     (setCategoryShortName
       (setCategoryComment
         (setCategoryMinECTS
           (setCategoryMaxECTS
             (setCategoryPosition
               (setCategoryStudyProgramProgramCategoriesKey categoryToUpdate
                 (studyProgramKey studyProgram))
               position)
             maxECTS)
           minECTS)
         comment)
       shortName)
     nameE)
   name

--- Transformation from entity type Category to a tuple
--- which can be used in WUI specifications.
category2Tuple
  :: StudyProgram
  -> Category -> (String,String,String,String,Int,Int,Int,StudyProgram)
category2Tuple studyProgram category =
  (categoryName category
  ,categoryNameE category
  ,categoryShortName category
  ,categoryComment category
  ,categoryMinECTS category
  ,categoryMaxECTS category
  ,categoryPosition category
  ,studyProgram)

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
  :: UserSessionInfo
  -> [StudyProgram]
  -> ((String,String,String,String,Int,Int,Int,StudyProgram) -> Controller)
  -> Controller -> [HtmlExp]
blankCategoryView sinfo possibleStudyPrograms controller cancelcontroller =
  createCategoryView sinfo "" "" "" "" 0 180 0 (head possibleStudyPrograms)
   possibleStudyPrograms
   controller
   cancelcontroller

--- Supplies a WUI form to create a new Category entity.
--- Takes default values to be prefilled in the form fields.
createCategoryView
  :: UserSessionInfo
  -> String
  -> String
  -> String
  -> String
  -> Int
  -> Int
  -> Int
  -> StudyProgram
  -> [StudyProgram]
  -> ((String,String,String,String,Int,Int,Int,StudyProgram) -> Controller)
  -> Controller -> [HtmlExp]
createCategoryView
    _
    defaultName
    defaultNameE
    defaultShortName
    defaultComment
    defaultMinECTS
    defaultMaxECTS
    defaultPosition
    defaultStudyProgram
    possibleStudyPrograms
    controller
    cancelcontroller =
  renderWuiForm (wCategory possibleStudyPrograms)
   (defaultName
   ,defaultNameE
   ,defaultShortName
   ,defaultComment
   ,defaultMinECTS
   ,defaultMaxECTS
   ,defaultPosition
   ,defaultStudyProgram)
   controller
   cancelcontroller
   "Neue Kategorie"
   "Anlegen"

--- Supplies a WUI form to edit the given Category entity.
--- Takes also associated entities and a list of possible associations
--- for every associated entity type.
editCategoryView
 :: Category -> StudyProgram -> [StudyProgram]
  -> (Bool -> Category -> Controller) -> [HtmlExp]
editCategoryView category relatedStudyProgram possibleStudyPrograms
                 controller =
  let initdata = category
      
      wuiframe = wuiEditForm "Kategorie ändern" "Ändern"
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

showCategoryInfo :: UserSessionInfo -> Category -> [HtmlExp]
showCategoryInfo sinfo cat =
  (if minpts==0 && maxpts==0 then []
   else [par [htxt $ t "Minimal ECTS points in this category" ++": "++
                     showDiv10 minpts ++
                     " / " ++
                     t "Maximal ECTS points in this category" ++": "++
                     showDiv10 maxpts]]) ++
  (if null catcmt then [] else [par [HtmlText (docText2html catcmt)]])
 where
  catcmt = categoryComment cat
  minpts = categoryMinECTS cat
  maxpts = categoryMaxECTS cat

  t = translate sinfo
  
--- Supplies a list view for a given list of Category entities.
--- Shows also buttons to show, delete, or edit entries.
--- Various controller functions to show, delete, edit, and format entities
--- as well as sending emails to request corrections are passed as arguments.
--- The second argument is the current study program or a header expression.
--- The third argument is the real data to be shown:
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
  :: UserSessionInfo
  -> (String,Int)
  -> Either StudyProgram [HtmlExp]
  -> [(Either Category String, [(ModData,[Maybe (ModInst,Int)],[Bool])])]
  -> [(String,Int)]
  -> [User]
  -> (Either StudyProgram [HtmlExp] -> [(Either Category String, [ModData])]
        -> (String,Int) -> (String,Int) -> Bool -> Bool -> Bool -> Controller)
  -> ([(String,[ModData])] -> Controller)
  -> (Either StudyProgram [HtmlExp] -> [(Either Category String, [ModData])]
        -> (String,Int) -> (String,Int) -> Controller)
  -> [HtmlExp]
listCategoryView sinfo cursem mbsprog catmods semperiod users
                 showCategoryPlanController formatCatModsController
                 showEmailCorrectionController =
  [h1 $ either (\sp -> [studyProgramToHRef sinfo sp]) id mbsprog] ++
  (if length catmods == 1 && isLeft (fst (head catmods))
   then let cat = fromLeft (fst (head catmods))
         in showCategoryInfo sinfo cat
   else []) ++
  [spTable
    (if isAdminSession sinfo && null (concatMap snd catmods)
     then map (categoryLabelList!!) [0,2,6] :
          map listCategory
              (mergeSortBy leqCategory
               (concatMap (\ (c,_) -> either (:[]) (const []) c) catmods))
     else
      concatMap
        (\ (mbcat,mods) ->
           (either (\c->[style "category"
                          (langSelect sinfo (listCategory c !! 1)
                                            (head (listCategory c)))])
                   (\s->[style "category" [htxt s]])
                   mbcat
            : if null mods
              then []
              else map (\s -> [style "category" [stringToHtml s]])
                       ("ECTS" : map showSemester semperiod)) :
           map (\ (md,mis,univs) -> modDataToCompactListView sinfo md ++
                  if null univs
                  then map (maybe [] showModInst) mis
                  else map (showUnivisInst md)
                           (zip3 semperiod mis univs))
               (mergeSortBy (\ (m1,_,_) (m2,_,_) -> leqModData m1 m2)
                  -- if a semester planning is shown, show only modules
                  -- having an instance or a UnivIS instance in the plan
                  (filter (\ (_,mis,univs) -> null semperiod || any isJust mis
                                                             || or univs)
                          mods)))
        catmods)] ++
   (if null (concatMap snd catmods)
    then either
          (\sprog ->
            [par
              [spHref ("?Category/studyprogramall/"++showStudyProgramKey sprog)
                      [htxt $ t "Show all modules in this degree program"]]])
          (const [])
          mbsprog
    else
     [par ([bold [htxt $ t "Semester planning"], htxt $ t " from ",
            spShortSelectionInitial fromsem semSelection
              (findSemesterSelection cursem
                 (if null semperiod then cursem else (head semperiod))),
            htxt $ t " to ",
            spShortSelectionInitial tosem semSelection
              (if null semperiod
                 then findSemesterSelection cursem cursem + 3
                 else findSemesterSelection cursem (last semperiod)),
            htxt ": ",
            spSmallPrimaryButton (t "Show")
                                 (showPlan False False False mbsprog)] ++
           (maybe []
                  (\_ -> [spSmallButton (t "with UnivIS comparison")
                                        (showPlan True False False mbsprog),
                          spSmallButton (t "with master program usage")
                                        (showPlan False True False mbsprog)])
                  (userLoginOfSession sinfo)) ++
           (if isAdminSession sinfo
            then [spSmallButton (t "with student numbers")
                                   (showPlan False False True mbsprog),
                  spSmallButton "UnivIS-Abgleich Emails senden"
                                (showCorrectionEmails mbsprog)]
            else []))]) ++
   (if isAdminSession sinfo
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

   t = translate sinfo

   -- show UnivIS instance of a semester
   showUnivisInst md ((term,year),mbmi,hasinst)
     | hasinst && isJust mbmi
     = [hrefUnivisInfo univisUrl [htxt "UnivIS"]
          `addTitle` (t "to UnivIS entry")]
     | hasinst
     = [hrefUnivisDanger univisUrl [htxt  "!UnivIS!"]
          `addTitle` (t "UnivIS entry without MDB entry!")]
     | isJust mbmi
     = [hrefUnivisDanger univisUrl [htxt "???"]
          `addTitle` (t "Missing UnivIS entry!")]
     | otherwise                = [nbsp]
    where univisUrl = "?UnivisInfo/showmod/"++showModDataKey md++"/"
                                            ++term++"/"++show year

   showModInst (mi,num) =
     let miuserkey = modInstUserLecturerModsKey mi
         showUser u = let name = userName u
                       in if length name > 6 then take 5 name ++ "." else name
      in [italic
           [hrefModInst ("?ModInst/show/"++showModInstKey mi)
                  [htxt (maybe "???" showUser
                               (find (\u -> userKey u == miuserkey) users)),
                   htxt (if num==0 then "" else '(':show num++")")]]]

   semSelection = map (\(s,i) -> (showSemester s,show i))
                      (zip (semesterSelection cursem) [0..])

   showPlan withunivis withmprogs withstudyplan sprog env = do
    let start = maybe 0 id (findIndex (\(_,i) -> i==(env fromsem)) semSelection)
        stop  = maybe 0 id (findIndex (\(_,i) -> i==(env tosem  )) semSelection)
    showCategoryPlanController sprog
     (map (\ (c,cmods) -> (c,map (\ (m,_,_)->m) cmods)) catmods)
     (semesterSelection cursem !! start) (semesterSelection cursem !! stop)
     withunivis withmprogs withstudyplan >>= getForm

   showCorrectionEmails sprog env = do
    let start = maybe 0 id (findIndex (\(_,i) -> i==(env fromsem)) semSelection)
        stop  = maybe 0 id (findIndex (\(_,i) -> i==(env tosem  )) semSelection)
    showEmailCorrectionController sprog
     (map (\ (c,cmods) -> (c,map (\ (m,_,_)->m) cmods)) catmods)
     (semesterSelection cursem !!start) (semesterSelection cursem !! stop)
        >>= getForm

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
 :: Either StudyProgram [HtmlExp]
  -> [(ModData,[Maybe ModInst],[Bool])]
  -> [(String,Int)] -> [User]
  -> [HtmlExp]
listEmailCorrectionView mbsprog modinsts semperiod users =
  [h1 $ either (\sp -> [htxt $ studyProgramName sp]) id mbsprog,
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

