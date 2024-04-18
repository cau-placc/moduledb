module View.Category
 ( wCategory, tuple2Category, category2Tuple, wCategoryType
 , showCategoryView, listCategoryView
 , selSemesterPlanningView
 , leqCategory, showCategoryInfo, listEmailCorrectionView
 ) where

import Data.List
import Data.Either
import Data.Maybe ( isJust )
import Data.Time

import HTML.Base
import HTML.Styles.Bootstrap4
import HTML.WUI

import System.Spicey
import Model.MDB
import View.MDBEntitiesToHtml
import System.Helpers
import View.ModData
import Model.ConfigMDB
import View.UnivisInfo
import System.Mail
import System.Process (sleep)
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

  numwidthRendering = inline . map (`addClass` "numwidth")

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

------------------------------------------------------------------------------
--- Supplies a view to show the details of a Category.
showCategoryView :: Category -> StudyProgram -> [BaseHtml]
showCategoryView category relatedStudyProgram =
  categoryToDetailsView category relatedStudyProgram ++
   [hrefPrimSmButton "?Category/list" [htxt "back to Category list"]]

--- Compares two Category entities. This order is used in the list view.
leqCategory :: Category -> Category -> Bool
leqCategory x1 x2 =
  categoryPosition x1 <= categoryPosition x2

showCategoryInfo :: UserSessionInfo -> Category -> [BaseHtml]
showCategoryInfo sinfo cat =
  (if minpts==0 && maxpts==0 then []
   else [par [htxt $ t "Minimal ECTS points in this category" ++": "++
                     showDiv10 minpts ++
                     " / " ++
                     t "Maximal ECTS points in this category" ++": "++
                     showDiv10 maxpts]]) ++
  (if null catcmt then [] else [par [htmlText (docText2html catcmt)]])
 where
  catcmt = categoryComment cat
  minpts = categoryMinECTS cat
  maxpts = categoryMaxECTS cat

  t = translate sinfo
  
--- Supplies a list view for a given list of Category entities.
--- Shows also buttons to show, delete, or edit entries.
--- The second argument is the current study program or a header expression.
--- The third argument contains the real data to be shown:
--- a list of category information where each info contains
--- a category (or a header string) together with a list of triples containing
--- the modules in this category. Each triple contains:
--- * the module data
--- * a list of possible module instances for the semester period
--    (passed as the fourth argument)
--- * a list of Booleans indicating whether the corresponding module instance
---   has a UnivIS entry (if this list is empty, UnivIS entries should not
---   be shown).
--- The further arguments are the current semester,
--- the semesters in the selected period,
--- the list of all users (to show them in the module instances), and
--- the HTML expression of the form to select a semester period to show
--- the module instances in this period.
listCategoryView
  :: UserSessionInfo -> Either StudyProgram [BaseHtml]
  -> [(Either Category String, [(ModData,[Maybe (ModInst,Int)],[Bool])])]
  -> (String,Int) -> [(String,Int)] -> [User] -> BaseHtml
  -> [BaseHtml]
listCategoryView sinfo mbsprog catmods cursem semperiod users semselectform =
  [h1 $ either (\sp -> [studyProgramToHRef sinfo sp]) id mbsprog] ++
  (if length catmods == 1 && isLeft (fst (head catmods))
   then let cat = fromLeft (fst (head catmods))
         in showCategoryInfo sinfo cat
   else []) ++
  [spTable
    (if isAdminSession sinfo && null (concatMap snd catmods)
     then map (categoryLabelList!!) [0,2,6] :
          map listCategory
              (sortBy leqCategory
               (concatMap (\ (c,_) -> either (:[]) (const []) c) catmods))
     else
      concatMap
        (\ (mbcat,mods) ->
           (either (\c -> if null semperiod
                            then
                              [let (showref,cname) = categoryToShowHRef sinfo c
                               in style "category"
                                        [href showref [stringToHtml cname]
                                    `addClass` "btn btn-primary"], nbsp,
                               style "category"
                                     [href (semPlanURL $ showCategoryKey c)
                                           [stringToHtml showSemPlanTitle]
                                  `addClass` "btn btn-info"]]
                            else [style "category" (head (listCategory c))])
                   (\s -> [style "category" [htxt s]])
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
               (sortBy (\ (m1,_,_) (m2,_,_) -> leqModData m1 m2)
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
               [ hrefPrimBadge
                   ("?Category/studyprogramall/" ++ showStudyProgramKey sprog)
                   [htxt $ t "Show all modules in this degree program"]
               , nbsp
               , hrefPrimBadge
                   ("?StudyProgram/prereqs/" ++ showStudyProgramKey sprog)
                   [htxt $ t "Show all module dependencies"]
               ]])
           (const [])
           mbsprog
     else [par [semselectform]])
  where
   t = translate sinfo

   fromSemPlan      = nextSemester cursem
   toSemPlan        = last (filter isValidSemester
                                   (take 4 (iterate nextSemester fromSemPlan)))
   showSemPlanTitle = t "Planning" ++ " " ++ showSemester fromSemPlan ++
                      " - " ++ showSemester toSemPlan
   semPlanURL ckey  = "?Category/showplan/" ++ showSemesterCode fromSemPlan ++
                      "/" ++ showSemesterCode toSemPlan ++ "/" ++ ckey

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
    where univisUrl = "?UnivisInfo/showmod/" ++ showModDataKey md ++ "/"
                                             ++ term ++ "/" ++ show year

   showModInst (mi,num) =
     let miuserkey = modInstUserLecturerModsKey mi
         showUser u = let name = userName u
                       in if length name > 6 then take 5 name ++ "." else name
      in [italic
           [hrefModInst ("?ModInst/show/"++showModInstKey mi)
                  [htxt (maybe "???" showUser
                               (find (\u -> userKey u == miuserkey) users)),
                   htxt (if num==0 then "" else '(':show num++")")]]]

   listCategory category =
     categoryToListView sinfo category ++
     [[hrefPrimBadge ("?Category/show/"  ++ showCategoryKey category)
                     [htxt "Anzeigen"]]
     ,[hrefPrimBadge ("?Category/edit/"  ++ showCategoryKey category)
                     [htxt "Ändern"]]
     ,[hrefPrimBadge ("?Category/delete/"++ showCategoryKey category)
                     [htxt "Löschen"]]
     ]

--- A view for a form to select a semester period to show the module instances
--- in this period.
--- Various controller functions to show plans, format entities
--- as well as sending emails to request corrections are passed as arguments.
selSemesterPlanningView ::
     (Maybe User -> Either StudyProgram [BaseHtml]
      -> [(Either Category String, [ModData])]
      -> (String,Int) -> (String,Int) -> Bool -> Bool -> Bool -> Controller)
  -> (Either StudyProgram [BaseHtml] -> [(Either Category String, [ModData])]
        -> (String,Int) -> (String,Int) -> Controller)
  -> ([(String,[ModData])] -> Controller)
  -> ( UserSessionInfo
     , Maybe User
     , Either StudyProgram [BaseHtml]
     , [(Either Category String, [ModData])]
     , (String,Int)
     , [(String,Int)])
  -> [HtmlExp]
selSemesterPlanningView showCategoryPlanController showEmailCorrectionController
  formatCatModsController (sinfo, mbuser, mbsprog, catmods, cursem, semperiod) =
  [par ([bold [htxt $ t "Semester planning"], htxt $ t " from ",
               spShortSelectionInitial fromsem semSelection
                 (findSemesterSelection cursem
                    (if null semperiod then cursem else (head semperiod))),
               htxt $ t " to ",
               spShortSelectionInitial tosem semSelection
                 (if null semperiod
                    then min (findSemesterSelection cursem cursem + 3)
                             (length semSelection - 1)
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
        else []))] ++
  (if isAdminSession sinfo
     then [par [spSmallButton "Alle Module formatieren"
                  (nextController
                     (formatCatModsController
                        (map (\(cat,mods) -> (either categoryName id cat, mods))
                             catmods))),
                spSmallButton "Alle sichtbaren Module formatieren"
                  (nextController
                     (formatCatModsController
                        (map (\ (cat,mods) -> (either categoryName id cat,
                                               filter modDataVisible mods))
                             catmods)))]]
     else [])
 where
  fromsem,tosem free

  t = translate sinfo

  semSelection = map (\ (s,i) -> (showSemester s,show i))
                     (zip (semesterSelection cursem) [0..])

  showPlan withunivis withmprogs withstudyplan sprog env = do
    let start = maybe 0 id (findIndex (\(_,i) -> i==(env fromsem)) semSelection)
        stop  = maybe 0 id (findIndex (\(_,i) -> i==(env tosem  )) semSelection)
    showCategoryPlanController mbuser sprog catmods
     (semesterSelection cursem !! start) (semesterSelection cursem !! stop)
     withunivis withmprogs withstudyplan >>= getPage

  showCorrectionEmails sprog env = do
    let start = maybe 0 id (findIndex (\(_,i) -> i==(env fromsem)) semSelection)
        stop  = maybe 0 id (findIndex (\(_,i) -> i==(env tosem  )) semSelection)
    showEmailCorrectionController sprog catmods
     (semesterSelection cursem !! start) (semesterSelection cursem !! stop)
        >>= getPage

--- Supplies a form view for a given list of module instances combined
--- with their UnivIS occurrences and a button to send mails to the
--- responsible person to correct their entries.
listEmailCorrectionView
  :: [(ModData,[Maybe ModInst],[Bool])] -> [(String,Int)] -> [User] -> [HtmlExp]
listEmailCorrectionView modinsts semperiod users =
  [spTable (map showUnivisInst problemmods),
   spPrimButton "UnivIS-Korrektur-Emails jetzt an alle versenden" sendMails
  ]
  where
   -- show UnivIS instance of a semester
   showUnivisInst (md,sem,reason) =
     [[htxt (modDataCode md)],
      [htxt (showSemester sem)],
      [htxt (if reason=="NOMDB" then "Kein Eintrag in MDB"
                                else "Kein Eintrag in UnivIS")],
      [htxt ("TO: "++ getResponsibleEmail md)]]

   sendMails _ = do
     mailresults <- mapM sendSingleMail problemmods
     getPage ([h1 [htxt "Mails gesendet!"]] ++
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
        | hasinst && isJust mbmi = []
        | hasinst                = [(md,sem,"NOMDB")]
        | isJust mbmi            = [(md,sem,"NOUNIVIS")]
        | otherwise              = []

   getResponsibleEmail md =
     let mduserkey = modDataUserResponsibleKey md
      in maybe adminEmail userEmail (find (\u -> userKey u == mduserkey) users)

------------------------------------------------------------------------------
