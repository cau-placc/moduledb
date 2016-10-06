module AdvisorStudyProgramView
  ( wAdvisorStudyProgram, tuple2AdvisorStudyProgram, advisorStudyProgram2Tuple
  , wAdvisorStudyProgramType, blankAdvisorStudyProgramView
  , createAdvisorStudyProgramView, editAdvisorStudyProgramView
  , showAdvisorStudyProgramView, listAdvisorStudyProgramView ) where

import WUI
import Helpers
import HTML
import List
import Time
import Sort
import Spicey
import CategoryView
import SessionInfo
import MDB
import MDBEntitiesToHtml
import MultiLang

--- The WUI specification for the entity type AdvisorStudyProgram.
--- It also includes fields for associated entities.
wAdvisorStudyProgram
  :: Bool -> UserSessionInfo -> [StudyProgram]
  -> [User]
  -> WuiSpec (String,String,Int,String,String,String,Bool,StudyProgram,User)
wAdvisorStudyProgram isnewprog sinfo studyProgramList userList =
  withRendering
   (w9Tuple wLargeRequiredString wTrm wYr wStr wStr wStr
     wVisible
     wStudyProg
     wAdvisor)
   (renderWithLabels advisorStudyProgramLabelList)
 where
  -- render with labels but put row 7 (StudyProgram) at the top:
  renderWithLabels labels hexps =
    let rows = map (\(l, h) -> [l, [h]]) (zip labels hexps)
     in spTable ([rows!!7] ++ take 7 rows ++ drop 8 rows)

  admin = isAdminSession sinfo
  
  wTrm = if isnewprog || admin then wTerm        else wConstant stringToHtml
  wYr  = if isnewprog || admin then wCurrentYear else wConstant intToHtml

  wStudyProg = if isnewprog then wSelect studyProgramName studyProgramList
                            else wConstant (stringToHtml . studyProgramName)
                            
  wAdvisor = if admin then wSelect userToShortView userList
                      else wConstant (stringToHtml . userToShortView)

  wStr = wTextArea (6,70) `withRendering` renderWithFormControl

--- Transformation from data of a WUI form to entity type AdvisorStudyProgram.
tuple2AdvisorStudyProgram
  :: AdvisorStudyProgram
  -> (String,String,Int,String,String,String,Bool,StudyProgram,User)
  -> AdvisorStudyProgram
tuple2AdvisorStudyProgram
    advisorStudyProgramToUpdate
    (name,term,year,desc,prereq,comments,visible,studyProgram,user) =
  setAdvisorStudyProgramName
   (setAdvisorStudyProgramTerm
     (setAdvisorStudyProgramYear
       (setAdvisorStudyProgramDesc
         (setAdvisorStudyProgramPrereq
           (setAdvisorStudyProgramComments
             (setAdvisorStudyProgramVisible
               (setAdvisorStudyProgramUserStudyAdvisingKey
                 (setAdvisorStudyProgramStudyProgramStudyProgramsAdvisedKey
                   advisorStudyProgramToUpdate
                   (studyProgramKey studyProgram))
                 (userKey user))
               visible)
             comments)
           prereq)
         desc)
       year)
     term)
   name

--- Transformation from entity type AdvisorStudyProgram to a tuple
--- which can be used in WUI specifications.
advisorStudyProgram2Tuple
  :: StudyProgram
  -> User
  -> AdvisorStudyProgram
  -> (String,String,Int,String,String,String,Bool,StudyProgram,User)
advisorStudyProgram2Tuple studyProgram user advisorStudyProgram =
  (advisorStudyProgramName advisorStudyProgram
  ,advisorStudyProgramTerm advisorStudyProgram
  ,advisorStudyProgramYear advisorStudyProgram
  ,advisorStudyProgramDesc advisorStudyProgram
  ,advisorStudyProgramPrereq advisorStudyProgram
  ,advisorStudyProgramComments advisorStudyProgram
  ,advisorStudyProgramVisible advisorStudyProgram
  ,studyProgram
  ,user)

--- WUI Type for editing or creating AdvisorStudyProgram entities.
--- Includes fields for associated entities.
wAdvisorStudyProgramType
  :: UserSessionInfo -> AdvisorStudyProgram
  -> StudyProgram
  -> User -> [StudyProgram] -> [User] -> WuiSpec AdvisorStudyProgram
wAdvisorStudyProgramType sinfo
    advisorStudyProgram studyProgram user studyProgramList userList =
  transformWSpec
   (tuple2AdvisorStudyProgram advisorStudyProgram
   ,advisorStudyProgram2Tuple studyProgram user)
   (wAdvisorStudyProgram False sinfo studyProgramList userList)

--- Supplies a WUI form to create a new AdvisorStudyProgram entity.
--- The fields of the entity have some default values.
blankAdvisorStudyProgramView
  :: UserSessionInfo
  -> [StudyProgram]
  -> User -> [User]
  -> ((String,String,Int,String,String,String,Bool,StudyProgram,User)
  -> Controller)
  -> Controller -> [HtmlExp]
blankAdvisorStudyProgramView
    sinfo possibleStudyPrograms user possibleUsers controller cancelcontroller =
  createAdvisorStudyProgramView sinfo "" "" 2015 "" "" "" False
   defaultStudyProgram
   user
   possibleStudyPrograms
   possibleUsers
   controller
   cancelcontroller
 where
  defaultStudyProgram =
    maybe (head possibleStudyPrograms)
          id
          -- Select MSc Inf study program as the default program:
          (find (\sp -> studyProgramProgKey sp == "MScI15") possibleStudyPrograms)
          
--- Supplies a WUI form to create a new AdvisorStudyProgram entity.
--- Takes default values to be prefilled in the form fields.
createAdvisorStudyProgramView
  :: UserSessionInfo
  -> String
  -> String
  -> Int
  -> String
  -> String
  -> String
  -> Bool
  -> StudyProgram
  -> User
  -> [StudyProgram]
  -> [User]
  -> ((String,String,Int,String,String,String,Bool,StudyProgram,User)
  -> Controller)
  -> Controller -> [HtmlExp]
createAdvisorStudyProgramView
    sinfo
    defaultName
    defaultTerm
    defaultYear
    defaultDesc
    defaultPrereq
    defaultComments
    defaultVisible
    defaultStudyProgram
    defaultUser
    possibleStudyPrograms
    possibleUsers
    controller
    cancelcontroller =
  renderWuiForm
   (wAdvisorStudyProgram True sinfo possibleStudyPrograms possibleUsers)
   (defaultName
   ,defaultTerm
   ,defaultYear
   ,defaultDesc
   ,defaultPrereq
   ,defaultComments
   ,defaultVisible
   ,defaultStudyProgram
   ,defaultUser)
   controller
   cancelcontroller
   "Neues Studienprogramm"
   "Studienprogramm anlegen"

--- Supplies a WUI form to edit the given AdvisorStudyProgram entity.
--- Takes also associated entities and a list of possible associations
--- for every associated entity type.
editAdvisorStudyProgramView
  :: UserSessionInfo
  -> AdvisorStudyProgram
  -> StudyProgram
  -> User
  -> [StudyProgram]
  -> [User] -> (AdvisorStudyProgram -> Controller) -> Controller -> [HtmlExp]
editAdvisorStudyProgramView
    sinfo
    advisorStudyProgram
    relatedStudyProgram
    relatedUser
    possibleStudyPrograms
    possibleUsers
    controller
    cancelcontroller =
  renderWuiForm
   (wAdvisorStudyProgramType sinfo advisorStudyProgram relatedStudyProgram
     relatedUser
     possibleStudyPrograms
     possibleUsers)
   advisorStudyProgram
   controller
   cancelcontroller
   "Studienprogramm ändern"
   "Ändern"

--- Supplies a view to show the details of a AdvisorStudyProgram.
showAdvisorStudyProgramView
  :: UserSessionInfo -> Bool -> Bool
  -> (AdvisorStudyProgram -> Controller)
  -> Controller
  -> (Category -> Controller)
  -> (AdvisorModule -> Controller)
  -> AdvisorStudyProgram -> String -> StudyProgram
  -> [(AdvisorModule,ModInst,ModData)] -> [Category] -> User
  -> [HtmlExp]
showAdvisorStudyProgramView
    sinfo admin editallowed editcontroller _ {-showcontroller-}
    addcatmodcontroller delmodcontroller
    asprog xmlurl relatedsprog amdatas cats advisor =
  [h1 [htxt (advisorStudyProgramName asprog)
      ,ehref xmlurl [imageNB "images/xml.png" "XML representation"]]] ++
  (if advisorStudyProgramVisible asprog then []
     else [h4 [htxt "(nicht öffentlich sichtbar)"]]) ++
  [h2 [htxt (t "Study program" ++ ": "),
       studyProgramToHRef sinfo relatedsprog]] ++
  [h3 [htxt $ t "Start: " ++ showSemester (startSem,startYear) ++
              " / Research advisor: ", userToHtmlView advisor]] ++
  [par $ (if admin || editallowed
          then [spButton "Beschreibung/Sichtbarkeit ändern"
                       (nextController (editcontroller asprog))]
          else []) ++
         (if admin then [spHref ("?AdvisorStudyProgram/delete/"
                                 ++ showAdvisorStudyProgramKey asprog)
                                 [htxt "Studienprogramm löschen"]]
                   else [])] ++
  [h4 [htxt $ t "Description"++":"],
   par [HtmlText (docText2html (advisorStudyProgramDesc asprog))],
   h4 [htxt $ t "Prerequisites"++":"],
   par [HtmlText (docText2html (advisorStudyProgramPrereq asprog))],
   h4 [htxt $ t "Comments"++":"],
   par [HtmlText (docText2html (advisorStudyProgramComments asprog))],
   h3 [htxt $ t "Program overview by categories:"],
   spTable $
     concatMap
       (\c -> [[[catRef c],
                (if admin || editallowed
                  then [spSmallButton "Modulempfehlung hinzufügen"
                                      (nextController (addcatmodcontroller c))]
                  else [])],
               [showCategoryInfo sinfo c]] ++
            let camods = filter (isAdvisorModuleOfCat c) amdatas
             in if null camods then []
                else (map showAdvisorModuleData camods))
       cats]
 where
   t = translate sinfo
   
   startSem  = advisorStudyProgramTerm asprog
   startYear = advisorStudyProgramYear asprog

   catRef c = hrefCategory ("?Category/show/"++showCategoryKey c)
                    [htxt $ (langSelect sinfo categoryNameE categoryName) c]
                       
   isAdvisorModuleOfCat cat (am,_,_) =
     advisorModuleCategoryAdvisorCategorizingKey am == categoryKey cat

   showAdvisorModuleData (am,modinst,md) =
    [[(if mandatory then bold else italic) [modtitle],
      htxt $ " (" ++ showDiv10 (modDataECTS md) ++ " ECTS, "
                 ++ showSemester (modInstSemester modinst) ++ ", " ++
                 (if mandatory then t "mandatory" else t "recommended")
                 ++ ")"],
     if admin || editallowed
       then [spSmallButton "Modulempfehlung löschen"
                           (nextController (delmodcontroller am))]
       else []]
    where
      mandatory = advisorModuleMandatory am
      modtitle = ehref ("?ModData/show/"++showModDataKey md)
                       [htxt $ modDataCode md ++": "++ modDataNameG md]


--- Compares two AdvisorStudyProgram entities. This order is used in the list view.
leqAdvisorStudyProgram
  :: (AdvisorStudyProgram,StudyProgram)
  -> (AdvisorStudyProgram,StudyProgram) -> Bool
leqAdvisorStudyProgram (x1,sp1) (x2,sp2) =
  (advisorStudyProgramYear x2
  ,advisorStudyProgramTerm x2
  ,studyProgramPosition sp1
  ,advisorStudyProgramName x1)
   <= (advisorStudyProgramYear x1
      ,advisorStudyProgramTerm x1
      ,studyProgramPosition sp2
      ,advisorStudyProgramName x2)

--- Supplies a list view for a given list of AdvisorStudyProgram entities.
--- Shows also show/edit/delete buttons if the user is logged in.
--- The arguments are the session info and the list of AdvisorStudyProgram entities.
listAdvisorStudyProgramView
  :: UserSessionInfo -> [(AdvisorStudyProgram,StudyProgram)] -> [HtmlExp]
listAdvisorStudyProgramView sinfo advisorStudyPrograms =
  [h1 [htxt $ t "Study programs at the department of computer science"]] ++
  masterStudyOldNote sinfo ++
  [hrule] ++
  if null advisorprogramgroups then [] else
   concatMap (\ag -> [h2 [htxt $ t "Start: " ++
                          let asp = fst (head ag) in
                          showLongSemester (advisorStudyProgramTerm asp,
                                            advisorStudyProgramYear asp)],
                      ulist (map listAdvisorStudyProgram ag), hrule])
       advisorprogramgroups
  where
   advisorprogramgroups =
     groupBy sameSemester
             (mergeSortBy leqAdvisorStudyProgram advisorStudyPrograms)

   sameSemester (p1,_) (p2,_) =
        advisorStudyProgramYear p1 == advisorStudyProgramYear p2
     && advisorStudyProgramTerm p1 == advisorStudyProgramTerm p2
                     
   listAdvisorStudyProgram (asprog,sprog) =
    [(if advisorStudyProgramVisible asprog then bold else italic)
      [href ("?AdvisorStudyProgram/show/" ++ showAdvisorStudyProgramKey asprog)
            [htxt (advisorStudyProgramName asprog)]
      ,htxt $ " (" ++ t "Study program" ++ ": "
      ,studyProgramToHRef sinfo sprog
      ,htxt ")"]
    ]

   t = translate sinfo
