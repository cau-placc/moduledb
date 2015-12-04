--------------------------------------------------------------------------
--- This module implements the views for the main page of this application.
--------------------------------------------------------------------------

module MainView(mainPageView)
 where

import Spicey
import HTML
import Char
import Helpers
import MDB
import MDBEntitiesToHtml
import StudyProgramView
import Sort
import SessionInfo
import MultiLang

-----------------------------------------------------------------------------
--- A view for the main page.
mainPageView :: UserSessionInfo -> [StudyProgram] -> [HtmlExp]
mainPageView sinfo studyPrograms =
  [h1 [htxt $ mainTitle sinfo],
   par [htxt $ mainExplanation sinfo],
   par $ minorSubjectNote sinfo,
   h2 [htxt $ t "Study programs"],
   spTable (map (\sp -> [langSelect sinfo
                                    (studyProgramToListView sp !! 1)
                                    (head (studyProgramToListView sp))])
                (mergeSort leqStudyProgram studyPrograms)),
   h2 [htxt $ t "Master studies in computer science:"],
   par [spHref "?MCA/list" [htxt $ t "Core areas"], nbsp,
        spHref "?AdvisorStudyProgram/list" [htxt $ t "Master programs"],
        spHref "?MasterProgram/list" [htxt $ t "Master programs (until SS15)"],
        nbsp,
        spEHref "http://www-ps.informatik.uni-kiel.de/studienplaner/"
                [htxt $ t "Study planner"]],
   h3 [htxt $ t "Further information:"],
   ulist
    [[htxt $ t "Overview on the ",
      ehref "lehrplanung.html"
            [htxt $ t "planning instruments of the institute"]],
     [bold [htxt $ t "For persons in charge for modules: "],
      htxt $ t "General ",
      ehref "edit_infos.html"
            [htxt $ t "notes on module descriptions and their preparation"]],
     [bold [htxt $ t "For programmers:"], nbsp,
      ehref "?xml" [htxt $ t "XML index to all modules"],
      htxt " | ",
      ehref "?xmlaprog=all" [htxt $ t "XML document with all master programs"],
      htxt " | ",
      ehref "?xmlprog=all" [htxt $ t "XML document with all master programs (until SS15)"]]]]
 where
  t = translate sinfo

-----------------------------------------------------------------------------
