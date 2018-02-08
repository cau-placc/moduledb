--------------------------------------------------------------------------
--- This module implements the views for the main page of this application.
--------------------------------------------------------------------------

module View.Main(mainPageView)
 where

import System.Spicey
import HTML.Base
import Char
import System.Helpers
import MDB
import View.MDBEntitiesToHtml
import View.StudyProgram
import Sort
import System.SessionInfo
import System.MultiLang

-----------------------------------------------------------------------------
--- A view for the main page.
mainPageView :: UserSessionInfo -> [StudyProgram] -> [HtmlExp]
mainPageView sinfo studyPrograms =
  [h1 [htxt $ mainTitle sinfo],
   par $ mainExplanation sinfo,
   par $ minorSubjectNote sinfo,
   h2 [htxt $ t "Degree programs"],
   studyProgramHtmlTable sinfo studyPrograms,
   h3 [htxt $ t "Further information:"],
   ulist
    [[htxt $ t "Master studies in computer science:", nbsp,
      spHref "?MCA/list" [htxt $ t "Core areas"], nbsp,
      spEHref "http://www-ps.informatik.uni-kiel.de/studienplaner/"
              [htxt $ t "Study planner"]
     ],
     [htxt $ t "Overview on the ",
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