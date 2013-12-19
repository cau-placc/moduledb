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
import UserPreferences

-----------------------------------------------------------------------------
--- A view for the main page.
mainPageView :: UserPrefs -> [StudyProgram] -> [HtmlExp]
mainPageView prefs studyPrograms =
  [h1 [htxt $ mainTitle prefs],
   par [htxt $ mainExplanation prefs],
   h2 [htxt $ t "Study programs"],
   spTable (map (\sp -> [head (studyProgramToListView sp)])
                (mergeSort leqStudyProgram studyPrograms)),
   h2 [htxt $ t "Master studies in informatics:"],
   par [spHref "?mca/list" [htxt $ t "Core areas"], nbsp,
        spHref "?listMasterProgram" [htxt $ t "Master programs"], nbsp,
        spEHref "http://www-ps.informatik.uni-kiel.de/studienplaner/"
                [htxt $ t "Study planner"]],
   h3 [htxt $ t "Further information:"],
   ulist
    [[bold [htxt $ t "For persons in charge for modules: "],
      htxt $ t "General ",
      ehref "edit_infos.html"
            [htxt $ t "notes on module descriptions and their preparation"]],
     [bold [htxt $ t "For programmers:"], nbsp,
      ehref "?xml" [htxt $ t "XML index to all modules"],
      htxt " | ",
      ehref "?xmlprog=all" [htxt $ t "XML document with all master programs"]]]]
 where
  t = translate prefs

-----------------------------------------------------------------------------
