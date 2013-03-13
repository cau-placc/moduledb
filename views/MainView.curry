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

-----------------------------------------------------------------------------
--- A view for the main page.
mainPageView :: [StudyProgram] -> [HtmlExp]
mainPageView studyPrograms =
  [h1 [htxt "Module und Studienprogramme des Instituts für Informatik"],
   par [htxt $ "Auf diesen Webseiten sind die Module aller Studienprogramme "++
               "des Instituts für Informatik sowie alle vom Institut "++
               "angebotenen Module beschrieben. "++
               "Außerdem befindet sich hier eine Übersicht über alle "++
               "angebotenen Masterprogramme."],
   h2 [htxt "Studiengänge"],
   spTable (map (\sp -> [head (studyProgramToListView sp)])
                (mergeSort leqStudyProgram studyPrograms)),
   h2 [htxt "Masterstudium Informatik:", nbsp,
       spHref "?listMasterCoreArea" [htxt "Schwerpunktbereiche"], nbsp,
       spHref "?listMasterProgram" [htxt "Masterprogramme"]],
   par [htxt "Weitere Informationen:"],
   ulist
    [[bold [htxt "Für Modulverantwortliche: "],
      htxt "Allgemeine ",
      ehref "edit_infos.html"
            [htxt "Hinweise zu Modulbeschreibungen und deren Bearbeitung"]],
     [bold [htxt "Für Programmierer: "],
      ehref "?xml" [htxt "XML-Index aller Module"],
      htxt " | ",
      ehref "?xmlprog=all" [htxt "XML-Dokument aller Masterprogramme"]]]]

-----------------------------------------------------------------------------
