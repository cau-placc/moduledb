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
               "angebotenen Module beschrieben."++
               "Außerdem befindet sich hier eine Übersicht über alle "++
               "angebotenen Masterprogramme."],
   h2 [htxt "Studiengänge"],
   table (map (\sp -> [head (studyProgramToListView sp)])
              (mergeSort leqStudyProgram studyPrograms)),
   h2 [htxt "Masterstudium Informatik:", nbsp,
   style "buttonhref" [href "?listMasterCoreArea"
                                 [htxt "Schwerpunktbereiche"]],nbsp,
        style "buttonhref" [href "?listMasterProgram" [htxt "Masterprogramme"]]],
   par [htxt "Weitere Informationen:"],
   ulist
    [[bold [htxt "Für Modulverantwortliche: "],
      htxt "Allgemeine ",
      ehref "edit_infos.html"
            [htxt "Hinweise zu Modulbeschreibungen und deren Bearbeitung"]],
     [bold [htxt "Für Programmierer: "],
      ehref "?xml"
            [htxt "XML-Index aller Module des Instituts für Informatik"]]]]

-----------------------------------------------------------------------------
