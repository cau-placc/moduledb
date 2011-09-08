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
  [h1 [htxt "Module und Studienprogramme des Instituts f�r Informatik"],
   par [htxt $ "Auf diesen Webseiten sind die Module aller Studienprogramme "++
               "des Instituts f�r Informatik sowie alle vom Institut "++
               "angebotenen Module beschrieben."++
               "Au�erdem befindet sich hier eine �bersicht �ber alle "++
               "angebotenen Masterprogramme."],
   h2 [htxt "Studieng�nge"],
   table (map (\sp -> [head (studyProgramToListView sp)])
              (mergeSort leqStudyProgram studyPrograms)),
   h2 [href "?listMasterProgram" [htxt "Masterprogramme"]],
   par [htxt "Weitere Informationen:"],
   ulist
    [[bold [htxt "F�r Modulverantwortliche: "],
      htxt "Allgemeine ",
      ehref "edit_infos.html"
            [htxt "Hinweise zu Modulbeschreibungen und deren Bearbeitung"]],
     [ehref "?xml"
            [htxt "XML-Index aller Module des Instituts f�r Informatik"]]]]

-----------------------------------------------------------------------------
