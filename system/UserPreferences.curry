----------------------------------------------------------------------------
--- This module supports the handling of user preferences in the web
--- presentation. Currently, it contains support for multi-language
--- access.
---
--- @author Michael Hanus
--- @version July 2013
----------------------------------------------------------------------------

module UserPreferences (
  Language(..), UserPrefs, preferredLanguage,
  getSessionUserPrefs, setPreferredLanguage,
  toEnglish,
  translate, langSelect, mainTitle, mainExplanation, masterStudyNote, ssComment
 ) where

import Session
import Global
import HTML

--------------------------------------------------------------------------
-- Definition of user preferences.

--- The languages which are currently supported.
data Language = German | English

--- The user preferences supported by the web application.
--- Currently, these are the selection of a preferred language.
data UserPrefs = UserPrefs Language

--- The default user preferences.
defaultUserPrefs :: UserPrefs
defaultUserPrefs = UserPrefs German

-- The language of the user preferences.
preferredLanguage :: UserPrefs -> Language
preferredLanguage (UserPrefs lang) = lang

--------------------------------------------------------------------------
-- Operations for storing user preferences in the current session.

--- Definition of the session state to store the user preferences.
sessionUserPrefs :: Global (SessionStore UserPrefs)
sessionUserPrefs = global emptySessionStore (Persistent "sessionPreferences")

--- Gets the user preferences of the current session
--- (or the Nothing if there is no login).
getSessionUserPrefs :: IO UserPrefs
getSessionUserPrefs =
  getSessionData sessionUserPrefs >>= return . maybe defaultUserPrefs id

--- Stores a preferred language in the current session.
setPreferredLanguage :: Language -> IO ()
setPreferredLanguage lang = putSessionData (UserPrefs lang) sessionUserPrefs

--------------------------------------------------------------------------
--- Translates a string w.r.t. given user preferences.
--- We assume that the given string is in English.
translate :: UserPrefs -> String -> String
translate userprefs s =
  case preferredLanguage userprefs of
    English -> s
    German  -> toGerman s

--- Select the string in the right language (first english, second german)
langSelect :: UserPrefs -> a -> a -> a
langSelect userprefs es gs =
  case preferredLanguage userprefs of
    English -> es
    German  -> gs

toGerman :: String -> String
toGerman s = maybe s id (lookup s english2german)

toEnglish :: String -> String
toEnglish s = maybe s id (lookup s (map (\ (x,y) -> (y,x)) english2german))

english2german =
 [("All categories"     ,"Alle Kategorien")
 ,("All modules"        ,"Alle Module")
 ,("all modules"        ,"alle Module")
 ,("all English modules","alle englischen Module")
 ,("Core area: "        ,"Schwerpunktbereich: ")
 ,("Core areas"         ,"Schwerpunktbereiche")
 ,("Cycle:"             ,"Turnus:")
 ,("Duration:"          ,"Dauer:")
 ,("English title:"     ,"Englische Bezeichnung:")
 ,("every year"         ,"jedes Jahr")
 ,("every year in summer term","jedes Jahr im SS")
 ,("every year in winter term","jedes Jahr im WS")
 ,("every semester"     ,"jedes Semester")
 ,("For programmers:"   ,"Für Programmierer:")
 ,("For persons in charge for modules: ","Für Modulverantwortliche: ")
 ,("Found modules"      ,"Gefundene Module")
 ,(" from "             ," von ")
 ,("Further information:","Weitere Informationen:")
 ,("Further modules"    ,"Weitere Module")
 ,("General "           ,"Allgemeine ")
 ,("Go to"              ,"Gehe zu")
 ,("in the module code or title","im Modulcode oder -titel")
 ,("irregular"          ,"unregelmäßig")
 ,("Login"              ,"Anmelden")
 ,("Logout"             ,"Abmelden")
 ,("Main page of the module information system","Hauptseite der Moduldatenbank")
 ,("Mandatary modules"  ,"Pflichtmodule")
 ,("Master programs"    ,"Masterprogramme")
 ,("Master programs in informatics","Programme im Masterstudiengang Informatik")
 ,("Master studies in informatics:","Masterstudium Informatik:")
 ,("Module categories:" ,"Modulkategorien:")
 ,("Module code:"       ,"Modulcode:")
 ,("Modules of"         ,"Module von")
 ,("My modules"         ,"Eigene Module")
 ,("New master program" ,"Neues Masterprogramm")
 ,("not logged in"      ,"nicht angemeldet")
 ,("notes on module descriptions and their preparation",
   "Hinweise zu Modulbeschreibungen und deren Bearbeitung")
 ,("one"                ,"ein")
 ,("Person in charge:"  ,"Modulverantwortliche(r):")
 ,("Presence:"          ,"Präsenzzeiten:")
 ,("programming language","Programmiersprache")
 ,("Search modules"     ,"Modulsuche")
 ,("Search all modules containing","Alle Module mit Zeichenfolge")
 ,("semester"           ,"Semester")
 ,("Semester planning"  ,"Semesterplanung")
 ,("Show"               ,"Anzeigen")
 ,("show"               ,"anzeigen")
 ,("Show all master programs",
   "Alle (auch ältere) Masterprogramme anzeigen")
 ,("Show all modules in this study program",
   "Alle Module in diesem Studienprogramm anzeigen")
 ,("Start: "            ,"Beginn: ")
 ,("Study programs"     ,"Studiengänge")
 ,("suchen"             ,"search")
 ,("Supported by:"      ,"Unterstützt durch:")
 ,(" to "               ," bis ")
 ,("Teaching language:" ,"Lehrsprache:")
 ,("two"                ,"zwei")
 ,("with master program usage","mit Masterprogrammverwendungen")
 ,("with UnivIS comparison","mit UnivIS-Abgleich")
 ,("with pattern"       ,"mit Muster")
 ,("XML index to all modules","XML-Index aller Module")
 ,("XML document with all master programs","XML-Dokument aller Masterprogramme")
 ]

mainTitle prefs = langSelect prefs
  "Modules and study programs of the Institut für Informatik"
  "Module und Studienprogramme des Instituts für Informatik"

mainExplanation prefs = langSelect prefs
  ("This web site provides an overview on all modules and "++
   "study programs offered by the Institut für Informatik. "++
   "Additionally, it contains an overiew on all master programs "++
   "in computer science. A list of all modules offered in English "++
   "can be found in the category \"Search modules\".")
  ("Auf diesen Webseiten sind die Module aller Studienprogramme "++
   "des Instituts für Informatik sowie alle vom Institut "++
   "angebotenen Module beschrieben. "++
   "Außerdem befindet sich hier eine Übersicht über alle "++
   "angebotenen Masterprogramme.")

masterStudyNote prefs = langSelect prefs
  [italic [htxt "Important note: "],
   htxt "All master students should plan their individual studies with the ",
   spEHref "http://www-ps.informatik.uni-kiel.de/studienplaner/"
           [bold [htxt "study planner"]], htxt "!"]
  [italic [htxt "Wichtiger Hinweis: "],
   htxt "Alle Masterstudierenden sollten ihre individualle Planung mit dem ",
   spEHref "http://www-ps.informatik.uni-kiel.de/studienplaner/"
           [bold [htxt "Studienplaner"]], htxt " durchführen! ",
   htxt "Damit wird weitgehend gewährleistet, dass das geplante Studium ",
   htxt "auch wirklich durchführbar ist."]
   
  
ssComment prefs = langSelect prefs
  ("If the master studies are started in the summer term, "++
   "one can also choose a master program from an adjacent winter term. "++
   "Ask your academic advisor to adapt such a master program.")
  ("Bei Beginn im Sommersemester können auch Programme der "++
   "benachbarten Wintersemester gewählt werden. "++
   "Bei der Anpassung berät Sie der Academic Advisor.")

--------------------------------------------------------------------------
-- Auxiliaries:

spEHref :: String -> [HtmlExp] -> HtmlExp
spEHref ref hexps =
  href ref hexps `addClass` "btn btn-small"
                 `addAttr` ("target","_blank")
