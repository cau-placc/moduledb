----------------------------------------------------------------------------
--- This module supports the handling of user preferences in the web
--- presentation. Currently, it contains support for multi-language
--- access.
---
--- @author Michael Hanus
--- @version November 2013
----------------------------------------------------------------------------

module UserPreferences (
  Language(..), UserPrefs, preferredLanguage,
  getSessionUserPrefs, setPreferredLanguage,
  toEnglish,
  translate, langSelect, loginEmailText, loginText, mainTitle, mainExplanation,
  masterStudyNote, sendPasswordCmt, ssComment,
  timeoutText, unknownUser, useURLText
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

--- Select the item in the right language (first english, second german)
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
 [("Acknowledgment"     ,"Bestätigung")
 ,("All categories"     ,"Alle Kategorien")
 ,("All modules"        ,"Alle Module")
 ,("all modules"        ,"alle Module")
 ,("all English modules","alle englischen Module")
 ,("Back to last page"  ,"Zurück zur letzten Seite")
 ,("Cancel"             ,"Abbrechen")
 ,("Change password"    ,"Passwort ändern")
 ,("change your password","Passwort ändern")
 ,("Copy module"        ,"Modul kopieren")
 ,("Core area: "        ,"Schwerpunktbereich: ")
 ,("Core areas"         ,"Schwerpunktbereiche")
 ,("Cycle:"             ,"Turnus:")
 ,("Delete module"      ,"Modul löschen")
 ,("Duration:"          ,"Dauer:")
 ,("External URL for module","Externe URL für das Modul")
 ,("German title:"      ,"Englische Bezeichnung:") -- wegen ModDataView!!!
 ,("every year"         ,"jedes Jahr")
 ,("every year in summer term","jedes Jahr im SS")
 ,("every year in winter term","jedes Jahr im WS")
 ,("every semester"     ,"jedes Semester")
 ,("For programmers:"   ,"Für Programmierer:")
 ,("For persons in charge for modules: ","Für Modulverantwortliche: ")
 ,("Forgot your login data?","Login-Daten vergessen?")
 ,("Found modules"      ,"Gefundene Module")
 ,(" from "             ," von ")
 ,("Further information:","Weitere Informationen:")
 ,("Further modules"    ,"Weitere Module")
 ,("General "           ,"Allgemeine ")
 ,("Go to"              ,"Gehe zu")
 ,("in the module code or title","im Modulcode oder -titel")
 ,("Institute of Informatics","Institut für Informatik")
 ,("irregular"          ,"unregelmäßig")
 ,("Logged in as: "     ,"Angemeldet als: ")
 ,("Logged out"         ,"Abgemeldet")
 ,("Login"              ,"Anmelden")
 ,("Login name:"        ,"Benutzername:")
 ,("Login successful"   ,"Anmeldung erfolgreich")
 ,("Login to module database","Anmeldung zur Moduldatenbank")
 ,("Login data for module database","Moduldatenbankzugangsdaten")
 ,("Logout"             ,"Abmelden")
 ,("Main page of the module information system","Hauptseite der Moduldatenbank")
 ,("Mandatary modules"  ,"Pflichtmodule")
 ,("Master programs"    ,"Masterprogramme")
 ,("Master programs in informatics","Programme im Masterstudiengang Informatik")
 ,("Master studies in informatics:","Masterstudium Informatik:")
 ,("Module categories:" ,"Modulkategorien:")
 ,("Module code:"       ,"Modulcode:")
 ,("Module Information System","Modulinformationssystem Informatik")
 ,("Modules of"         ,"Module von")
 ,("My modules"         ,"Eigene Module")
 ,("New master program" ,"Neues Masterprogramm")
 ,("New password:"      ,"Neues Passwort:")
 ,("New passwords are different!","Neue Passwörter sind verschieden!")
 ,("not logged in"      ,"nicht angemeldet")
 ,("notes on module descriptions and their preparation",
   "Hinweise zu Modulbeschreibungen und deren Bearbeitung")
 ,("one"                ,"ein")
 ,("Old password:"      ,"Altes Passwort:")
 ,("Password:"          ,"Passwort:")
 ,("Password changed"   ,"Passwort geändert")
 ,("Person in charge:"  ,"Modulverantwortliche(r):")
 ,("Presence:"          ,"Präsenzzeiten:")
 ,("programming language","Programmiersprache")
 ,("Really logout?"     ,"Wirklich abmelden?")
 ,("Repeat new password:","Neues Passwort wiederholen:")
 ,("Search modules"     ,"Modulsuche")
 ,("Search all modules containing","Alle Module mit Zeichenfolge")
 ,("search"             ,"suchen")
 ,("semester"           ,"Semester")
 ,("Send login data"    ,"Login-Daten zusenden")
 ,("Send new password"  ,"Neues Passwort senden")
 ,("Semester planning"  ,"Semesterplanung")
 ,("Show"               ,"Anzeigen")
 ,("show"               ,"anzeigen")
 ,("Show all master programs",
   "Alle (auch ältere) Masterprogramme anzeigen")
 ,("Show all modules in this study program",
   "Alle Module in diesem Studienprogramm anzeigen")
 ,("Start: "            ,"Beginn: ")
 ,("Study planner"      ,"Studienplaner")
 ,("Study programs"     ,"Studiengänge")
 ,("Supported by:"      ,"Unterstützt durch:")
 ,(" to "               ," bis ")
 ,("Teaching language:" ,"Lehrsprache:")
 ,("two"                ,"zwei")
 ,("with master program usage","mit Masterprogrammverwendungen")
 ,("with UnivIS comparison","mit UnivIS-Abgleich")
 ,("with pattern"       ,"mit Muster")
 ,("Wrong login data!"  ,"Falsche Login-Daten!")
 ,("Wrong password!"    ,"Falsches Passwort!")
 ,("XML index to all modules","XML-Index aller Module")
 ,("XML document with all master programs","XML-Dokument aller Masterprogramme")
 ,("You can also "      ,"Sie können auch nur Ihr ")
 ,("Your email address: ","Ihre Email-Adresse: ")
 ,("Your new password has been sent","Ihr neues Passwort wurde Ihnen zugesandt")
 ]

loginText prefs loginname = langSelect prefs
  ("You are logged in as user '" ++ loginname ++
   "' and are allowed to change your modules and programs.")
  ("Sie sind als Benutzer '" ++ loginname ++
   "' angemeldet und können Ihre Module und Programme bearbeiten.")

loginEmailText prefs loginname passwd = langSelect prefs
  ("Your login data:\n\nLogin name: " ++ loginname ++
   "\nNew password: " ++ passwd ++
   "\n\nYou can use this data to login into the module database\n\n"++
   "http://www-ps.informatik.uni-kiel.de/~mh/studiengaenge/\n\n"++
   "and work on your modules and master programs.\n\n"++
   "You can change your password after the login by selecting 'Logout'\n"++
   "followed by 'Change password'.")
  ("Ihre Zugangsdaten sind:\n\nLogin-Name: " ++ loginname ++
   "\nNeues Passwort: " ++ passwd ++
   "\n\nMit diesen Daten koennen Sie sich in der Moduldatenbank\n\n"++
   "http://www-ps.informatik.uni-kiel.de/~mh/studiengaenge/\n\n"++
   "anmelden und Ihre Module und Masterprogramme aendern.\n\n"++
   "Sie koennen das Passwort aendern, indem Sie sich anmelden\n"++
   "und dann nach Auswahl von 'Abmelden' den Punkt\n"++
   "'Passwort aendern' waehlen.")

mainTitle prefs = langSelect prefs
  "Modules and study programs of the Institute of Informatics"
  "Module und Studienprogramme des Instituts für Informatik"

mainExplanation prefs = langSelect prefs
  ("This web site provides an overview on all modules and "++
   "study programs offered by the Institute of Informatics. "++
   "Additionally, it contains an overiew on all master programs "++
   "in informatics. A list of all modules offered in English "++
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
   
sendPasswordCmt prefs = langSelect prefs
  ("You can send a new password to your email address "++
   "if you are registered in the system.")
  ("Sie können sich ein neues Password an Ihre Email-Adresse " ++
   "zusenden lassen, sofern Sie im System registriert sind.")
  
ssComment prefs = langSelect prefs
  ("If the master studies are started in the summer term, "++
   "one can also choose a master program from an adjacent winter term. "++
   "Ask your academic advisor to adapt such a master program.")
  ("Bei Beginn im Sommersemester können auch Programme der "++
   "benachbarten Wintersemester gewählt werden. "++
   "Bei der Anpassung berät Sie der Academic Advisor.")

timeoutText prefs = langSelect prefs
  ("Please note that you are automatically logged out "++
   "if you are inactive for more than 60 minutes.")
  ("Bitte beachten Sie, dass Sie bei mehr als 60 Minuten "++
   "Inaktivität automatisch wieder abgemeldet werden.")

unknownUser prefs = langSelect prefs
  "There is no user with this email address!"
  "Ein Benutzer mit dieser Email-Adresse ist im System nicht bekannt!"

useURLText prefs = langSelect prefs
  "Please use the following URL to refer to this module from other web pages:"
  "Bitte verwenden Sie die folgende URL, um das Modul aus anderen Webseiten zu referenzieren:"

--------------------------------------------------------------------------
-- Auxiliaries:

spEHref :: String -> [HtmlExp] -> HtmlExp
spEHref ref hexps =
  href ref hexps `addClass` "btn btn-small"
                 `addAttr` ("target","_blank")
