----------------------------------------------------------------------------
--- This module contains support for multi-language access.
---
--- @author Michael Hanus
--- @version January 2014
----------------------------------------------------------------------------

module System.MultiLang (
  toEnglish,
  translate, langSelect,
  loginEmailText, loginText, mainTitle, mainExplanation,
  masterStudyNote, masterStudyOldNote, minorSubjectNote,
  sendPasswordCmt, ssComment,
  timeoutText, unknownUser, useURLText
 ) where

import System.SessionInfo
import HTML.Base
import ConfigMDB(baseURL,studyPlannerURL)

--------------------------------------------------------------------------
--- Translates a string w.r.t. given user session info.
--- We assume that the given string is in English.
translate :: UserSessionInfo -> String -> String
translate sinfo s =
  case languageOfSession sinfo of
    English -> s
    German  -> toGerman s

--- Select the item in the right language (first english, second german)
langSelect :: UserSessionInfo -> a -> a -> a
langSelect sinfo es gs =
  case languageOfSession sinfo of
    English -> es
    German  -> gs

toGerman :: String -> String
toGerman s = maybe s id (lookup s english2german)

toEnglish :: String -> String
toEnglish s = maybe s id (lookup s (map (\ (x,y) -> (y,x)) english2german))

english2german :: [(String, String)]
english2german =
 [("Acknowledgment"     ,"Bestätigung")
 ,("Add"                ,"Hinzufügen")
 ,("Add semester"       ,"Semester hinzufügen")
 ,("Add prerequisite"   ,"Voraussetzung hinzufügen")
 ,("Advisor programs"   ,"Studienprogramme")
 ,("All categories"     ,"Alle Kategorien")
 ,("All modules"        ,"Alle Module")
 ,("all modules"        ,"alle Module")
 ,("All modules of a person","Alle Module einer Person")
 ,("All my modules"     ,"Alle eigenen Module")
 ,("All English modules","Alle englischen Module")
 ,("Back to last page"  ,"Zurück zur letzten Seite")
 ,("Cancel"             ,"Abbrechen")
 ,("Change module data" ,"Modulbasisdaten ändern")
 ,("Change module description","Modulbeschreibung ändern")
 ,("Change password"    ,"Passwort ändern")
 ,("Change semesters"   ,"Semesterangaben ändern")
 ,("change your password","Passwort ändern")
 ,("Comments"           ,"Kommentare")
 ,("Copy module"        ,"Modul kopieren")
 ,("Core area: "        ,"Schwerpunktbereich: ")
 ,("Core areas"         ,"Schwerpunktbereiche")
 ,("Cycle:"             ,"Turnus:")
 ,("Degree program"      ,"Studiengang")
 ,("Degree programs"     ,"Studiengänge")
 ,("Degree programs at the department of computer science",
   "Studiengänge im Institut für Informatik")
 ,("Delete module"      ,"Modul löschen")
 ,("Delete prerequisites","Voraussetzungen löschen")
 ,("delete as prerequisite","als Voraussetzung löschen")
 ,("Description"        ,"Beschreibung")
 ,("Duration:"          ,"Dauer:")
 ,("External URL for module","Externe URL für das Modul")
 ,("Formatted module description","Formatierte Modulbeschreibung")
 ,("Formatted module descriptions","Formatierte Modulbeschreibungen")
 ,("English"            ,"Englisch")
 ,("every year"         ,"jedes Jahr")
 ,("every year in summer term","jedes Jahr im SS")
 ,("every year in winter term","jedes Jahr im WS")
 ,("every semester"     ,"jedes Semester")
 ,("For programmers:"   ,"Für Programmierer:")
 ,("For persons in charge for modules: ","Für Modulverantwortliche: ")
 ,("Forgot your login data?","Login-Daten vergessen?")
 ,("format modules (PDF)","Module formatieren (PDF)")
 ,("Found modules"      ,"Gefundene Module")
 ,(" from "             ," von ")
 ,("Further information:","Weitere Informationen:")
 ,("Further modules"    ,"Weitere Module")
 ,("General "           ,"Allgemeine ")
 ,("German"             ,"Deutsch")
 ,("Go to"              ,"Gehe zu")
 ,("in the module code or title","im Modulcode oder -titel")
 ,("Department of Computer Science","Institut für Informatik")
 ,("irregular"          ,"unregelmäßig")
 ,("LaTeX output"       ,"LaTeX-Ausgaben")
 ,("Logged in as: "     ,"Angemeldet als: ")
 ,("Logged out"         ,"Abgemeldet")
 ,("Login"              ,"Anmelden")
 ,("Login name:"        ,"Benutzername:")
 ,("Login successful"   ,"Anmeldung erfolgreich")
 ,("Login to module database","Anmeldung zur Moduldatenbank")
 ,("Login data for module database","Moduldatenbankzugangsdaten")
 ,("Logout"             ,"Abmelden")
 ,("Main page of the module information system","Hauptseite der Moduldatenbank")
 ,("mandatory"          ,"Pflicht")
 ,("Mandatary modules"  ,"Pflichtmodule")
 ,("Master programs"    ,"Masterprogramme")
 ,("Master programs (since WS15/16)","Masterprogramme (ab WS15/16)")
 ,("Master programs (until SS15)","Masterprogramme (bis SS15)")
 ,("Master programs in computer science","Programme im Masterstudiengang Informatik")
 ,("Master studies in computer science:","Masterstudium Informatik:")
 ,("Maximal ECTS points in this category",
   "Maximale ECTS-Punkte in dieser Kategorie")
 ,("Minimal ECTS points in this category",
   "Minimale ECTS-Punkte in dieser Kategorie")
 ,("Missing UnivIS entry!","UnivIS-Eintrag fehlt!")
 ,("Module: "           ,"Modul: ")
 ,("Module categories:" ,"Modulkategorien:")
 ,("Module code:"       ,"Modulcode:")
 ,("Module dependencies","Modulabhängigkeiten")
 ,("Module Information System","Modulinformationssystem Informatik")
 ,("Modules of"         ,"Module von")
 ,("My modules"         ,"Eigene Module")
 ,("New advisor program","Neues Studienprogramm")
 ,("New degree program" ,"Neuer Studiengang")
 ,("New master program" ,"Neues Masterprogramm")
 ,("New password:"      ,"Neues Passwort:")
 ,("New passwords are different!","Neue Passwörter sind verschieden!")
 ,("New master program" ,"Neues Masterprogramm")
 ,("not logged in"      ,"nicht angemeldet")
 ,("notes on module descriptions and their preparation",
   "Hinweise zu Modulbeschreibungen und deren Bearbeitung")
 ,("one"                ,"ein")
 ,("only for internal use","nur zur internen Bearbeitung")
 ,("Old password:"      ,"Altes Passwort:")
 ,("Overview on the "   ,"Übersicht über die ")
 ,("Password:"          ,"Passwort:")
 ,("Password changed"   ,"Passwort geändert")
 ,("persons in charge"  ,"Modulverantwortliche")
 ,("Person in charge:"  ,"Modulverantwortliche(r):")
 ,("planning instruments of the institute","Lehrplanungsinstrumente des Instituts")
 ,("Prerequisites"      ,"Voraussetzungen")
 ,("Prerequisites:"     ,"Voraussetzungen:")
 ,("Prerequisite selection","Voraussetzungen bearbeiten")
 ,("Presence:"          ,"Präsenzzeiten:")
 ,("Program overview by categories:","Programmübersicht nach Studienbereichen:")
 ,("Program overview by terms:","Programmübersicht nach Semestern:")
 ,("programming language","Programmiersprache")
 ,("public"             ,"öffentlich")
 ,("recommended"        ,"empfohlen")
 ,("Really logout?"     ,"Wirklich abmelden?")
 ,("Repeat new password:","Neues Passwort wiederholen:")
 ,("search"             ,"suchen")
 ,("Search!"            ,"Suchen!")
 ,("Search modules"     ,"Modulsuche")
 ,("Search all modules containing","Alle Module mit Zeichenfolge")
 ,("Search for individual modules:","Einzelne Module suchen:")
 ,("Select a module:"   ,"Modul auswählen")
 ,("Select person:"     ,"Person auswählen")
 ,("semester"           ,"Semester")
 ,("Send login data"    ,"Login-Daten zusenden")
 ,("Send new password"  ,"Neues Passwort senden")
 ,("Semester planning"  ,"Semesterplanung")
 ,("Show"               ,"Anzeigen")
 ,("show"               ,"anzeigen")
 ,("Show all master programs",
   "Alle (auch ältere) Masterprogramme anzeigen")
 ,("Show all module dependencies", "Alle Modulabhängigkeiten anzeigen")
 ,("Show all modules in this degree program",
   "Alle Module in diesem Studienprogramm anzeigen")
 ,("Show module selections:","Module anzeigen:")
 ,("show examination requirements","Prüfungsanforderungen anzeigen")
 ,("show modules"       ,"Module anzeigen")
 ,("Show semester modules:","Module eines Semesters anzeigen:")
 ,("Show modules of a person","Module einer Person anzeigen")
 ,("Start: "            ,"Beginn: ")
 ,("Store"              ,"Speichern")
 ,("Study planner"      ,"Studienplaner")
 ,("Supported by:"      ,"Unterstützt durch:")
 ,(" to "               ," bis ")
 ,("Teaching language:" ,"Lehrsprache:")
 ,("to UnivIS entry"    ,"zum UnivIS-Eintrag")
 ,("UnivIS entry without MDB entry!","UnivIS-Eintrag ohne MDB-Eintrag!")
 ,("two"                ,"zwei")
 ,("Visibility:"         ,"Sichtbarkeit:")
 ,("with master program usage","mit Masterprogrammverwendungen")
 ,("with UnivIS comparison","mit UnivIS-Abgleich")
 ,("with pattern"       ,"mit Muster")
 ,("with student numbers","mit Studierendenzahlen")
 ,("Wrong login data!"  ,"Falsche Login-Daten!")
 ,("Wrong password!"    ,"Falsches Passwort!")
 ,("XML index to all modules","XML-Index aller Module")
 ,("XML document with all master programs","XML-Dokument aller Masterprogramme")
 ,("XML document with all master programs (until SS15)"
  ,"XML-Dokument aller Masterprogramme (bis SS15)")
 ,("You can also "      ,"Sie können auch nur Ihr ")
 ,("Your email address: ","Ihre Email-Adresse: ")
 ,("Your new password has been sent","Ihr neues Passwort wurde Ihnen zugesandt")
 ]

loginText :: UserSessionInfo -> String -> String
loginText sinfo loginname = langSelect sinfo
  ("You are logged in as user '" ++ loginname ++
   "' and are allowed to change your modules and programs.")
  ("Sie sind als Benutzer '" ++ loginname ++
   "' angemeldet und können Ihre Module und Programme bearbeiten.")

loginEmailText :: UserSessionInfo -> String -> String -> String
loginEmailText sinfo loginname passwd = langSelect sinfo
  ("Your login data:\n\nLogin name: " ++ loginname ++
   "\nNew password: " ++ passwd ++
   "\n\nYou can use this data to login into the module database\n\n"++
   baseURL++"\n\n"++
   "and work on your modules and master programs.\n\n"++
   "You can change your password after the login by selecting 'Logout'\n"++
   "followed by 'Change password'.")
  ("Ihre Zugangsdaten sind:\n\nLogin-Name: " ++ loginname ++
   "\nNeues Passwort: " ++ passwd ++
   "\n\nMit diesen Daten koennen Sie sich in der Moduldatenbank\n\n"++
   baseURL++"\n\n"++
   "anmelden und Ihre Module und Masterprogramme aendern.\n\n"++
   "Sie koennen das Passwort aendern, indem Sie sich anmelden\n"++
   "und dann nach Auswahl von 'Abmelden' den Punkt\n"++
   "'Passwort aendern' waehlen.")

mainTitle :: UserSessionInfo -> String
mainTitle sinfo = langSelect sinfo
  "Modules and degree programs of the Department of Computer Science"
  "Module und Studiengänge des Instituts für Informatik"

mainExplanation :: UserSessionInfo -> [HtmlExp]
mainExplanation sinfo = langSelect sinfo
  [htxt $
    "This web site provides an overview on all modules and "++
    "degree programs offered by the Department of Computer Science. "++
    "Additionally, it contains an overiew on all master programs "++
    "in computer science and business information technology. "++
    "A list of all modules offered in English "++
    "can be found in the category \"Search modules\"."]
  [htxt $ "Auf diesen Webseiten sind die Module aller Studiengänge "++
    "des Instituts für Informatik sowie alle vom Institut "++
     "angebotenen Module beschrieben. "++
     "Außerdem befindet sich hier eine Übersicht über alle "++
     "angebotenen Masterprogramme in Informatik und Wirtschaftsinformatik.",
   htxt $ "Aktuelle Informationen zu den Lehrveranstaltungen kann man auch im ",
   spEHref univisURL [htxt "UnivIS"],
   htxt $ " finden."]
 where
  univisURL = "http://univis.uni-kiel.de/"

masterStudyNote :: UserSessionInfo -> [HtmlExp]
masterStudyNote sinfo = langSelect sinfo
  [italic [htxt "Important note: "],
   htxt "All master students should plan their individual studies with the ",
   spEHref studyPlannerURL [bold [htxt "study planner"]], htxt "!"]
  [italic [htxt "Wichtiger Hinweis: "],
   htxt "Alle Masterstudierenden sollten ihre individualle Planung mit dem ",
   spEHref studyPlannerURL [bold [htxt "Studienplaner"]],
   htxt " durchführen! ",
   htxt "Damit wird weitgehend gewährleistet, dass das geplante Studium ",
   htxt "auch wirklich durchführbar ist."]
   
masterStudyOldNote :: UserSessionInfo -> [HtmlExp]
masterStudyOldNote sinfo = langSelect sinfo
  [htxt "The master programs until SS15 can be found ",
   spEHref oldMasterProgURL [bold [htxt "here"]], htxt "."]
  [htxt "Die Masterprogramme bis zum Sommersemester 2015 sind ",
   spEHref oldMasterProgURL [bold [htxt "hier"]],
   htxt " zu finden."]
 where
  oldMasterProgURL = "?MasterProgram/list" 
   
minorSubjectNote :: UserSessionInfo -> [HtmlExp]
minorSubjectNote sinfo = langSelect sinfo
  [italic [htxt "Note: "],
   htxt "The possible minor/application subjects and their modules are listed ",
   spEHref minorURL [htxt "on this page."]]
  [italic [htxt "Hinweis: "],
   htxt "Die möglichen Anwendungsgebiete im Bachelor- und Masterstudiengang ",
   htxt "Informatik sowie die dazugehörigen Module findet man ",
   spEHref minorURL [htxt "auf dieser Seite."]]
 where
  minorURL = "http://www.inf.uni-kiel.de/de/studium/studiengaenge/inf-1-fach/bachelorstudiengang/nebenfaecher_anwendungsgebiete"

sendPasswordCmt :: UserSessionInfo -> String
sendPasswordCmt sinfo = langSelect sinfo
  ("You can send a new password to your email address "++
   "if you are registered in the system.")
  ("Sie können sich ein neues Password an Ihre Email-Adresse " ++
   "zusenden lassen, sofern Sie im System registriert sind.")
  
ssComment :: UserSessionInfo -> String
ssComment sinfo = langSelect sinfo
  ("If the master studies are started in the summer term, "++
   "one can also choose a master program from an adjacent winter term. "++
   "Ask your academic advisor to adapt such a master program.")
  ("Bei Beginn im Sommersemester können auch Programme der "++
   "benachbarten Wintersemester gewählt werden. "++
   "Bei der Anpassung berät Sie der Academic Advisor.")

timeoutText :: UserSessionInfo -> String
timeoutText sinfo = langSelect sinfo
  ("Please note that you are automatically logged out "++
   "if you are inactive for more than 60 minutes.")
  ("Bitte beachten Sie, dass Sie bei mehr als 60 Minuten "++
   "Inaktivität automatisch wieder abgemeldet werden.")

unknownUser :: UserSessionInfo -> String
unknownUser sinfo = langSelect sinfo
  "There is no user with this email address!"
  "Ein Benutzer mit dieser Email-Adresse ist im System nicht bekannt!"

useURLText :: UserSessionInfo -> String
useURLText sinfo = langSelect sinfo
  "Please use the following URL to refer to this module from other web pages:"
  "Bitte verwenden Sie die folgende URL, um das Modul aus anderen Webseiten zu referenzieren:"

--------------------------------------------------------------------------
-- Auxiliaries:

spEHref :: String -> [HtmlExp] -> HtmlExp
spEHref ref hexps =
  href ref hexps `addClass` "btn btn-sm btn-default"
                 `addAttr` ("target","_blank")