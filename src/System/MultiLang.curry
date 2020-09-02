----------------------------------------------------------------------------
--- This module contains support for multi-language access
--- and the translation of texts shown in the application.
---
--- @author Michael Hanus
--- @version July 2020
----------------------------------------------------------------------------

module System.MultiLang (
  toEnglish,
  translate, langSelect,
  loginEmailText, mainTitle, mainExplanation,
  masterStudyOldNote, minorSubjectNote,
  privacyCookieCmt, searchToolTip, sendCodeCmt, sendPasswordCmt, ssComment,
  studentExplanation, studentLoginEmailText,studentLoginExplanation,
  timeoutText, unknownUser, useURLText, prereqExplainText
 ) where

import System.SessionInfo
import HTML.Base
import HTML.Styles.Bootstrap4
import ConfigMDB              ( baseURL, studyPlannerURL )

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
 [("...as lecturer"     ,"...als Modulverwalter")
 ,("...as student"      ,"...als Studierender")
 ,("Acknowledgment"     ,"Bestätigung")
 ,("Add"                ,"Hinzufügen")
 ,("Add semester"       ,"Semester hinzufügen")
 ,("Add prerequisite"   ,"Voraussetzung hinzufügen")
 ,("Advisor programs"   ,"Studienprogramme")
 ,("All categories"     ,"Alle Kategorien")
 ,("All taught modules" ,"Alle gehaltenen Module")
 ,("All modules"        ,"Alle Module")
 ,("all modules"        ,"alle Module")
 ,("All modules of a person","Alle Module einer Person")
 ,("All my modules"     ,"Alle eigenen Module")
 ,("All English modules","Alle englischen Module")
 ,("Back to last page"  ,"Zurück zur letzten Seite")
 ,("Cancel"             ,"Abbrechen")
 ,("Change"             ,"Ändern")
 ,("Change basic data"  ,"Basisdaten ändern")
 ,("Change description" ,"Beschreibung ändern")
 ,("Change password"    ,"Passwort ändern")
 ,("Change semesters"   ,"Semesterangaben ändern")
 ,("change your password","Passwort ändern")
 ,("Change visibility"  ,"Sichtbarkeit ändern")
 ,("Comments"           ,"Kommentare")
 ,("Copy module"        ,"Modul kopieren")
 ,("Core area: "        ,"Schwerpunktbereich: ")
 ,("Core areas"         ,"Schwerpunktbereiche")
 ,("Cycle:"             ,"Turnus:")
 ,("Degree program"      ,"Studiengang")
 ,("Degree programs"     ,"Studiengänge")
 ,("Delete module"      ,"Modul löschen")
 ,("Delete prerequisites","Voraussetzungen löschen")
 ,("delete as prerequisite","als Voraussetzung löschen")
 ,("Department of Computer Science","Institut für Informatik")
 ,("Description"        ,"Beschreibung")
 ,("Duration:"          ,"Dauer:")
 ,("Email address:"     ,"Email-Adresse:")
 ,("Email address already registered!","Email-Adresse im System schon registriert!")
 ,("Email address not allowed!","Unzulässige Email-Adresse!")
 ,("External URL for module","Externe URL für das Modul")
 ,("English"            ,"Englisch")
 ,("every year"         ,"jedes Jahr")
 ,("every year in summer term","jedes Jahr im SS")
 ,("every year in winter term","jedes Jahr im WS")
 ,("every semester"     ,"jedes Semester")
 ,("First name"         ,"Vorname")
 ,("For programmers:"   ,"Für Programmierer:")
 ,("For persons in charge for modules: ","Für Modulverantwortliche: ")
 ,("Forgot your login data?","Login-Daten vergessen?")
 ,("format modules (PDF)","Module formatieren (PDF)")
 ,("Formatted module description","Formatierte Modulbeschreibung")
 ,("Formatted module descriptions","Formatierte Modulbeschreibungen")
 ,("Found modules"      ,"Gefundene Module")
 ,(" from "             ," von ")
 ,("Further information:","Weitere Informationen:")
 ,("Further modules"    ,"Weitere Module")
 ,("General "           ,"Allgemeine ")
 ,("German"             ,"Deutsch")
 ,("Go to"              ,"Gehe zu")
 ,("in the module code or title","im Modulcode oder -titel")
 ,("irregular"          ,"unregelmäßig")
 ,("Information on data privacy", "Information zum Datenschutz")
 ,("Last name"          ,"Nachname")
 ,("LaTeX output"       ,"LaTeX-Ausgaben")
 ,("Logged in as"       ,"Angemeldet als")
 ,("Logged out"         ,"Abgemeldet")
 ,("Login"              ,"Anmelden")
 ,("Login as lecturer"  ,"Anmeldung als Modulverwalter")
 ,("Login as student"   ,"Anmeldung als Studierender")
 ,("Login code:"        ,"Zugangscode:")
 ,("Login name:"        ,"Benutzername:")
 ,("Login successful"   ,"Anmeldung erfolgreich")
 ,("Login data for module database","Moduldatenbankzugangsdaten")
 ,("Login or register as student"   ,"Anmeldung oder Registrierung als Studierender")
 ,("Logout"             ,"Abmelden")
 ,("Main page of the module information system","Hauptseite der Moduldatenbank")
 ,("Make visible"       ,"Sichtbar machen")
 ,("mandatory"          ,"Pflicht")
 ,("Mandatary modules"  ,"Pflichtmodule")
 ,("Master programs"    ,"Masterprogramme")
 ,("Master programs (since WS15/16)","Masterprogramme (ab WS15/16)")
 ,("Master programs (until SS15)","Masterprogramme (bis SS15)")
 ,("Master programs at the department of computer science",
   "Masterprogramme im Institut für Informatik")
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
 ,("Module dependencies","Modulabhängigkeiten")
 ,("Module Information System","Modulinformationssystem Informatik")
 ,("Modules of"         ,"Module von")
 ,("Modules without prerequisites","Module ohne Voraussetzungen")
 ,("My modules"         ,"Eigene Module")
 ,("New advisor program","Neues Studienprogramm")
 ,("New degree program" ,"Neuer Studiengang")
 ,("New master program" ,"Neues Masterprogramm")
 ,("New password:"      ,"Neues Passwort:")
 ,("New passwords are different!","Neue Passwörter sind verschieden!")
 ,("New master program" ,"Neues Masterprogramm")
 ,("No","Nein")
 ,("not logged in"      ,"nicht angemeldet")
 ,("notes on module descriptions and their preparation",
   "Hinweise zu Modulbeschreibungen und deren Bearbeitung")
 ,("one"                ,"ein")
 ,("only"               ,"nur")
 ,("only for internal use","nur zur internen Bearbeitung")
 ,("Old password:"      ,"Altes Passwort:")
 ,("Overview on the "   ,"Übersicht über die ")
 ,("Password:"          ,"Passwort:")
 ,("Password changed"   ,"Passwort geändert")
 ,("persons in charge"  ,"Modulverantwortliche")
 ,("Person in charge:"  ,"Modulverantwortliche(r):")
 ,("planning instruments of the institute","Lehrplanungsinstrumente des Instituts")
 ,("Prerequisites"      ,"Voraussetzungen")
 ,("Prerequisite selection","Voraussetzungen bearbeiten")
 ,("Presence:"          ,"Präsenzzeiten:")
 ,("Program overview by categories:","Programmübersicht nach Studienbereichen:")
 ,("Program overview by terms:","Programmübersicht nach Semestern:")
 ,("programming language","Programmiersprache")
 ,("public"             ,"öffentlich")
 ,("Quick search"       ,"Schnellsuche")
 ,("Really logout?"     ,"Wirklich abmelden?")
 ,("Register as new student","Registrierung als neuer Studierender")
 ,("Register"           ,"Registrieren")
 ,("recommended"        ,"empfohlen")
 ,("Repeat new password:","Neues Passwort wiederholen:")
 ,("search"             ,"suchen")
 ,("Search!"            ,"Suchen!")
 ,("Search modules"     ,"Modulsuche")
 ,("Search all modules containing","Alle Module mit Zeichenfolge")
 ,("Search for individual modules:","Einzelne Module suchen:")
 ,("Select a module:"   ,"Modul auswählen")
 ,("select/change modules in semester","Module im Semester auswählen/ändern")
 ,("Select/change modules","Module auswählen/ändern")
 ,("Select the modules you like to attend in ",
   "Modulauswahl für das Semester ")
 ,("Select person:"     ,"Person auswählen")
 ,("Select semester:"   ,"Semester auswählen")
 ,("semester"           ,"Semester")
 ,("Send login data"    ,"Login-Daten zusenden")
 ,("Send new login code","Neuen Zugangscode senden")
 ,("Send new password"  ,"Neues Passwort senden")
 ,("Semester planning"  ,"Semesterplanung")
 ,("Show"               ,"Anzeigen")
 ,("show"               ,"anzeigen")
 ,("Show all master programs",
   "Alle (auch ältere) Masterprogramme anzeigen")
 ,("Show all module dependencies", "Alle Modulabhängigkeiten anzeigen")
 ,("Show all modules in this degree program",
   "Alle Module in diesem Studienprogramm anzeigen")
 ,("show examination requirements","Prüfungsanforderungen anzeigen")
 ,("Show modules:"      ,"Module anzeigen:")
 ,("show modules"       ,"Module anzeigen")
 ,("Show selected modules","Ausgewählte Module anzeigen")
 ,("Show semester modules:","Module eines Semesters anzeigen:")
 ,("Show modules of a person","Module einer Person anzeigen")
 ,("Start: "            ,"Beginn: ")
 ,("Store"              ,"Speichern")
 ,("Study planner"      ,"Studienplaner")
 ,("Supported by:"      ,"Unterstützt durch:")
 ,(" to "               ," bis ")
 ,("Taught modules"     ,"Gehaltene Module")
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
 ,("Your email address used for registration: "
  ,"Ihre registrierte Email-Adresse: ")
 ,("Your new login code has been sent to:"
  ,"Ihr neuer Zugangscode wurde gesendet an:")
 ,("Yes","Ja")
 ,("Your new password has been sent","Ihr neues Passwort wurde Ihnen zugesandt")
 ,("Your selected modules:","Ihre ausgewählten Module:")
 ]

loginEmailText :: UserSessionInfo -> String -> String -> String
loginEmailText sinfo loginname passwd = langSelect sinfo
  ("Your login data:\n\nLogin name: " ++ loginname ++
   "\nNew password: " ++ passwd ++
   "\n\nYou can use this data to login into the module database\n\n"++
   baseURL++"\n\n"++
   "and work on your modules and master programs.\n\n"++
   "You can change your password after the login by selecting\n"++
   "'Change password' in the user menu.")
  ("Ihre Zugangsdaten sind:\n\nLogin-Name: " ++ loginname ++
   "\nNeues Passwort: " ++ passwd ++
   "\n\nMit diesen Daten koennen Sie sich in der Moduldatenbank\n\n"++
   baseURL++"\n\n"++
   "anmelden und Ihre Module und Masterprogramme aendern.\n\n"++
   "Sie koennen das Passwort aendern, indem Sie sich anmelden\n"++
   "und dann 'Passwort aendern' im Benutzermenu waehlen.")

studentLoginEmailText :: UserSessionInfo -> String -> String -> String
studentLoginEmailText sinfo email code = langSelect sinfo
  ("Your login data:\n\nEmail address: " ++ email ++
   "\nNew code: " ++ code ++
   "\n\nYou can use this data to login as a student into the module database\n\n"++
   baseURL++"\n\n"++
   "and select a plan for your modules.\n\n")
  ("Ihre Zugangsdaten sind:\n\nEmail-Adresse: " ++ email ++
   "\nNeuer Zugangscode: " ++ code ++
   "\n\nMit diesen Daten koennen Sie sich als Studierende(r) in der Moduldatenbank\n\n"++
   baseURL++"\n\n"++
   "anmelden und Ihre Module planen.\n\n")

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
   ehrefScndBadge univisURL [htxt "UnivIS"],
   htxt $ " finden."]
 where
  univisURL = "http://univis.uni-kiel.de/"

masterStudyOldNote :: UserSessionInfo -> [HtmlExp]
masterStudyOldNote sinfo = langSelect sinfo
  [htxt "The master programs until SS15 can be found ",
   hrefInfoBadge oldMasterProgURL [bold [htxt "here"]], htxt "."]
  [htxt "Die Masterprogramme bis zum Sommersemester 2015 sind ",
   hrefInfoBadge oldMasterProgURL [bold [htxt "hier"]],
   htxt " zu finden."]
 where
  oldMasterProgURL = "?MasterProgram/listall"
   
minorSubjectNote :: UserSessionInfo -> [HtmlExp]
minorSubjectNote sinfo = langSelect sinfo
  [italic [htxt "Note: "],
   htxt "The possible minor/application subjects and their modules are listed ",
   ehrefScndBadge minorURL [htxt "on this page."]]
  [italic [htxt "Hinweis: "],
   htxt "Die möglichen Anwendungsgebiete im Bachelor- und Masterstudiengang ",
   htxt "Informatik sowie die dazugehörigen Module findet man ",
   ehrefScndBadge minorURL [htxt "auf dieser Seite."]]
 where
  minorURL = "http://www.inf.uni-kiel.de/de/studium/studiengaenge/inf-1-fach/bachelorstudiengang/nebenfaecher_anwendungsgebiete"

privacyCookieCmt :: UserSessionInfo -> [HtmlExp]
privacyCookieCmt sinfo = langSelect sinfo
  [htxt $
    "This page uses cookies to store navigation information, login data, " ++
     "and language settings temporarily. By using this web site, you agree " ++
     "to use these cookies. ",
   htxt "There is also some ",
   ehref dataProtectCAU
         [htxt $ "general information about data protection " ++
                 "for the web sites of this univerity"],
   htxt " and ",
   ehref dataProtectMDB
         [htxt $ "specific informationen about data protection " ++
                 "for this web site"],
   htxt "."]
  [htxt $
     "Diese Seite verwendet Cookies zur temporären Speicherung von " ++
     "Navigationsinformationen, Anmeldedaten und Sprachwünschen. " ++
     "Durch die Verwendung dieser Webseite stimmen Sie dieser Nutzung " ++
     "von Cookies zu. ",
   htxt "Es gibt auch ",
   ehref dataProtectCAU
         [htxt $ "generelle Informationen zum Datenschutz für Webseiten " ++
                 "an dieser Universität"],
   htxt " und ",
   ehref dataProtectMDB
         [htxt "spezielle Informationen zum Datenschutz für diese Webseite"],
   htxt "."]
 where
  dataProtectCAU = "http://www.uni-kiel.de/suchen/impressum.shtml#datenschutz"
  dataProtectMDB = "datenschutz.html"

searchToolTip :: UserSessionInfo -> String
searchToolTip sinfo = langSelect sinfo
  "Show all modules containing a string in the module code or title."
  "Alle Module mit einer Zeichenfolge im Modulcode oder -titel anzeigen."

sendCodeCmt :: UserSessionInfo -> String
sendCodeCmt sinfo = langSelect sinfo
  ("You can send a new access code to your email address "++
   "if you are registered in the system as a student.")
  ("Sie können sich einen neuen Zugangscode an Ihre Email-Adresse " ++
   "zusenden lassen, sofern Sie im System als Studierende(r) registriert sind.")
  
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

studentExplanation :: UserSessionInfo -> [HtmlExp]
studentExplanation sinfo = langSelect sinfo
  [htxt $
    "You can select modules which you want to take in the next " ++
    "semesters. " ++
    "This selection is not a formal commitment but it helps " ++
    "the Department of Computer Science to plan the modules in " ++
    "each semester so that possible scheduling conflicts are reduced. "]
  [htxt $
    "Sie können in der Moduldatenbank " ++
    "Module auswählen, die Sie in den nächsten Semestern hören wollen. " ++
    "Diese Auswahl ist nicht verbindlich, sondern hilft dem Institut, " ++
    "das Modulangebot so zu planen, dass es möglichst wenig " ++
    "Terminüberschneidungen gibt. "]

studentLoginExplanation :: UserSessionInfo -> [HtmlExp]
studentLoginExplanation sinfo = studentExplanation sinfo ++ langSelect sinfo
  [htxt $
    "In order to store and change your selection, you have to register " ++
    "to the module database with your 'stu...@mail.uni-kiel.de' account " ++
    "and login with the generated access code."]
  [htxt $
    "Um diese Auswahl zu speichern und auch zu ändern, müssen Sie " ++
    "sich in der Moduldatenbank mit Ihrem 'stu...@mail.uni-kiel.de' " ++
    "Account einmal registrieren und danach mit dem generierten " ++
    "Zugangscode anmelden."]

timeoutText :: UserSessionInfo -> String
timeoutText sinfo = langSelect sinfo
  ("Please note: you will be automatically logged out "++
   "if you are inactive for more than 60 minutes.")
  ("Bitte beachten: bei mehr als 60 Minuten Inaktivität " ++
   "erfolgt eine automatische Abmeldung.")

unknownUser :: UserSessionInfo -> String
unknownUser sinfo = langSelect sinfo
  "There is no user with this email address!"
  "Ein Benutzer mit dieser Email-Adresse ist im System nicht bekannt!"

useURLText :: UserSessionInfo -> String
useURLText sinfo = langSelect sinfo
  "Please use the following URL to refer to this module from other web pages:"
  "Bitte verwenden Sie die folgende URL, um das Modul aus anderen Webseiten zu referenzieren:"

prereqExplainText :: UserSessionInfo -> String
prereqExplainText sinfo = langSelect sinfo
  "The skills of these modules are required."
  "Die in diesen Modulen vermittelten Kompetenzen werden vorausgesetzt."

--------------------------------------------------------------------------
