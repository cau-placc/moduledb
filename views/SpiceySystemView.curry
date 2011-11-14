--------------------------------------------------------------------------
--- This module implements the views related to the standard controllers
--- in a Spicey application.
--- In particular, it defines a default view for login
--- and a view of a list of user processes.
--------------------------------------------------------------------------

module SpiceySystemView(loginView,processListView,historyView)
 where

import UserProcesses
import Processes
import Spicey
import Authentication
import KeyDatabase
import MDB
import DefaultController
import Mail
import ConfigMDB

-----------------------------------------------------------------------------
--- View for login/logout. If the passed login name is the empty string,
--- we offer a login dialog, otherwise a logout dialog.
loginView :: Controller -> Maybe String -> [HtmlExp]
loginView controller currlogin =
  case currlogin of
   Nothing -> [h3 [htxt "Anmeldung zur Moduldatenbank"],
               table [[[htxt "Benutzername:"], [textfield loginfield ""]],
                      [[htxt "Passwort:"],     [password passfield]]], hrule,
               par [button "Anmelden" loginHandler],
               hrule,
               par [button "Login-Daten vergessen?" (const sendLoginDataForm)]]
   Just _  -> [h3 [htxt "Wirklich abmelden?"],
               par [button "Abmelden"  (logoutHandler True),
                    button "Abbrechen" (logoutHandler False)],
               hrule,
               h3 [htxt "Sie können auch nur Ihr ",
                   button "Passwort ändern" (const passwdForm)]]
 where
  loginfield,passfield free

  loginHandler env = do
    let loginname = env loginfield
    hashpass <- getUserHash loginname (env passfield)
    users <- runQ $ queryUserByLoginPass loginname hashpass
    if null users
      then do setPageMessage "Falsche Login-Daten!"
              nextInProcessOr controller Nothing >>= getForm
      else do loginToSession loginname
              setPageMessage ("Angemeldet als: "++loginname)
              urls <- getLastUrls
              getForm $
               [h1 [htxt "Anmeldung erfolgreich"],
                par [htxt $ "Sie sind als Benutzer '" ++ loginname ++
                            "' angemeldet und können Ihre Module und " ++
                     "Programme bearbeiten."],
                par [htxt timeoutText]] ++
                if length urls > 1
                then [par [href ('?':urls!!1) [htxt "Zurück zur letzten Seite"]]]
                else []

  timeoutText = "Bitte beachten Sie, dass Sie bei mehr als 60 Minuten "++
                "Inaktivität automatisch wieder abgemeldet werden."

  logoutHandler confirm _ = do
    if confirm then logoutFromSession >> setPageMessage "Abgemeldet"
               else cancelOperation
    nextInProcessOr controller Nothing >>= getForm

--- Gets a user entry with a given login name and (hashed) password:
queryUserByLoginPass :: String -> String -> Query [User]
queryUserByLoginPass login pass = seq login $ seq pass $
  queryCondUser (\u -> userLogin u == login && userPassword u == pass)

-----------------------------------------------------------------------------
-- a form to change the password
passwdForm :: IO HtmlForm
passwdForm = do
  login <- getSessionLogin
  getForm
    [h1 [htxt "Passwort ändern"],
     table [[[htxt "Altes Passwort:"], [password oldpass]],
            [[htxt "Neues Passwort:"], [password newpass1]],
            [[htxt "Neues Passwort wiederholen:"], [password newpass2]]],
     hrule,
     button "Passwort ändern" (\ e -> passwdHandler login e >>= getForm), nbsp,
     button "Abbrechen"
            (const (cancelOperation >> defaultController >>= getForm))]
 where
  oldpass,newpass1,newpass2 free

  passwdHandler Nothing _ = cancelOperation >> defaultController
  passwdHandler (Just lname) env = do
    hashpass <- getUserHash lname (env oldpass)
    users <- runQ $ queryUserByLoginPass lname hashpass
    if null users
     then displayError "Falsches Passwort!"
     else do let u = head users
             newhashpass <- getUserHash lname (env newpass1)
             if env newpass1 /= env newpass2
              then displayError "Neue Passwörter sind verschieden!"
              else do runT (updateUser (setUserPassword u newhashpass))
                      setPageMessage "Passwort geändert"
                      defaultController

------------------------------------------------------------------------
-- send login data to a new user
sendLoginDataForm :: IO HtmlForm
sendLoginDataForm =
  getForm
    [h1 [htxt "Login-Daten zusenden"],
     par [htxt "Sie können sich ein neues Password an Ihre Email-Adresse ",
          htxt " zusenden lassen, sofern Sie im System registriert sind."],
     par [htxt "Ihre Email-Adresse: ", textfield emailref ""],
     hrule,
     button "Neues Passwort senden" sendHandler,
     button "Abbrechen"
            (const (cancelOperation >> defaultController >>= getForm))]
 where
  emailref free

  sendHandler env = do
    users <- runQ $ queryCondUser (\u -> userEmail u == env emailref)
    let err = "Ein Benutzer mit dieser Email-Adresse ist im System nicht bekannt!"
    if null users
     then displayError err >>= getForm
     else sendNewEmail (head users)

  sendNewEmail user = do
    newpass <- randomPassword 10
    hashpass <- getUserHash (userLogin user) newpass
    sendMail adminEmail
      (userEmail user)
      "Moduldatenbankzugangsdaten"
      ("Ihre Zugangsdaten sind:\n\nLogin-Name: " ++ userLogin user ++
       "\nNeues Passwort: " ++ newpass ++
       "\n\nMit diesen Daten koennen Sie sich in der Moduldatenbank\n\n"++
       "http://www-ps.informatik.uni-kiel.de/~mh/studiengaenge/\n\n"++
       "anmelden und Ihre Module und Masterprogramme aendern.\n\n"++
       "Sie koennen das Passwort aendern, indem Sie sich anmelden\n"++
       "und dann nach Auswahl von 'Abmelden' den Punkt\n"++
       "'Passwort aendern' waehlen.")
    runT $ updateUser (setUserPassword user hashpass)
    getForm [h1 [htxt "Bestätigung"],
             h3 [htxt "Ihr neues Passwort wurde Ihnen zugesandt"]]

------------------------------------------------------------------------
--- A view for all processes contained in a given process specification.
processListView :: Processes a -> [HtmlExp]
processListView procs =
  [h1 [htxt "Processes"],
   ulist (map processColumn (zip (processNames procs) [1..]))]
 where
   processColumn (pname, id) =
     [href ("?spiceyProcesses/"++show id) [htxt pname]]

-----------------------------------------------------------------------------
--- A view for all URLs of a session.
historyView :: [String] -> [HtmlExp]
historyView urls =
  [h1 [htxt "History"],
   ulist (map (\url -> [href ("?"++url) [htxt url]])
              (filter (not . null) urls))]

-----------------------------------------------------------------------------
