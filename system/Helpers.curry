-------------------------------------------------------------------------------
-- Auxiliaries for module management
-------------------------------------------------------------------------------

module Helpers(LogEvent(..),logEvent,
               masterProgsLatexFile,
               modInfoLatexFile,modTableLatexFile,semTableLatexFile,
               shortModInfoLatexFile,
               ehref,
               showDigit2,showDiv10, formatPresence,
               string2hrefs, latex2html, html2latex,
               showSemester, nextSemester, prevSemester, leqSemester,
               semesterSelection, lowerSemesterSelection, upperSemesterSelection,
               imageNB, wTerm, wYear, wVisible)
  where

import ConfigMDB
import MDB
import IO
import IOExts
import Time
import System(getEnviron,getHostname)
import List
import Integer(abs)
import HTML
import WUI
import Char
import Authentication
import ReadShowTerm

-------------------------------------------------------------------------------
-- Logging:

-- logfile:
logFile :: String
logFile = storageDir++"CHANGE.LOG"

-- The kind of events we want to log.
data LogEvent =
    NewModData  ModData  | UpdateModData  ModData  | DeleteModData  ModData
  | NewModDescr ModDescr | UpdateModDescr ModDescr | DeleteModDescr ModDescr
  | NewModInst  ModInst  | UpdateModInst  ModInst  | DeleteModInst  ModInst
  | NewMasterProgram MasterProgram | UpdateMasterProgram MasterProgram
  | DeleteMasterProgram MasterProgram
  | NewMasterProgInfo MasterProgInfo | UpdateMasterProgInfo MasterProgInfo
  | DeleteMasterProgInfo MasterProgInfo

-- A info in the log file consists of info about the time,
-- the current user, the remote host, and a log event.
data LogInfo = LogInfo String String String LogEvent

-- Adds an event with some info string to the global log file.
logEvent :: LogEvent -> IO ()
logEvent event = exclusiveIO (logFile++".lock") $ do
  time  <- getLocalTime
  login <- getSessionLogin
  raddr <- getEnviron "REMOTE_ADDR"
  rhost <- if null raddr then getHostname else getHostnameForIP raddr
  appendFile logFile
     (showQTerm (LogInfo (calendarTimeToString time) (maybe "???" id login)
                         (rhost++"/"++raddr) event) ++ "\n")

--- Get symbolic name of ip address:
getHostnameForIP :: String -> IO String
getHostnameForIP ipaddr = (flip catchFail) (return "") $ do
  h <- connectToCommand $ "host " ++ ipaddr
  b <- hIsEOF h
  if b then return ""
       else hGetLine h >>= return . last . words

-------------------------------------------------------------------------------
-- name of LaTeX include with infos for all modules:
modInfoLatexFile = "include_modinfo.tex"

-- name of LaTeX include with short info for all modules:
shortModInfoLatexFile = "include_shortinfo.tex"

-- name of LaTeX include which contains the catalogue table:
modTableLatexFile = "include_moduletable.tex"

-- name of LaTeX include which contains the semester table:
semTableLatexFile = "include_semestertable.tex"

-- name of LaTeX include which contains all master programs:
masterProgsLatexFile = "include_mscprograms.tex"

-------------------------------------------------------------------------------
-- an external reference:
ehref :: String -> [HtmlExp] -> HtmlExp
ehref ref hexp = href ref hexp `addAttr` ("target","_blank")

-------------------------------------------------------------------------------

-- show an integer with at leat 2 digit (i.e., leading 0):
showDigit2 :: Int -> String
showDigit2 i = if abs i < 10 then '0':show i else show i

-- show an integer divided by 10:
showDiv10 :: Int -> String
showDiv10 i = let m = mod i 10
               in show (div i 10) ++ (if m==0 then "" else ',' : show m)

-----------------------------------------------------------------------------
-- format the presence field string of ModData entities
-- (delete 0 fields, replace P bei PUe):
formatPresence :: String -> String
formatPresence ps =
  let xs = words ps
   in unwords (map p2pue (filter (\s -> head s /= '0') xs))
 where
   p2pue s = case s of
               [d,'P'] -> [d,'P','Ü']
               _ -> s

-----------------------------------------------------------------------------
-- translate potential URLs in a string into HTML hyperrefs:
string2hrefs :: String -> String
string2hrefs s =
  let (sps,s1) = break (not . isSpace) s
      (w,s2)   = break isSpace s1
   in if w==""
      then s -- there is no word
      else sps ++ toHref w ++ string2hrefs s2
 where
  toHref w = if take 7 w == "http://"
             then "<a href=\""++w++"\" target=\"_blank\">"++w++"</a>"
             else w

-----------------------------------------------------------------------------
-- translate string containing some standard latex markups into HTML:
latex2html [] = []
latex2html (c:cs) | c=='\\' = tryTrans c cs slashtrans
                  | c=='"'  = tryTrans c cs apotrans
                  | c=='\n' = tryLinebreak cs
                  | otherwise = htmlIsoUmlauts [c] ++ latex2html cs
 where
  tryLinebreak xs = let (ys,zs) = break (=='\n') xs in
    if null zs || not (all isSpace ys)
    then '\n' : latex2html xs
    else "<br/>" ++ latex2html (tail zs)

  tryTrans x xs [] = x : latex2html xs
  tryTrans x xs ((lmacro,hmacro) : ms) = let l = length lmacro in
    if take l xs == lmacro then hmacro ++ latex2html (drop l xs)
                           else tryTrans x xs ms

  slashtrans = [("begin{itemize}","<ul>"),("end{itemize}","</ul>"),
                ("begin{enumerate}","<ol>"),("end{enumerate}","</ol>"),
                ("item","<li>"),
                ("\"a","&auml;"),("\"o","&ouml;"),("\"u","&uuml;"),
                ("\"A","&Auml;"),("\"O","&Ouml;"),("\"U","&Uuml;"),
                ("ss{}","&szlig;"),("-",""),("\\","<br/>"),(" "," "),
                ("%","%")]

  apotrans = [("a","&auml;"),("o","&ouml;"),("u","&uuml;"),("s","&szlig;"),
              ("A","&Auml;"),("O","&Ouml;"),("U","&Uuml;"),
              ("\'","\""),("`","\"")]
         
-- translate some standard HTML markups in a string into latex equivalents:
html2latex [] = []
html2latex (c:cs) | c=='<' = tryTrans c cs htmltrans
                  | otherwise = c : html2latex cs
 where
  tryTrans x xs [] = x : html2latex xs
  tryTrans x xs ((hmacro,lmacro) : ms) = let l = length hmacro in
    if take l xs == hmacro then lmacro ++ html2latex (drop l xs)
                           else tryTrans x xs ms

  htmltrans = [("ul>","\\begin{itemize}"),("/ul>","\\end{itemize}"),
               ("ol>","\\begin{enumerate}"),("/ol>","\\end{enumerate}"),
               ("li>","\\item{}"),("/li>","")]
         
-----------------------------------------------------------------------------
-- Semester/Year management:

-- show a semester:
showSemester (sem,year) =
  let yr2 = year `mod` 100
   in if sem=="SS" then "SS"++showDigit2 yr2
                   else "WS"++showDigit2 yr2++"/"++showDigit2 (yr2+1)

-- compute following semester:
nextSemester (sem,year) = if sem=="SS" then ("WS",year) else ("SS",year+1)

-- compute previous semester:
prevSemester (sem,year) = if sem=="WS" then ("SS",year) else ("WS",year-1)

-- compare semesters:
leqSemester (s1,y1) (s2,y2) = y1 < y2 || (y1 == y2 && (s1 == s2 || s1 == "SS"))

-- a list of semesters to select in some WUIs
semesterSelection :: [(String,Int)]
semesterSelection = take 14 (iterate nextSemester ("WS",2008))

-- preselected lower and upper bound index of a semester list:
lowerSemesterSelection =
  maybe (error "Helpers.lowerSemesterSelection: current semester not found!")
        id
        (findIndex (==currentSemester) semesterSelection)

upperSemesterSelection = lowerSemesterSelection + 3

-----------------------------------------------------------------------------
--- An image without borders.
imageNB :: String -> String -> HtmlExp
imageNB r a = image r a `addAttr` ("border","0")


--- The WUI specification for a term.
wTerm :: WuiSpec String
wTerm = wSelect id ["WS","SS"]

--- The WUI specification for a year.
wYear :: WuiSpec Int
wYear = wSelect show [(currentYear-4)..(currentYear+6)]

--- A WUI for the visibility of a module:
wVisible :: WuiSpec Bool
wVisible = wRadioBool [htxt "öffentlich sichtbar"]
                      [htxt "nur zur internen Bearbeitung"]

-----------------------------------------------------------------------------
