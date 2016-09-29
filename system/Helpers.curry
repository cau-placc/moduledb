-------------------------------------------------------------------------------
-- Auxiliaries for module management
-------------------------------------------------------------------------------

module Helpers(LogEvent(..),logEvent,
               masterProgsLatexFile,
               modInfoLatexFile,modTableLatexFile,semTableLatexFile,
               shortModInfoLatexFile,
               ehref, stripSpaces,
               showDigit2,showDiv10, formatPresence,
               hrefs2markdown,
               docText2html, docText2latex, escapeLaTeXSpecials,
               quoteUnknownLatexCmd,
               showSemester, showLongSemester, showSemesterCode,
               nextSemester, prevSemester, leqSemester,
               semesterSelection, findSemesterSelection,
               lowerSemesterSelection, upperSemesterSelection,
               currentUpperSemester,
               imageNB, wTerm, wCurrentYear, wYear, wVisible,
               wLargeString, wLargeRequiredString,
               wMediumString, wMediumRequiredString,
               largetextinputRendering, mediumtextinputRendering,
               shorttextinputRendering, renderWithFormControl )
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
import Markdown
import Unsafe(unsafePerformIO)

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
  | NewAdvisorStudyProgram AdvisorStudyProgram
  | UpdateAdvisorStudyProgram AdvisorStudyProgram
  | DeleteAdvisorStudyProgram AdvisorStudyProgram

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
getHostnameForIP ipaddr = (flip catch) (\_ -> return "") $ do
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
-- strip pre- and post spaces in a string:
stripSpaces :: String -> String
stripSpaces = reverse . dropWhile isSpace . reverse . dropWhile isSpace

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
-- (delete 0 fields, replace P bei PUe if there are only 4 fields (old format)):
formatPresence :: String -> String
formatPresence ps =
  let xs = words ps
      oldformat = length xs == 4
   in unwords (map (p2pue oldformat) (filter (\s -> head s /= '0') xs))
 where
   p2pue oldformat s =
     if oldformat
       then case s of
              [d,'P'] -> [d,'P','Ü']
              _ -> s
       else s

-----------------------------------------------------------------------------
-- translate potential URLs in a string into markdown hyperrefs:
hrefs2markdown :: String -> String
hrefs2markdown s =
  let (sps,s1) = break (not . isSpace) s
      (w,s2)   = break isSpace s1
   in if w==""
      then s -- there is no word
      else sps ++ toHref w ++ hrefs2markdown s2
 where
  toHref w = if take 7 w == "http://" || take 8 w == "https://"
             then "<"++w++">"
             else w

-----------------------------------------------------------------------------
-- translate a document text (containing some standard latex markups
-- as well as markdown markups) into HTML:
docText2html :: String -> String
docText2html = showHtmlExps . markdownText2HTML  . latex2MD

-- translate string containing some standard latex markups into HTML:
latex2html [] = []
latex2html (c:cs) | c=='\\' = tryTrans c cs slashtrans
                  | c=='"'  = tryTrans c cs apotrans
                  | c=='{'  = '\\' : '{' : latex2html cs
                  | c=='}'  = '\\' : '}' : latex2html cs
                  | otherwise = htmlIsoUmlauts [c] ++ latex2html cs
 where
  tryTrans x xs [] = x : latex2html xs
  tryTrans x xs ((lmacro,hmacro) : ms) = let l = length lmacro in
    if take l xs == lmacro then hmacro ++ latex2html (drop l xs)
                           else tryTrans x xs ms

  slashtrans = [("begin{itemize}","<ul>"),("end{itemize}","</ul>"),
                ("begin{enumerate}","<ol>"),("end{enumerate}","</ol>"),
                ("item","<li>"),
                ("\"a","&auml;"),("\"o","&ouml;"),("\"u","&uuml;"),
                ("\"A","&Auml;"),("\"O","&Ouml;"),("\"U","&Uuml;"),
                ("ss{}","&szlig;"),("-",""),("\\","\n\n"),(" "," "),
                ("%","%")]

  apotrans = [("a","&auml;"),("o","&ouml;"),("u","&uuml;"),("s","&szlig;"),
              ("A","&Auml;"),("O","&Ouml;"),("U","&Uuml;"),
              ("\'","\""),("`","\"")]
         
-- Translate a string containing some standard latex markups into
-- markdown syntax (i.e., also HTML markupds):
latex2MD :: String -> String
latex2MD [] = []
latex2MD (c:cs) | c=='\\' = tryTrans cs slashtrans
                | c=='{'  = '\\' : '{' : latex2MD cs
                | c=='}'  = '\\' : '}' : latex2MD cs
                | otherwise = htmlIsoUmlauts [c] ++ latex2MD cs
 where
  tryTrans [] [] = "\\\\" -- quote backslash
  tryTrans (x:xs) [] = -- no translatable LaTeX element found:
    if x `elem` markdownEscapeChars
    then '\\' : x : latex2MD xs
    else '\\' : '\\' : x: latex2MD xs -- quote backslash
  tryTrans xs ((lmacro,hmacro) : ms) = let l = length lmacro in
    if take l xs == lmacro then hmacro ++ latex2MD (drop l xs)
                           else tryTrans xs ms

  slashtrans = [("begin{itemize}","<ul>"),("end{itemize}","</ul>"),
                ("begin{enumerate}","<ol>"),("end{enumerate}","</ol>"),
                ("item","<li>"),
                ("\"a","&auml;"),("\"o","&ouml;"),("\"u","&uuml;"),
                ("\"A","&Auml;"),("\"O","&Ouml;"),("\"U","&Uuml;"),
                ("ss{}","&szlig;"),("-",""),("\\","\n\n"),(" "," "),
                ("%","%")]
         
-- translate a document text (containing some standard latex markups
-- as well as markdown markups) into LaTeX:
docText2latex :: String -> String
docText2latex = markdownText2LaTeX . latex2MD

--- Escape all characters with a special meaning in LaTeX into quoted
--- LaTeX characters.
escapeLaTeXSpecials :: String -> String
escapeLaTeXSpecials [] = []
escapeLaTeXSpecials (c:cs)
  | c=='^'      = "{\\tt\\char94}" ++ escapeLaTeXSpecials cs
  | c=='~'      = "{\\tt\\char126}" ++ escapeLaTeXSpecials cs
  | c=='\\'     = "{\\textbackslash}" ++ escapeLaTeXSpecials cs
  | c=='<'      = "{\\tt\\char60}" ++ escapeLaTeXSpecials cs
  | c=='>'      = "{\\tt\\char62}" ++ escapeLaTeXSpecials cs
  | c=='_'      = "\\_" ++ escapeLaTeXSpecials cs
  | c=='#'      = "\\#" ++ escapeLaTeXSpecials cs
  | c=='$'      = "\\$" ++ escapeLaTeXSpecials cs
  | c=='%'      = "\\%" ++ escapeLaTeXSpecials cs
  | c=='{'      = "\\{" ++ escapeLaTeXSpecials cs
  | c=='}'      = "\\}" ++ escapeLaTeXSpecials cs
  | c=='&'      = "\\&" ++ escapeLaTeXSpecials cs
  | otherwise   = c : escapeLaTeXSpecials cs

         
-----------------------------------------------------------------------------
-- Transform a latex string into a latex string where all latex
-- commands that are not explicitly allowed are quoted, e.g.,
-- "\input{/etc/passwd}" is translated into
-- "{\textbackslash}input{/etc/passwd}"
quoteUnknownLatexCmd [] = []
quoteUnknownLatexCmd (c:cs) | c=='\\'   = tryQuote cs allowedLatexCommands
                            | otherwise = c : quoteUnknownLatexCmd cs
 where
  tryQuote xs [] = logUnknownLatex xs `seq`
                     "{\\textbackslash}" ++ quoteUnknownLatexCmd xs
  tryQuote xs (cmd:cmds) =
    if cmd `isPrefixOf` xs
    then '\\' : cmd ++ quoteUnknownLatexCmd (drop (length cmd) xs)
    else tryQuote xs cmds

allowedLatexCommands =
  ["\\","#","$","%","{","}","&","\"","ss","begin","end","item",
   "textbackslash",
   "module","descmain","descrest","importmodule",
   "ite","url","em","texttt","textbf","par","bf","href","mbox",
   "leftarrow","rightarrow"]

-- logging for development:
logUnknownLatex cmd =
 unsafePerformIO (appendFile (storageDir++"LATEX.LOG") ('\\':take 20 cmd++"\n"))

-----------------------------------------------------------------------------
-- Semester/Year management:

-- show a semester:
showSemester (sem,year) =
  let yr2 = year `mod` 100
   in if sem=="SS" then "SS"++showDigit2 yr2
                   else "WS"++showDigit2 yr2++"/"++showDigit2 (yr2+1)

-- show a semester in long format:
showLongSemester (sem,year) =
  let yr2 = year `mod` 100
   in if sem=="SS" then "Sommersemester 20"++showDigit2 yr2
                   else "Wintersemester 20"++showDigit2 yr2++"/20"++showDigit2 (yr2+1)

-- show a semester as a short code, i.e., in the form "ss13" or "ws14":
showSemesterCode (sem,year) =
  let yr2 = year `mod` 100
   in (if sem=="SS" then "ss" else "ws") ++ showDigit2 yr2

-- compute following semester:
nextSemester :: (String,Int) -> (String,Int)
nextSemester (sem,year) = if sem=="SS" then ("WS",year) else ("SS",year+1)

-- compute previous semester:
prevSemester :: (String,Int) -> (String,Int)
prevSemester (sem,year) = if sem=="WS" then ("SS",year) else ("WS",year-1)

-- compare semesters:
leqSemester :: (String,Int) -> (String,Int) -> Bool
leqSemester (s1,y1) (s2,y2) = y1 < y2 || (y1 == y2 && (s1 == s2 || s1 == "SS"))

-- a list of semesters to select in some WUIs
semesterSelection :: [(String,Int)]
semesterSelection =
  take 14 (iterate nextSemester (iterate prevSemester currentSemester !! 6))

-- find the index of a given semester in the semesterSelection list:
findSemesterSelection :: (String,Int) -> Int
findSemesterSelection sem =
  maybe (error $ "Helpers.findSemesterSelection: semester " ++
                 showSemester sem ++ " not found!")
        id
        (findIndex (==sem) semesterSelection)

-- preselected lower and upper bound index of a semester list:
lowerSemesterSelection :: Int
lowerSemesterSelection = findSemesterSelection currentSemester

upperSemesterSelection :: Int
upperSemesterSelection = lowerSemesterSelection + 3

-- the upper semester in current period views:
currentUpperSemester = iterate nextSemester currentSemester !! 3

-----------------------------------------------------------------------------
--- An image without borders.
imageNB :: String -> String -> HtmlExp
imageNB r a = image r a `addAttr` ("border","0")


--- The WUI specification for a term.
wTerm :: WuiSpec String
wTerm = wSelect id ["WS","SS"]
         `withRendering` shorttextinputRendering

--- The WUI specification for an almost current year.
wCurrentYear :: WuiSpec Int
wCurrentYear = wSelect show [(currentYear-4)..(currentYear+6)]
                 `withRendering` shorttextinputRendering

--- The WUI specification for an arbitrary year.
wYear :: WuiSpec Int
wYear = wInt `withRendering` shorttextinputRendering

--- A WUI for the visibility of a module:
wVisible :: WuiSpec Bool
wVisible = wRadioBool [htxt "öffentlich sichtbar", nbsp, nbsp]
                      [htxt "nur zur internen Bearbeitung"]

--- A WUI for a large input text line
wLargeString = wString `withRendering` renderWithFormControl

--- A WUI for a large required input text line
wLargeRequiredString = wRequiredString `withRendering` renderWithFormControl

--- A WUI for a medium input text line
wMediumString = wStringSize 70 `withRendering` mediumtextinputRendering

--- A WUI for a medium required input text line
wMediumRequiredString = wRequiredStringSize 70
                         `withRendering` mediumtextinputRendering

largetextinputRendering [s] = inline [s `addClass` "largetextinput"]

mediumtextinputRendering [s] = inline [s `addClass` "mediumtextinput"]

shorttextinputRendering [s] = inline [s `addClass` "shorttextinput"]

renderWithFormControl = inline . map (`addClass` "form-control")

-----------------------------------------------------------------------------
