-------------------------------------------------------------------------------
-- Auxiliaries for module management
-------------------------------------------------------------------------------

module Helpers(LogEvent(..),logEvent,
               masterProgsLatexFile,
               modInfoLatexFile,modTableLatexFile,semTableLatexFile,
               shortModInfoLatexFile,
               ehref,
               showDigit2,showDiv10, formatPresence,
               hrefs2markdown,
               docText2html, docText2latex, quoteUnknownLatexCmd,
               showSemester, nextSemester, prevSemester, leqSemester,
               semesterSelection, lowerSemesterSelection, upperSemesterSelection,
               currentUpperSemester,
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
docText2html = showHtmlExps . markdownText2HTML  . latex2html

-- translate string containing some standard latex markups into HTML:
latex2html [] = []
latex2html (c:cs) | c=='\\' = tryTrans c cs slashtrans
                  | c=='"'  = tryTrans c cs apotrans
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
         
-- translate a document text (containing some standard latex markups
-- as well as markdown markups) into LaTeX:
docText2latex :: String -> String
docText2latex =
  htmlLists2latexLists . markdownText2LaTeXWithFormat html2latex . latex2html

html2latex =
  escapeLaTeXSpecials . htmlSpecialChars2tex . translateMDSpecials2LaTeX
 where
  escapeLaTeXSpecials s = case s of
    [] -> []
    ('#':cs) -> "\\#" ++ escapeLaTeXSpecials cs
    ('%':cs) -> "\\%" ++ escapeLaTeXSpecials cs
    ('&':cs) -> "\\&" ++ escapeLaTeXSpecials cs
    ('\\':'#':cs) -> "\\#" ++ escapeLaTeXSpecials cs
    ('\\':'%':cs) -> "\\%" ++ escapeLaTeXSpecials cs
    ('\\':'&':cs) -> "\\&" ++ escapeLaTeXSpecials cs
    ('\\':'\\':cs) -> "\\\\" ++ escapeLaTeXSpecials cs
    ('\\':cs) -> "\\" ++ escapeLaTeXSpecials cs
    (c:cs) -> c : escapeLaTeXSpecials cs

  -- by removing the backslash
  translateMDSpecials2LaTeX :: String -> String
  translateMDSpecials2LaTeX s = case s of
    []          -> []
    ('\\':'\\':cs) -> '\\' : '\\' : translateMDSpecials2LaTeX cs
                      --"{\\tt\\char92}" ++ translateMDSpecials2LaTeX cs
    ('\\':'_':cs) -> '\\' : '_' : translateMDSpecials2LaTeX cs
    ('\\':c:cs) -> if c `elem` mdEscapeChars
                   then c : translateMDSpecials2LaTeX cs
                   else '\\' : translateMDSpecials2LaTeX (c:cs)
    (c:cs)      -> c : translateMDSpecials2LaTeX cs

  mdEscapeChars =
    ['\\','`','*','_','{','}','[',']','(',')','#','+','-','.',' ','!']

-- translate HTML markups for lists in a string into latex equivalents:
htmlLists2latexLists [] = []
htmlLists2latexLists (c:cs) | c=='<' = tryTrans c cs htmltrans
                  | otherwise = c : htmlLists2latexLists cs
 where
  tryTrans x xs [] = x : htmlLists2latexLists xs
  tryTrans x xs ((hmacro,lmacro) : ms) = let l = length hmacro in
    if take l xs == hmacro then lmacro ++ htmlLists2latexLists (drop l xs)
                           else tryTrans x xs ms

  htmltrans = [("ul>","\\begin{itemize}"),("/ul>","\\end{itemize}"),
               ("ol>","\\begin{enumerate}"),("/ol>","\\end{enumerate}"),
               ("li>","\\item{}"),("/li>","")]
         
-----------------------------------------------------------------------------
-- Transform a latex string into a latex string where all latex
-- commands that are not explicitly allowed are quoted, e.g.,
-- "\input{/etc/passwd}" is translated into
-- "{\char92}{/etc/passwd}"
quoteUnknownLatexCmd [] = []
quoteUnknownLatexCmd (c:cs) | c=='\\'   = tryQuote cs allowedLatexCommands
                            | otherwise = c : quoteUnknownLatexCmd cs
 where
  tryQuote xs [] = logUnknownLatex xs `seq`
                     "{\\char92}" ++ quoteUnknownLatexCmd xs
  tryQuote xs (cmd:cmds) =
    if cmd `isPrefixOf` xs
    then '\\' : cmd ++ quoteUnknownLatexCmd (drop (length cmd) xs)
    else tryQuote xs cmds

allowedLatexCommands =
  ["\\","%","&","\"","ss","begin","end","item",
   "module","descmain","descrest","importmodule",
   "ite","url","em","texttt","textbf","par","bf","href",
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

-- the upper semester in current period views:
currentUpperSemester = iterate nextSemester currentSemester !! 3

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
