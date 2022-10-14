-------------------------------------------------------------------------------
-- Auxiliaries for module management
-------------------------------------------------------------------------------

module System.Helpers
  ( returnT, (|>>), (|>>=), showTError, -- compatibilities
    Database.CDBI.Connection.DBAction,
    LogEvent(..),logEvent,
    masterProgsLatexFile,
    modInfoLatexFile,modTableLatexFile,semTableLatexFile,
    shortModInfoLatexFile,
    stripSpaces,
    showDigit2,showDiv10, formatPresence,
    hrefs2markdown,
    docText2html, docText2latex, escapeLaTeXSpecials,
    quoteUnknownLatexCmd,
    getCurrentSemester,
    showSemester, showLongSemester, showSemesterCode, readSemesterCode,
    nextSemester, prevSemester, leqSemester,
    semesterSelection, findSemesterSelection,
    imageNB, wTerm, wCurrentYear, wYear,
    wLargeString, wLargeRequiredString,
    wMediumString, wMediumRequiredString,
    largetextinputRendering, mediumtextinputRendering,
    shorttextinputRendering, renderWithFormControl )
  where

import Data.Char
import System.IO
import System.IOExts
import Data.List
import Numeric ( readNat )
--import ReadShowTerm
import System.Environment (getEnv, getHostname )
import Data.Time
import System.IO.Unsafe ( unsafePerformIO )
import HTML.WUI

import Database.CDBI.Connection
import HTML.Base
import Markdown

import ConfigMDB
import MDB
import System.Authentication

-----------------------------------------------------------------------------
-- Some auxiliary definition for compatibility with old 
-- Database.KeyDatabaseSQLite API
-- Will be removed in the future.

infixl 1 |>>, |>>=

showTError :: DBError -> String
showTError dberr = show dberr

returnT :: a -> DBAction a
returnT = return

(|>>) :: DBAction a -> DBAction b -> DBAction b
ta |>> tb = ta >+ tb

(|>>=) :: DBAction a -> (a -> DBAction b) -> DBAction b
ta |>>= tb = ta >+= tb

-----------------------------------------------------------------------------
-- Logging:

-- logfile:
logFile :: String
logFile = storageDir ++ "CHANGE.LOG"

-- The kind of events we want to log.
data LogEvent =
    NewModData  ModData  | UpdateModData  ModData  | DeleteModData  ModData
  | NewModDescr ModDescr | UpdateModDescr ModDescr | DeleteModDescr ModDescr
  | NewModInst  ModInst  | UpdateModInst  ModInst  | DeleteModInst  ModInst
  | NewPrerequisite ModData ModDataID | DeletePrerequisites ModData [ModDataID]
  | NewMasterProgram MasterProgram | UpdateMasterProgram MasterProgram
  | DeleteMasterProgram MasterProgram
  | NewMasterProgInfo MasterProgInfo | UpdateMasterProgInfo MasterProgInfo
  | DeleteMasterProgInfo MasterProgInfo
  | NewAdvisorStudyProgram AdvisorStudyProgram
  | UpdateAdvisorStudyProgram AdvisorStudyProgram
  | DeleteAdvisorStudyProgram AdvisorStudyProgram
 deriving Show

-- An information in the log file consists of info about the time,
-- the current user, the remote host, and a log event.
data LogInfo = LogInfo String String String LogEvent
 deriving Show

-- Adds an event with some info string to the global log file.
logEvent :: LogEvent -> IO ()
logEvent event = exclusiveIO (logFile ++ ".lock") $ do
  time  <- getLocalTime
  login <- getSessionLogin
  raddr <- getEnv "REMOTE_ADDR"
  rhost <- if null raddr then getHostname else getHostnameForIP raddr
  appendFile logFile
     (show (LogInfo (calendarTimeToString time) (maybe "???" id login)
                    (rhost ++ "/" ++ raddr) event) ++ "\n")

--- Get symbolic name of ip address:
getHostnameForIP :: String -> IO String
getHostnameForIP ipaddr = (flip catch) (\_ -> return "") $ do
  h <- connectToCommand $ "host " ++ ipaddr
  b <- hIsEOF h
  if b then return ""
       else hGetLine h >>= return . last . words

-------------------------------------------------------------------------------
-- name of LaTeX include with infos for all modules:
modInfoLatexFile :: String
modInfoLatexFile = "include_modinfo.tex"

-- name of LaTeX include with short info for all modules:
shortModInfoLatexFile :: String
shortModInfoLatexFile = "include_shortinfo.tex"

-- name of LaTeX include which contains the catalogue table:
modTableLatexFile :: String
modTableLatexFile = "include_moduletable.tex"

-- name of LaTeX include which contains the semester table:
semTableLatexFile :: String
semTableLatexFile = "include_semestertable.tex"

-- name of LaTeX include which contains all master programs:
masterProgsLatexFile :: String
masterProgsLatexFile = "include_mscprograms.tex"

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
             then "<" ++ w ++ ">"
             else w

-----------------------------------------------------------------------------
-- translate a document text (containing some standard latex markups
-- as well as markdown markups) into HTML:
docText2html :: String -> String
docText2html = showBaseHtmls . markdownText2HTML  . latex2MD

-- Translate a string containing some standard latex markups into
-- markdown syntax (i.e., also HTML markups with UTF-8 encoding):
latex2MD :: String -> String
latex2MD [] = []
latex2MD (c:cs) | c=='\\' = tryTrans cs slashtrans
                | c=='{'  = '\\' : '{' : latex2MD cs
                | c=='}'  = '\\' : '}' : latex2MD cs
                | otherwise = c : latex2MD cs
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
escapeLaTeXSpecials = concatMap escChar
 where
  escChar c | c=='^'    = "{\\tt\\char94}"
            | c=='~'    = "{\\tt\\char126}"
            | c=='\\'   = "{\\textbackslash}"
            | c=='<'    = "{\\tt\\char60}"
            | c=='>'    = "{\\tt\\char62}"
            | c=='_'    = "\\_"
            | c=='#'    = "\\#"
            | c=='$'    = "\\$"
            | c=='%'    = "\\%"
            | c=='{'    = "\\{"
            | c=='}'    = "\\}"
            | c=='&'    = "\\&"
            | otherwise = [c]


-----------------------------------------------------------------------------
-- Transform a latex string into a latex string where all latex
-- commands that are not explicitly allowed are quoted, e.g.,
-- "\input{/etc/passwd}" is translated into
-- "{\textbackslash}input{/etc/passwd}"
quoteUnknownLatexCmd :: String -> String
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

allowedLatexCommands :: [String]
allowedLatexCommands =
  ["\\","_","#","$","%","{","}","&","\"","ss","begin","end","item",
   "textbackslash",
   "module","descmain","descrest","importmodule",
   "ite","url","em","texttt","textbf","par","bf","href","mbox",
   "leftarrow","rightarrow"]

-- logging for development:
logUnknownLatex :: String -> ()
logUnknownLatex cmd = unsafePerformIO $
  appendFile (storageDir ++ "LATEX.LOG") ('\\':take 20 cmd ++ "\n")

-----------------------------------------------------------------------------
-- Semester/Year management:

--- Gets the current semester from the current date.
getCurrentSemester :: IO (String,Int)
getCurrentSemester = do
  ltime <- getLocalTime
  return $ monthYearToSemester (ctMonth ltime) (ctYear ltime)
 where
  monthYearToSemester mt yr | mt>=4 && mt<=9 = ("SS",yr)
                            | mt>=10         = ("WS",yr)
                            | otherwise      = ("WS",yr-1)
                            
-- show a semester:
showSemester :: (String,Int) -> String
showSemester (sem,year) =
  let yr2 = year `mod` 100
   in if sem=="SS" then "SS" ++ showDigit2 yr2
                   else "WS" ++ showDigit2 yr2 ++ "/" ++ showDigit2 (yr2+1)

-- show a semester in long format:
showLongSemester :: (String,Int) -> String
showLongSemester (sem,year) =
  if sem=="SS"
    then "Sommersemester 20" ++ showDigit2 yr2
    else "Wintersemester 20" ++ showDigit2 yr2 ++ "/20" ++ showDigit2 (yr2+1)
 where yr2 = year `mod` 100

-- show a semester as a short code, i.e., in the form "ss13" or "ws14":
showSemesterCode :: (String,Int) -> String
showSemesterCode (sem,year) =
  let yr2 = year `mod` 100
   in (if sem=="SS" then "ss" else "ws") ++ showDigit2 yr2

-- read a string produced by 'showSemesterCode'.
readSemesterCode :: String -> (String,Int)
readSemesterCode s =
  let sem  = if take 2 s == "ss" then "SS" else "WS"
      year = case readNat (drop 2 s) of [(n,"")] -> n
                                        _        -> 0
  in (sem, 2000+year)

-- compute following semester:
nextSemester :: (String,Int) -> (String,Int)
nextSemester (sem,year) = if sem=="SS" then ("WS",year) else ("SS",year+1)

-- compute previous semester:
prevSemester :: (String,Int) -> (String,Int)
prevSemester (sem,year) = if sem=="WS" then ("SS",year) else ("WS",year-1)

-- compare semesters:
leqSemester :: (String,Int) -> (String,Int) -> Bool
leqSemester (s1,y1) (s2,y2) = y1 < y2 || (y1 == y2 && (s1 == s2 || s1 == "SS"))

-- A list of semesters to select in some WUIs where the current semester
-- is given:
semesterSelection :: (String,Int) -> [(String,Int)]
semesterSelection cursem =
  take 14 (iterate nextSemester (iterate prevSemester cursem !! 6))

-- Find the index of a given semester in the semesterSelection list
-- where the current semester is given as the first argument:
findSemesterSelection :: (String,Int) -> (String,Int) -> Int
findSemesterSelection cursem sem =
  maybe (error $ "Helpers.findSemesterSelection: semester " ++
                 showSemester sem ++ " not found!")
        id
        (findIndex (==sem) (semesterSelection cursem))

-----------------------------------------------------------------------------
--- An image without borders.
imageNB :: HTML h => String -> String -> h
imageNB r a = image r a `addAttr` ("border","0")


--- The WUI specification for a term.
wTerm :: WuiSpec String
wTerm = wSelect id ["WS","SS"]
         `withRendering` shorttextinputRendering

--- The WUI specification for an almost current year.
wCurrentYear :: Int -> WuiSpec Int
wCurrentYear curyear = wSelect show [(curyear-4) .. (curyear+6)]
                         `withRendering` shorttextinputRendering

--- The WUI specification for an arbitrary year.
wYear :: WuiSpec Int
wYear = wInt `withRendering` shorttextinputRendering

--- A WUI for a large input text line
wLargeString :: WuiSpec String
wLargeString = wString `withRendering` renderWithFormControl

--- A WUI for a large required input text line
wLargeRequiredString :: WuiSpec String
wLargeRequiredString = wRequiredString `withRendering` renderWithFormControl

--- A WUI for a medium input text line
wMediumString :: WuiSpec String
wMediumString = wStringSize 70 `withRendering` mediumtextinputRendering

--- A WUI for a medium required input text line
wMediumRequiredString :: WuiSpec String
wMediumRequiredString = wRequiredStringSize 70
                         `withRendering` mediumtextinputRendering

largetextinputRendering :: HTML h => [h] -> h
largetextinputRendering = inline . map (`addClass` "largetextinput")

mediumtextinputRendering :: HTML h => [h] -> h
mediumtextinputRendering = inline . map (`addClass` "mediumtextinput")

shorttextinputRendering :: HTML h => [h] -> h
shorttextinputRendering = inline . map (`addClass` "shorttextinput")

renderWithFormControl :: HTML h => [h] -> h
renderWithFormControl = inline . map (`addClass` "form-control")

-----------------------------------------------------------------------------
