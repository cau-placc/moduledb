-- Reading data from UnivIS and storing it in a term file.

module Model.ReadUnivIS ( loadLectures )
 where

import Control.Search.SetFunctions
import Debug.Profile
import System.URL
import XML
import XCuery

import Model.ConfigMDB(storageDir)

-------------------------------------------------------------------------
-- Benchmark definitions:

getRunTime :: IO Int
getRunTime = getProcessInfos >>= \i -> return (maybe 0 id (lookup RunTime i))

runBench :: Data a => String -> (XmlExp -> IO a) -> IO ()
runBench xmlfile xmlaction = do
  t0 <- getRunTime
  xexp <- readXmlFile xmlfile
  t1 <- getRunTime
  putStrLn $ "XML read in " ++ show (t1-t0) ++ " msecs"
  xmlaction xexp
  t2 <- getRunTime
  putStrLn $ "XML action executed in " ++ show (t2-t1) ++ " msecs"

runBenchComp :: Data a => String -> (XmlExp -> a) -> IO a
runBenchComp xmlfile xmlcomp = do
  t0 <- getRunTime
  xexp <- readXmlFile xmlfile
  t1 <- getRunTime
  putStrLn $ "XML read in " ++ show (t1-t0) ++ " msecs"
  let r free
  doSolve (r =:= xmlcomp xexp)
  t2 <- getRunTime
  putStrLn $ "XML computation executed in " ++ show (t2-t1) ++ " msecs"
  return r

-------------------------------------------------------------------------
-- Matching in UnivIS person structure (just for testing):
findPerson :: String -> XmlExp -> (String,String,String)
findPerson name
  (deepXml' "Person"
     (with [xml "firstname" f, xml "lastname" n,
            deepXml "email" e]))
  | textOf n == name
  = (textOf f, textOf n, textOf e)

testPerson :: String -> IO ()
testPerson name = do
  xexp <- readXmlFile "univis_persons.xml"
  printValues (set2 findPerson name xexp)

testPersonBench :: String -> IO [(String,String,String)]
testPersonBench name =
  runBenchComp "univis_persons.xml"
    (\xe -> sortValues (set2 findPerson name xe))

-- find the IDs of each lecture in a given semester:
findLectureURL :: String -> XmlExp -> (String,String)
findLectureURL univissem
 (xml' "UnivIS"
  (with [xml' "Lecture"
     (with ([xml "id" nr, xml "name" name]))]))
  = (textOf name,
     "http://univis.uni-kiel.de/prg?search=lectures&id=" ++ textOf nr ++
     "&show=long&sem="++univissem)

-- Read all Informatik-lectures from UnivIS in a given semester
-- and store the corresponding Curry terms in a file
loadLectures :: (String,Int) -> IO (Either String String)
loadLectures sem = do
  let univissem = showSemUnivis sem
      termfile  = storageDir++"UnivisLectureURL_"++univissem++".terms"
  xmlstring <- getContentsOfUrl $
    "http://univis.uni-kiel.de/prg?search=lectures&department=080110000&sem="++
    univissem++"&show=xml"
  --writeFile ("univis_lectures_"++univissem++".xml") xmlstring
  let xexps = parseXmlString xmlstring
  if null xexps
   then return (Right "No XML document!")
   else
    if not (null (tail xexps))
    then return (Right "More than one XML document!")
    else do
     writeFile termfile
       (unlines (map show
                     (sortValues (set2 findLectureURL univissem (head xexps)))))
     return (Left $ "...and written into file " ++ termfile)

-- ...with benchmarking
loadLecturesBench :: (String,Int) -> IO [(String,String)]
loadLecturesBench sem =
  let univissem = showSemUnivis sem
   in runBenchComp ("univis_lectures_"++univissem++".xml")
                   (\xe -> sortValues (set2 findLectureURL univissem xe))

-- show semester in UnivIS string format:
showSemUnivis :: (String,Int) -> String
showSemUnivis (term,year) = show year ++ if term=="SS" then "s" else "w"

main :: IO ()
main = do
  loadLectures ("WS",2009) >>= either putStrLn error
  loadLectures ("SS",2010) >>= either putStrLn error
  loadLectures ("WS",2010) >>= either putStrLn error
  loadLectures ("SS",2011) >>= either putStrLn error
  loadLectures ("WS",2011) >>= either putStrLn error

-- > Next step: call UnivIS.addUnivisOfSemester to store the data in the MDB
