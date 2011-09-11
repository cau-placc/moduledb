-- Operations to connect UnivIS data to module database

import ConfigMDB
import MDB
import ReadShowTerm
import KeyDatabase
import Helpers
import ReadUnivIS

-- Initialize Univis DB:
initUnivisDB = do
  addUnivisOfSemester ("WS",2009)
  addUnivisOfSemester ("SS",2010)
  addUnivisOfSemester ("WS",2010)
  addUnivisOfSemester ("SS",2011)
  addUnivisOfSemester ("WS",2011)

-- Parse a file containing the data extracted from Univis
processUnivisFile :: (String,Int) -> IO [(String,String,Int,String)]
processUnivisFile (term,year) = do
  let fname = storageDir++"UnivisLectureURL_"++showSemUnivis++".terms"
  allmcodes <- runQ (transformQ (map modDataCode) queryAllModDatas)
  infos <- readQTermListFile fname
  return $ process allmcodes infos
 where
  process mcodes info =
    map (\ (t,url) -> (fst (break (==':') t), term, year, url))
        (filter hasModCode info)
   where
     hasModCode (title,_) =
       let (tc,_) = break (==':') title
        in tc `elem` mcodes

  -- show semester in UnivIS string format:
  showSemUnivis = show year ++ if term=="SS" then "s" else "w"

-- delete all infos for some semester:
deleteUnivisOfSemester :: (String,Int) -> Transaction ()
deleteUnivisOfSemester sem =
  getDB (queryCondUnivisInfo (\u -> (univisInfoTerm u,univisInfoYear u) == sem))
  |>>= mapT_ deleteUnivisInfo


-- add infos from UnivIS term file for some semester and delete
-- existing entries for the same semester
addUnivisOfSemester :: (String,Int) -> IO String
addUnivisOfSemester sem = do
  uisdata <- processUnivisFile sem
  runJustT (deleteUnivisOfSemester sem |>>
            mapT_  (\ (c,t,y,u) -> newUnivisInfo c t y u) uisdata)
  return $ "Univis infos for semester " ++ showSemester sem ++ " added."


----------------------------------------------------------------
-- Main operation: read UnivIS data and store it in the database:
readAndStoreUnivisOfSemester :: (String,Int) -> IO String
readAndStoreUnivisOfSemester sem =
  loadLectures sem >>= either (\_ -> addUnivisOfSemester sem) return

