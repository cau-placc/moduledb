-- Operations to connect UnivIS data to module database
module Model.UnivIS where

import Model.ConfigMDB
import Model.MDB
import System.Helpers
import Model.ReadUnivIS

-- Initialize Univis DB:
initUnivisDB :: IO String
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
  allmcodes <- runQ (fmap (map modDataCode) queryAllModDatas)
  infos <- readFile fname >>= return . map read . lines
  return $ process allmcodes infos
 where
  process mcodes info =
    map (\ (t,url) -> (stripSpaces (fst (break (==':') t)), term, year, url))
        (filter hasModCode info)
   where
     hasModCode (title,_) =
       let (tc,_) = break (==':') title
        in stripSpaces tc `elem` mcodes

  -- show semester in UnivIS string format:
  showSemUnivis = show year ++ if term=="SS" then "s" else "w"

-- delete all infos for some semester:
deleteUnivisOfSemester :: (String,Int) -> DBAction ()
deleteUnivisOfSemester sem =
  queryCondUnivisInfo (\u -> (univisInfoTerm u,univisInfoYear u) == sem)
  >>= mapM_ deleteUnivisInfo


-- add infos from UnivIS term file for some semester and delete
-- existing entries for the same semester
addUnivisOfSemester :: (String,Int) -> IO String
addUnivisOfSemester sem = do
  uisdata <- processUnivisFile sem
  runJustT $ do deleteUnivisOfSemester sem
                mapM_  (\ (c,t,y,u) -> newUnivisInfo c t y u) uisdata
  return $ "UnivIS infos for semester " ++ showSemester sem ++ " loaded: " ++
           show (length uisdata) ++ " UnivIS infos added."


----------------------------------------------------------------
-- Main operation: read UnivIS data and store it in the database:
readAndStoreUnivisOfSemester :: (String,Int) -> IO String
readAndStoreUnivisOfSemester sem =
  loadLectures sem >>= either (\_ -> addUnivisOfSemester sem) return

