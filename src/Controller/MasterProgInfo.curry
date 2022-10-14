module Controller.MasterProgInfo (
 deleteMasterProgInfoController, listMasterProgInfoController,
 progModsOfMasterProgInfo, reasonableMasterProgInfo
 ) where

import Data.List
import Data.Maybe
import System.Spicey
import HTML.Base
import MDB
import MDBExts
import View.MasterProgInfo
import System.Authorization
import System.AuthorizedActions
import Config.UserProcesses
import System.Helpers

-- --- Shows a form to edit the given MasterProgInfo entity.
-- editMasterProgInfoController :: (String,Int) -> Controller
--    -> MasterProgInfo -> Controller
-- editMasterProgInfoController semyear cntcontroller masterProgInfoToEdit =
--   checkAuthorization
--    (masterProgInfoOperationAllowed (UpdateEntity masterProgInfoToEdit)) $ \_ -> do
--     csem  <- getCurrentSemester
--     modinsts <- runJustT (getMasterModInstInSemesters semyear 3)
--     return (editMasterProgInfoView masterProgInfoToEdit csem modinsts
--                              (updateMasterProgInfoController cntcontroller))

--- Persists modifications of a given MasterProgInfo entity to the
--- database depending on the Boolean argument. If the Boolean argument
--- is False, nothing is changed.
updateMasterProgInfoController :: Controller -> Bool -> MasterProgInfo
                               -> Controller
updateMasterProgInfoController cntcontroller False _ = cntcontroller
updateMasterProgInfoController cntcontroller True mpinfo =
  runT (let progmods = progModsOfMasterProgInfo mpinfo
         in cleanProgMods progmods |>>= \exprogmods ->
            let newmpinfo = setMasterProgInfoProgModules mpinfo
                                                         (show exprogmods)
             in updateMasterProgInfo newmpinfo |>>
                reasonableMasterProgInfo newmpinfo |>>= \reas ->
                (if null reas
                 then returnT ""
                 else
                   getMasterProgram
                    (masterProgInfoMasterProgramProgramInfoKey newmpinfo) |>>=
                    \mprog ->
                      if masterProgramVisible mprog
                      then updateMasterProgram
                             (setMasterProgramVisible mprog False) |>>
                           returnT ("Masterprogramm nicht sichtbar, denn "++reas)
                      else returnT "") |>>= \cmt ->
            returnT (cmt,progmods==exprogmods)) >>=
  flip either (\ (cmt,exall) ->
              logEvent (UpdateMasterProgInfo mpinfo) >>
              (if exall
                then if null cmt then return () else setPageMessage cmt
                else setPageMessage notExistMsg) >>
              nextInProcessOr cntcontroller Nothing)
         (\ error -> displayError (showTError error))
 where
  notExistMsg = "Einige ausgewählte Modulinstanzen inzwischen sind nicht "++
                "mehr vorhanden und wurden daher gelöscht!"

--- Deletes a given MasterProgInfo entity (depending on the Boolean
--- argument) and proceeds with the list controller.
deleteMasterProgInfoController :: MasterProgInfo -> Bool -> Controller
deleteMasterProgInfoController _ False = listMasterProgInfoController
deleteMasterProgInfoController masterProgInfo True =
  checkAuthorization
   (masterProgInfoOperationAllowed (DeleteEntity masterProgInfo)) $ \_ ->
   (do transResult <- runT (deleteMasterProgInfo masterProgInfo)
       flip either (\ _ -> listMasterProgInfoController)
        (\ error -> displayError (showTError error)) transResult)

--- Lists all MasterProgInfo entities with buttons to show, delete,
--- or edit an entity.
listMasterProgInfoController :: Controller
listMasterProgInfoController =
  checkAuthorization (masterProgInfoOperationAllowed ListEntities) $ \_ ->
   (do masterProgInfos <- runQ queryAllMasterProgInfos
       return (listMasterProgInfoView masterProgInfos))

--- Shows a MasterProgInfo entity.
showMasterProgInfoController :: MasterProgInfo -> Controller
showMasterProgInfoController masterProgInfo =
  checkAuthorization
   (masterProgInfoOperationAllowed (ShowEntity masterProgInfo)) $ \_ -> do
     programInfoMasterProgram <- runJustT
                                   (getProgramInfoMasterProgram masterProgInfo)
     return (showMasterProgInfoView masterProgInfo programInfoMasterProgram)

--- Gets the associated MasterProgram entity for a given MasterProgInfo entity.
getProgramInfoMasterProgram :: MasterProgInfo -> DBAction MasterProgram
getProgramInfoMasterProgram mMasterProgram =
  getMasterProgram (masterProgInfoMasterProgramProgramInfoKey mMasterProgram)


--- Get module instances and their categories
--- of next n semesters from a given one:
getModInstCatsInSemesters :: (String,Int) -> Int
                          -> DBAction [(ModInst,[Category])]
getModInstCatsInSemesters semyear n = do
  mis <- queryModInstInSemesters semyear n
  mdkcats <- mapM (\mdk -> do cats <- getModDataKeyCategories mdk
                              return (mdk,cats))
                  (nub (map modInstModDataModuleInstancesKey mis))
  return
   (map (\mi -> (mi,fromJust
                      (lookup (modInstModDataModuleInstancesKey mi) mdkcats)))
        mis)

--- Get module instances (and their categories) belonging to given
--- list of categories (specified by their ShortNames)
--- of next n semesters from a given one:
getCategoryModInstInSemesters :: (String,Int) -> Int -> [String]
                          -> DBAction [(ModInst,[Category])]
getCategoryModInstInSemesters semyear n catkeys =
  getModInstCatsInSemesters semyear n |>>=
  returnT .
    (filter (\ (_,cats) -> any (\c -> categoryShortName c `elem` catkeys) cats))

--- Get master module instances (and their module data and categories)
--- of next n semesters from a given one:
getMasterModInstInSemesters :: (String,Int) -> Int
                          -> DBAction [(ModInst,ModData,[Category])]
getMasterModInstInSemesters semyear n =
  getCategoryModInstInSemesters semyear n ["IG","TG","IS","MV"] |>>=
  mapM (\ (mi,cats) -> getModData (modInstModDataModuleInstancesKey mi)
                       |>>= \md -> returnT (mi,md,cats))


--- Reads the string-encoded module instances of a master program
--- and return them as a list of the form [(catkey,mand.?,moddatakey,term,year)]
progModsOfMasterProgInfo :: MasterProgInfo -> [(String,Bool,String,String,Int)]
progModsOfMasterProgInfo mpi = read (masterProgInfoProgModules mpi)

--- Deletes non-existing module instances in a list.
cleanProgMods :: [(String,Bool,String,String,Int)]
              -> DBAction [(String,Bool,String,String,Int)]
cleanProgMods pms =
  mapM (\ (ck,p,mdk,term,year) ->
         queryInstancesOfMod (fromJust(readModDataKey mdk)) |>>= \mis ->
         if (term,year) `elem` (map modInstSemester mis)
         then returnT [(ck,p,mdk,term,year)]
         else returnT [])
       pms |>>= returnT . concat

--- Checks whether there are enough modules for each category of a
--- master program: at least 12 points in IG/TG/IS, 8 points in MV,
--- and at least 48 points in all categories IG/TG/IS/MV.
--- The returned string is empty if there are enough modules, otherwise
--- it contains an explanation about the missing points.
reasonableMasterProgInfo :: MasterProgInfo -> DBAction String
reasonableMasterProgInfo mpi =
  mapM getMCodeForInfo (progModsOfMasterProgInfo mpi) |>>= \catmods ->
  let catpoints = map (\ck -> foldr (+) 0 (map (modDataECTS . snd)
                                          (filter (\ (c,_) -> c==ck) catmods)))
                      ["IG","TG","IS","MV"]
   in returnT (satisfied catpoints)
 where
  satisfied [ig,tg,is,mv]
    | ig<120 = msgArea "IG" ig 120
    | tg<120 = msgArea "TG" tg 120
    | is<120 = msgArea "IS" is 120
    | mv<80  = msgArea "MV" mv  80
    | ig+tg+is+mv<480 = msgAllAreas
    | otherwise = ""
   where
     msgArea ck curr atleast =
       "der Studienbereich "++ck++" hat noch zu wenig ECTS-Punkte ("++
       showDiv10 curr++" statt mindestens "++showDiv10 atleast++")."
     msgAllAreas =
       "die Studienbereiche haben zusammen weniger als 48 ECTS-Punkte."

  getMCodeForInfo (c,_,mk,_,_) =
    getModData (fromJust (readModDataKey mk)) |>>= \mod -> returnT (c,mod)
