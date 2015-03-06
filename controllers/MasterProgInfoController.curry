module MasterProgInfoController (
 editMasterProgInfoController,
 deleteMasterProgInfoController, listMasterProgInfoController,
 progModsOfMasterProgInfo, reasonableMasterProgInfo
 ) where

import Spicey
import KeyDatabase
import HTML
import Time
import MDB
import MasterProgInfoView
import Maybe
import Authorization
import AuthorizedControllers
import UserProcesses
import Helpers
import List
import ReadShowTerm

--- Shows a form to edit the given MasterProgInfo entity.
editMasterProgInfoController :: (String,Int) -> Controller
   -> MasterProgInfo -> Controller
editMasterProgInfoController semyear cntcontroller masterProgInfoToEdit =
  checkAuthorization
   (masterProgInfoOperationAllowed (UpdateEntity masterProgInfoToEdit)) $ do
    modinsts <- runJustT (getMasterModInstInSemesters semyear 3)
    return (editMasterProgInfoView masterProgInfoToEdit modinsts
                             (updateMasterProgInfoController cntcontroller))

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
                                                         (showQTerm exprogmods)
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
  either (\ (cmt,exall) ->
              logEvent (UpdateMasterProgInfo mpinfo) >>
              (if exall
                then if null cmt then done else setPageMessage cmt
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
   (masterProgInfoOperationAllowed (DeleteEntity masterProgInfo)) $
   (do transResult <- runT (deleteMasterProgInfo masterProgInfo)
       either (\ _ -> listMasterProgInfoController)
        (\ error -> displayError (showTError error)) transResult)

--- Lists all MasterProgInfo entities with buttons to show, delete,
--- or edit an entity.
listMasterProgInfoController :: Controller
listMasterProgInfoController =
  checkAuthorization (masterProgInfoOperationAllowed ListEntities) $
   (do masterProgInfos <- runQ queryAllMasterProgInfos
       return
        (listMasterProgInfoView masterProgInfos showMasterProgInfoController
          (editMasterProgInfoController ("",0) listMasterProgInfoController)
          deleteMasterProgInfoController))

--- Shows a MasterProgInfo entity.
showMasterProgInfoController :: MasterProgInfo -> Controller
showMasterProgInfoController masterProgInfo =
  checkAuthorization
   (masterProgInfoOperationAllowed (ShowEntity masterProgInfo)) $
   (do programInfoMasterProgram <- runJustT
                                    (getProgramInfoMasterProgram
                                      masterProgInfo)
       return
        (showMasterProgInfoView masterProgInfo programInfoMasterProgram
          listMasterProgInfoController))

--- Gets the associated MasterProgram entity for a given MasterProgInfo entity.
getProgramInfoMasterProgram :: MasterProgInfo -> Transaction MasterProgram
getProgramInfoMasterProgram mMasterProgram =
  getMasterProgram (masterProgInfoMasterProgramProgramInfoKey mMasterProgram)



--- Get module instances of next n semesters from a given one:
queryModInstInSemesters :: (String,Int) -> Int -> Query [ModInst]
queryModInstInSemesters semyear n =
  let nextsems = take n (iterate nextSemester semyear)
   in queryCondModInst (\mi -> (modInstTerm mi,modInstYear mi) `elem` nextsems)

--- Get module instances and their categories
--- of next n semesters from a given one:
getModInstCatsInSemesters :: (String,Int) -> Int
                          -> Transaction [(ModInst,[Category])]
getModInstCatsInSemesters semyear n =
  getDB (queryModInstInSemesters semyear n) |>>= \mis ->
  mapT (\mdk -> getModDataKeyCategorys mdk |>>= \cats -> returnT (mdk,cats))
       (nub (map modInstModDataModuleInstancesKey mis)) |>>= \mdkcats ->
  returnT
   (map (\mi -> (mi,fromJust
                  (lookup (modInstModDataModuleInstancesKey mi) mdkcats))) mis)

--- Get module instances (and their categories) belonging to given
--- list of categories (specified by their CatKeys)
--- of next n semesters from a given one:
getCategoryModInstInSemesters :: (String,Int) -> Int -> [String]
                          -> Transaction [(ModInst,[Category])]
getCategoryModInstInSemesters semyear n catkeys =
  getModInstCatsInSemesters semyear n |>>=
  returnT .
    (filter (\ (_,cats) -> any (\c -> categoryCatKey c `elem` catkeys) cats))

--- Get master module instances (and their module data and categories)
--- of next n semesters from a given one:
getMasterModInstInSemesters :: (String,Int) -> Int
                          -> Transaction [(ModInst,ModData,[Category])]
getMasterModInstInSemesters semyear n =
  getCategoryModInstInSemesters semyear n ["IG","TG","IS","MV"] |>>=
  mapT (\ (mi,cats) -> getModData (modInstModDataModuleInstancesKey mi)
                       |>>= \md -> returnT (mi,md,cats))


--- Reads the string-encoded module instances of a master program
--- and return them as a list of the form [(catkey,mand.?,moddatakey,term,year)]
progModsOfMasterProgInfo :: MasterProgInfo -> [(String,Bool,String,String,Int)]
progModsOfMasterProgInfo mpi = readQTerm (masterProgInfoProgModules mpi)

--- Deletes non-existing module instances in a list.
cleanProgMods :: [(String,Bool,String,String,Int)]
              -> Transaction [(String,Bool,String,String,Int)]
cleanProgMods pms =
  mapT (\ (ck,p,mdk,term,year) ->
         getDB (queryInstancesOfMod (fromJust(readModDataKey mdk))) |>>= \mis ->
         if (term,year) `elem` (map modInstSemester mis)
         then returnT [(ck,p,mdk,term,year)]
         else returnT [])
       pms |>>= returnT . concat

--- Checks whether there are enough modules for each category of a
--- master program: at least 12 points in IG/TG/IS, 8 points in MV,
--- and at least 48 points in all categories IG/TG/IS/MV.
--- The returned string is empty if there are enough modules, otherwise
--- it contains an explanation about the missing points.
reasonableMasterProgInfo :: MasterProgInfo -> Transaction String
reasonableMasterProgInfo mpi =
  mapT getMCodeForInfo (progModsOfMasterProgInfo mpi) |>>= \catmods ->
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
