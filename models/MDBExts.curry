--- Some extensions to the basic MDB.

import Helpers
import MDB
import KeyDatabase

--- Gets the associated User entity for a given ModData entity.
getResponsibleUser :: ModData -> Transaction User
getResponsibleUser mUser = getUser (modDataUserResponsibleKey mUser)

--- Gets the associated User entity for a given MasterProgram entity.
getAdvisingUser :: MasterProgram -> Transaction User
getAdvisingUser mprog = getUser (masterProgramUserAdvisingKey mprog)

--- Gets the associated User entity for a given AdvisorStudyProgram entity.
getStudyProgAdvisorUser :: AdvisorStudyProgram -> Transaction User
getStudyProgAdvisorUser = getUser . advisorStudyProgramUserStudyAdvisingKey

--- Query the categories of a given StudyProgram.
queryCategorysOfStudyProgram :: StudyProgramKey -> Query [Category]
queryCategorysOfStudyProgram sp =
  queryCondCategory (\c -> categoryStudyProgramProgramCategoriesKey c == sp)


-----------------------------------------------------------------------
--- Get module instances of next n semesters from a given one:
queryModInstInSemesters :: (String,Int) -> Int -> Query [ModInst]
queryModInstInSemesters semyear n =
  let nextsems = take n (iterate nextSemester semyear)
   in queryCondModInst (\mi -> (modInstTerm mi,modInstYear mi) `elem` nextsems)

--- Get module instances and their module data of next n semesters
--  from a given one that belong to a given category:
getCatModInstsInSemesters :: Category -> (String,Int) -> Int
                          -> Transaction [(ModInst,ModData)]
getCatModInstsInSemesters cat semyear n =
  getDB (queryModInstInSemesters semyear n) |>>=
  mapT (\mi -> getModDataKeyCategorys (modInstModDataModuleInstancesKey mi)
               |>>= \cats -> returnT (mi,cats)) |>>= \miscats ->
  mapT (\ mi -> getModData (modInstModDataModuleInstancesKey mi)
                           |>>= \md -> returnT (mi,md))
       (map fst (filter (\ (mi,cats) -> elem cat cats) miscats))
       
--- Get module data for a list of AdvisorModules:
getAdvisorModuleData :: [AdvisorModule]
                     -> Transaction [(AdvisorModule,ModInst,ModData)]
getAdvisorModuleData =
  mapT (\amod ->
         getModInst (advisorModuleModInstAdvisedProgramModuleInstancesKey amod)
         |>>= \mi -> getModData (modInstModDataModuleInstancesKey mi)
         |>>= \moddata -> returnT (amod,mi,moddata))
