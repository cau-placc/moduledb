--- Some extensions to the basic MDB.

import MDB
import KeyDatabase

--- Gets the associated User entity for a given ModData entity.
getResponsibleUser :: ModData -> Transaction User
getResponsibleUser mUser = getUser (modDataUserResponsibleKey mUser)

--- Gets the associated User entity for a given MasterProgram entity.
getAdvisingUser :: MasterProgram -> Transaction User
getAdvisingUser mUser = getUser (masterProgramUserAdvisingKey mUser)

