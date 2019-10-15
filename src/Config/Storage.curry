------------------------------------------------------------------------------
--- Some configurations where data is stored.
------------------------------------------------------------------------------

module Config.Storage where

import FilePath        ( (</>) )

import ConfigMDB ( storageDir )

--- Prefix a file name with the directory where global form data
--- is stored during run time.
inDataDir :: String -> String
inDataDir filename = storageDir </> "sessiondata" </> filename
