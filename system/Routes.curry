--------------------------------------------------------------------------
--- This module defines operations to support the handling of routes
--- to controllers.
--------------------------------------------------------------------------

module Routes(
  getControllerReference,
  getRouteMenus
) where

import HTML
import RoutesData
import List

--generated in RoutesData
--type Route = (String, UrlMatch, ControllerReference)

--- Gets the reference of a controller corresponding to a given URL
--- according to the definition of all routes specified in
--- module RouteData.
getControllerReference :: String -> IO (Maybe ControllerReference)
getControllerReference url = getRoutes >>= return . findControllerReference
  where
    findControllerReference :: [Route] -> Maybe ControllerReference
    findControllerReference ((_, matcher, fktref):restroutes) =
      case matcher of
        Exact string -> if (url == string)
                        then Just fktref
                        else findControllerReference restroutes
        Matcher fkt  -> if (fkt url)
                        then Just fktref
                        else findControllerReference restroutes
        Always       -> Just fktref
    findControllerReference [] = Nothing -- no controller found for url

--- Generates the menu for all route entries put on the top of
--- each page. As a default, all routes specified with URL matcher
--- Exact in the module RouteData are taken as menu entries.
getRouteMenus :: IO (HtmlExp,HtmlExp)
getRouteMenus = do
  routes <- getRoutes
  let links = getLinks routes
      (newlinks,otherlinks) = partition (\l -> take 3 (fst l) == "new") links
  return $ (blockstyle "menu" [ulist (map snd newlinks)],
            blockstyle "menu" [ulist (map snd otherlinks)])
 where
   getLinks :: [Route] -> [(String,[HtmlExp])]
   getLinks ((name, matcher, _):restroutes) =
     case matcher of
       Exact string -> if string `elem`
                           ["main","search","listStudyProgram",
                            "listMasterProgram","newMasterProgram",
                            "login","listModData","listModInst"]
                       then getLinks restroutes
                       else (string,[(href ("?" ++ string) [htxt name])])
                             : getLinks restroutes
       _ -> getLinks restroutes
   getLinks [] = []
