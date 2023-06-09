--------------------------------------------------------------------------
--- This module defines operations to support the handling of routes
--- to controllers.
--------------------------------------------------------------------------

module System.Routes(
  getControllerReference,
  getRouteMenus
) where

import Data.List ( isPrefixOf, partition )

import HTML.Base
import HTML.Styles.Bootstrap4 ( hrefNav )

import Config.RoutesData

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
        Prefix pre _ -> if (url == pre)
                        then Just fktref
                        else findControllerReference restroutes
        Matcher fkt  -> if (fkt url)
                        then Just fktref
                        else findControllerReference restroutes
        Always       -> Just fktref
    findControllerReference [] = Nothing -- no controller found for url

--- Generates the references for all route entries put on the top of
--- each page. As a default, all routes specified with URL matcher
--- Exact in the module RouteData are taken as menu entries.
--- The first component contain the "new" references and
--- the second component the remaining references.
getRouteMenus :: IO ([BaseHtml],[BaseHtml])
getRouteMenus = do
  routes <- getRoutes
  let links = getLinks routes
      (newlinks,otherlinks) = partition (isNewLink . fst) links
  return $ (map snd newlinks,
            map snd otherlinks)
 where
   isNewLink s = take 3 s == "new" || "/new" `isPrefixOf` snd (break (=='/') s)

   getLinks :: [Route] -> [(String,BaseHtml)]
   getLinks ((name, matcher, _):restroutes) =
     case matcher of
       Exact string -> if string `elem` ["main","login"]
                         then getLinks restroutes
                         else (string, href ("?" ++ string) [htxt name])
                               : getLinks restroutes
       Prefix s1 s2 -> if (s1,s2) `elem` [("Category","list"),
                                          ("ModData","list"),
                                          ("ModInst","list"),
                                          ("MasterProgram","list"),
                                          ("AdvisorStudyProgram","list"),
                                          ("StudyProgram","list"),
                                          ("StudentCourse","list"),
                                          ("search","main")]
                       then getLinks restroutes
                       else let url = s1++"/"++s2
                             in (url, href ("?"++url) [htxt name])
                                  : getLinks restroutes
       _ -> getLinks restroutes
   getLinks [] = []
