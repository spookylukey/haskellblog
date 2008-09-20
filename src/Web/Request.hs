module Web.Request (method, path, environment, mkRequest)

where

import qualified Data.Map as Map
import Data.Maybe

data Request = Request { method :: String
                       , path :: String
                       , environment :: Map.Map String String
                       }

mkRequest :: [(String, String)] -> String -> Request
mkRequest env stdin = let envMap = Map.fromList env
                      in Request { 
                               method = fromJust $ Map.lookup "REQUEST_METHOD" envMap
                             , path = fromJust $ Map.lookup "PATH_INFO" envMap
                             , environment = envMap
                             }
