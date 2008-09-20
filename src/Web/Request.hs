module Web.Request

where

import qualified Data.Map as Map
import Data.Maybe

data Request = Request { method :: String,
                         path :: String}

mkRequest :: [(String, String)] -> String -> Request
mkRequest env stdin = let envMap = Map.fromList env
                      in Request { 
                               method = fromJust $ Map.lookup "REQUEST_METHOD" envMap,
                               path = fromJust $ Map.lookup "PATH_INFO" envMap
                             }
