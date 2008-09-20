module Web.Request (requestMethod, pathInfo, environment, mkRequest)

where

import qualified Data.Map as Map
import Data.Maybe

data Request = Request {
      environment :: Map.Map String String
    } deriving (Show, Read, Eq, Ord)

mkRequest :: [(String, String)] -> String -> Request
mkRequest env stdin = let envMap = Map.fromList env
                      in Request { 
                               environment = envMap
                             }

requestMethod request = fromJust $ Map.lookup "REQUEST_METHOD" $ environment request
pathInfo request = fromJust $ Map.lookup "PATH_INFO" $ environment request
