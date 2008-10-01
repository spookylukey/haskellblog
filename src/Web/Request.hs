module Web.Request (
                    Request
                    -- * Components of Request
                   , requestMethod, pathInfo, environment
                    -- * Constructors for Request
                   , mkRequest, buildCGIRequest
                   )

where

import qualified Data.Map as Map
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe
import System.Environment (getEnvironment)
import System.IO (stdin)

data Request = Request {
      environment :: Map.Map String String
    , requestBody :: ByteString
    } deriving (Show, Read, Eq, Ord)

-- | Create a Request object
mkRequest :: [(String, String)] -- association list of environment variables
          -> ByteString -- lazy ByteString containing request body
          -> Request
mkRequest env body = let envMap = Map.fromList env
                     in Request { 
                              environment = envMap
                            , requestBody = body
                            }

-- | Returns the request method (GET, POST etc) of the request
requestMethod :: Request -> String
requestMethod request = fromJust $ Map.lookup "REQUEST_METHOD" $ environment request

-- | Returns the path info of the request, with leading forward slash removed.
pathInfo request = let pi = Map.lookup "PATH_INFO" $ environment request
                   in case pi of
                        Nothing -> ""
                        Just ('/':rest) -> rest
                        Just path -> path


-- | Creates a Request object according to the CGI protocol
buildCGIRequest :: IO Request
buildCGIRequest = do
  env <- getEnvironment
  body <- BS.hGetContents stdin
  return $ mkRequest env body
