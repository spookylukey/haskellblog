module Web.Request (
                    -- * Encodings
                    Encoding(..)
                   , utf8Encoding
                    -- * Requests
                   , Request
                   , RequestOptions(..)
                    -- ** Components of Request
                   , requestMethod, pathInfo, environment
                    -- ** Constructors for Request
                   , mkRequest, buildCGIRequest
                   )

where

import qualified Data.Map as Map
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Maybe
import System.Environment (getEnvironment)
import System.IO (stdin)

-- Encodings

data Encoding = Encoding {
      name :: String
    -- ^ descriptive name of the encoding
    , decoder :: ByteString -> String
    -- ^ convert ByteString to unicode string
    }

instance Eq Encoding where
    x == y = name x == name y

instance Show Encoding where
    show x = "Encoding " ++ name x

-- Defaults

utf8Encoding = Encoding {
                 name = "UTF8"
               , decoder = UTF8.toString
               }


-- | Options that affect the way that HTTP requests are handled
data RequestOptions = RequestOptions {
      encoding :: Encoding -- ^ Handles request encoding translation
    } deriving (Eq, Show)

data Request = Request {
      environment :: Map.Map String String
    , requestBody :: ByteString
    , requestEncoding :: Encoding
    } deriving (Show, Eq)

-- | Create a Request object
mkRequest :: [(String, String)] -- ^ association list of environment variables
          -> ByteString -- ^ lazy ByteString containing request body
          -> Encoding -- ^ Encoding to use for request
          -> Request
mkRequest env body enc
    = let envMap = Map.fromList env
      in Request {
               environment = envMap
             , requestBody = body
             , requestEncoding = enc
             }

-- | Returns the request method (GET, POST etc) of the request
requestMethod :: Request -> String
requestMethod request = fromJust $ Map.lookup "REQUEST_METHOD" $ environment request

-- | Returns the path info of the request, with leading forward slash removed.
pathInfo request = let pi = Map.lookup "PATH_INFO" $ environment request
                       -- Normalise to having no leading slash
                       adjusted = case pi of
                                    Nothing -> ""
                                    Just ('/':rest) -> rest
                                    Just path -> path
                       -- PATH_INFO contains Haskell strings, but they
                       -- may contain uninterpreted byte sequences
                       -- instead of Unicode chars.  We re-pack as
                       -- bytes (BS.pack discards anything > \255),
                       -- and then re-interpret.
                       bytes = BS.pack adjusted
                   in (decoder $ requestEncoding request) bytes


-- | Creates a Request object according to the CGI protocol
buildCGIRequest :: Encoding -> IO Request
buildCGIRequest encoding = do
  env <- getEnvironment
  body <- BS.hGetContents stdin
  return $ mkRequest env body encoding

