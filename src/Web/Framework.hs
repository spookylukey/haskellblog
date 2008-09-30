{-# OPTIONS_GHC -fglasgow-exts -XOverloadedStrings #-}
module Web.Framework (
                      dispatchCGI
                     , dispatchRequest
                     , default404
                     , DispatchOptions(..)
                     , defaultDispatchOptions
                     , View
                     )

where

import Web.Response
import Web.Request
import Web.Utils
import System.IO (stdout, hClose)
import qualified Data.ByteString.Lazy.Char8 as BS

data DispatchOptions = DispatchOptions {
      notFoundHandler :: Request -> IO Response -- function that will return a 404 page in the case
                                                -- of no view functions matching
}

type View = Request -> IO (Maybe Response)

default404 = buildResponse utf8HtmlResponse [
              setStatus 404,
              addContent "<h1>404 Not Found</h1>\n<p>Sorry, the page you requested could not be found.</p>"
             ]

defaultDispatchOptions = DispatchOptions {
                           notFoundHandler = const $ return $ default404
                         }

dispatchRequest :: Request -> [View] -> IO (Maybe Response)
dispatchRequest req [] = return Nothing
dispatchRequest req (v:vs) = do
  resp <- v req
  case resp of
    Nothing -> dispatchRequest req vs
    x -> return x

-- | Handle a CGI request using a list of possible views
-- If a view returns 'Nothing' the next will be tried,
-- and a 404 issued if all return nothing
dispatchCGI :: [View] -- list of views functions that will be tried in order
            -> DispatchOptions                  -- options to use in dispatching
            -> IO ()
dispatchCGI views opts = do
  req <- buildCGIRequest
  resp' <- dispatchRequest req views
  resp <- case resp' of
            Nothing -> notFoundHandler opts $ req
            Just x -> return x
  BS.hPut stdout (formatResponse resp)
