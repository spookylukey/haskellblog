module Blog.Views where

import Web.Request
import Web.Response
import qualified Data.ByteString.Lazy.Char8 as BS

mainIndex :: Request -> IO (Maybe Response)
mainIndex req = let resp = buildResponse utf8HtmlResponse [
                            addContent (BS.pack "Hello, world")
                           ]
                in return $ Just resp
