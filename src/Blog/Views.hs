module Blog.Views where

import Web.Request
import Web.Response
import qualified Data.ByteString.Lazy.Char8 as BS

mainIndex :: Request -> IO (Maybe Response)
mainIndex req = do
  let r1 = utf8HtmlResponse
  let r2 = addContent (BS.pack "Hello, world") r1
  return $ Just r2
