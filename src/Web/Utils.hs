module Web.Utils where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Text.XHtml (renderHtml)
import Web.Response (addContent)


import GHC.Exts( IsString(..) )
instance IsString ByteString where
    fromString = UTF8.fromString

utf8 = UTF8.fromString

-- Utility functions
addHtml html resp = addContent (utf8 $ renderHtml html) resp
