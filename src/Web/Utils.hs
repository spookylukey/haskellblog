module Web.Utils where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS

import GHC.Exts( IsString(..) )
instance IsString ByteString where
    fromString = BS.pack
