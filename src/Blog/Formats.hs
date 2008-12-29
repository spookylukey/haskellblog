module Blog.Formats ( Format(..)
                    , getFormatter
                    )

where

import Control.Arrow ((>>>))
import Data.Array.IArray
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Ix
import qualified Text.XHtml as X

import Blog.Utils (regexReplace, regexReplaceCustom)
import Ella.GenUtils (utf8)

data Format = Rawhtml | Plaintext
            deriving (Eq, Ord, Show, Read, Enum, Ix)


formatRawhtml :: String -> String
formatRawhtml = id

formatPlaintext :: String -> String
formatPlaintext s = regexReplace "&" (BL.pack "&amp;") >>>
                    regexReplace "<" (BL.pack "&lt;") >>>
                    regexReplace ">" (BL.pack "&gt;") >>>
                    regexReplace "\n" (BL.pack "<br />\n") >>>
                    regexReplaceCustom "^(\\s+)"
                                           (regexReplace " " (BL.pack "&nbsp;")) >>>
                    UTF8.toString
                    $ (utf8 s)

formatters :: Array Format (String -> String)
formatters = array (Rawhtml, Plaintext)
             [ (Rawhtml, formatRawhtml)
             , (Plaintext, formatPlaintext)
             ]

getFormatter f = formatters ! f
