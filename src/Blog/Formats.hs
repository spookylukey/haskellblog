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


url_regex =
    "https?://"                                    ++ -- http:// or https://
    "(?:(?:[a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,6}|"       ++ -- domain...
    "localhost|"                                   ++ --localhost...
    "\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3})"   ++ -- ...or ip
    "(?::\\d+)?"                                   ++ -- optional port
    "(?:/\\S+|/?)"

escapeHtml = regexReplace "&" (BL.pack "&amp;") >>>
             regexReplace "<" (BL.pack "&lt;") >>>
             regexReplace ">" (BL.pack "&gt;")

escapeQuotes = regexReplace "\"" (BL.pack "&quot;")

linkify = regexReplaceCustom url_regex (\s -> (BL.pack "<a href=\"") `BL.append` (escapeQuotes s) `BL.append` (BL.pack "\">") `BL.append` s `BL.append` (BL.pack "</a>"))

preserveLeadingWhitespace = regexReplaceCustom "^(\\s+)" (regexReplace " " (BL.pack "&nbsp;"))

nl2br = regexReplace "\n" (BL.pack "<br />\n")

formatPlaintext :: String -> String
formatPlaintext s = utf8 >>>
                    escapeHtml >>>
                    nl2br >>>
                    linkify >>>
                    preserveLeadingWhitespace >>>
                    UTF8.toString
                    $ s

formatters :: Array Format (String -> String)
formatters = array (Rawhtml, Plaintext)
             [ (Rawhtml, formatRawhtml)
             , (Plaintext, formatPlaintext)
             ]

getFormatter f = formatters ! f
