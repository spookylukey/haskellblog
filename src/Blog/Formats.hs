{-# LANGUAGE DeriveDataTypeable #-}
module Blog.Formats ( Format(..)
                    , getFormatter
                    )

where

import Blog.Utils (regexReplace, regexReplaceCustom, regexReplaceS)
import Control.Arrow ((>>>))
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Data
import Data.Maybe (fromJust)
import Data.Typeable
import Ella.GenUtils (utf8)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.Map as Map
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Definition as PD
import qualified Text.XHtml as X

data Format = Rawhtml
            | Plaintext
            | RST
            deriving (Eq, Ord, Show, Read, Enum, Data, Typeable)


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

normaliseCRLF = regexReplace "\r\n" (BL.pack "\n")
normaliseCRLF_S = regexReplaceS "\r\n" "\n"

linkify = regexReplaceCustom url_regex (\s -> (BL.pack "<a href=\"") `BL.append` (escapeQuotes s) `BL.append` (BL.pack "\">") `BL.append` s `BL.append` (BL.pack "</a>"))

preserveLeadingWhitespace = regexReplaceCustom "^(\\s+)" (regexReplace " " (BL.pack "&nbsp;"))

nl2br = regexReplace "\n" (BL.pack "<br />\n")

formatPlaintext :: String -> String
formatPlaintext s = utf8 >>>
                    escapeHtml >>>
                    normaliseCRLF >>>
                    linkify >>>
                    nl2br >>>
                    preserveLeadingWhitespace >>>
                    UTF8.toString
                    $ s

removeRawHtml :: PD.Pandoc -> PD.Pandoc
removeRawHtml (PD.Pandoc m blocks) = PD.Pandoc m (filter (not . isRawHtml) blocks)
    where
      isRawHtml (PD.RawHtml s) = True
      isRawHtml _ = False

formatRST :: String -> String
formatRST = normaliseCRLF_S >>>
            Pandoc.readRST (Pandoc.defaultParserState { Pandoc.stateSanitizeHTML = False }) >>>
            removeRawHtml >>>
            Pandoc.writeHtmlString Pandoc.defaultWriterOptions { Pandoc.writerStandalone = False }

formatters :: Map.Map Format (String -> String)
formatters = Map.fromList
             [ (Rawhtml, formatRawhtml)
             , (Plaintext, formatPlaintext)
             , (RST, formatRST)
             ]

getFormatter f = fromJust $ Map.lookup f formatters
