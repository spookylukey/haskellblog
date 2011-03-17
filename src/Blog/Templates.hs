{-# OPTIONS_GHC -fglasgow-exts #-}
module Blog.Templates
where

import Blog.Links
import Blog.Utils (escapeHtmlStringBS)
import Data.Maybe (fromJust)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (formatTime)
import Ella.GenUtils (utf8)
import System.Locale (defaultTimeLocale)
import Text.XHtml
import Text.StringTemplate
import Text.StringTemplate.Classes (SElem(..))
import qualified Blog.Category as C
import qualified Blog.Comment as Cm
import qualified Blog.Post as P
import qualified Blog.Settings as Settings
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Map as Map
import qualified Data.Text.Lazy as LT

-- Templates

get_templates :: IO (STGroup UTF8.ByteString)
get_templates = do
  g1 <- directoryGroup Settings.template_path
  let g2 = setEncoderGroup escapeHtmlStringBS g1
      g3 = groupStringTemplates [("noescape", newSTMP "$it$" :: StringTemplate UTF8.ByteString)]
      g4 = mergeSTGroups g2 g3
  return g4

get_template :: String -> IO (StringTemplate UTF8.ByteString)
get_template name = do
  templates <- get_templates
  return $ fromJust $ getStringTemplate name templates

-- General HTML fragments

-- TODO - fix this to be able to work with URLs that have query
-- strings already.
pagingLinks :: String     -- ^ Base URL
            -> Int        -- ^ Current page
            -> Bool       -- ^ True if there is another page
            -> Html
pagingLinks url page shownext =
    (thediv ! [theclass "paginglinks"]
     << ((if page > 1
          then makeLink url (page - 1) "« Back"
          else thespan << "« Back")
         +++
         (toHtml " | ")
         +++
         (if shownext
          then makeLink url (page + 1) "Next »"
          else thespan << "Next »")
        )
     )
    where makeLink url page text = toHtml (hotlink (url ++ "?p=" ++ (show page)) << text)

formatName :: LB.ByteString -> LB.ByteString
formatName name = if LB.null name
                  then LB.pack "Anonymous Coward"
                  else name

showDate timestamp = formatTime defaultTimeLocale "%e %B %Y" $ posixSecondsToUTCTime $ realToFrac timestamp

-- HStringTemplate related:

-- Allow for heterogeneous lists
data ToSElemD = forall a. ToSElem a => ToSElemD a

instance ToSElem ToSElemD where
    toSElem (ToSElemD x) = toSElem x

-- Allow Html to be inserted
instance ToSElem Html where
    toSElem x = STR $ showHtmlFragment x

-- Convert to form needed for templates
postTemplateInfo :: P.Post -> Map.Map String ToSElemD
postTemplateInfo p = Map.fromList [ ("title", ToSElemD $ P.title p)
                                  , ("date", ToSElemD $ showDate $ P.timestamp p)
                                  , ("summary", ToSElemD $ P.summary_formatted p)
                                  , ("full", ToSElemD $ P.post_formatted p)
                                  , ("url", ToSElemD $ postUrl p)
                                  , ("commentsOpen", ToSElemD $ P.comments_open p)
                                  , ("adminUrl", ToSElemD $ adminEditPostUrl p)
                                  ]

categoryTemplateInfo :: C.Category -> Map.Map String ToSElemD
categoryTemplateInfo c = Map.fromList [ ("name", ToSElemD $ C.name c)
                                      , ("url", ToSElemD $ categoryUrl c)
                                      ]

commentTemplateInfo :: Cm.Comment -> Map.Map String ToSElemD
commentTemplateInfo cm = Map.fromList [ ("name", ToSElemD $ Cm.name cm)
                                      , ("formattedName", ToSElemD $ formatName $ Cm.name cm)
                                      , ("isAuthor", ToSElemD $ Cm.name cm == utf8 Settings.blog_author_name)
                                      , ("date", ToSElemD $ showDate $ Cm.timestamp cm)
                                      , ("textFormatted", ToSElemD $ Cm.textformatted cm)
                                      , ("email", ToSElemD $ Cm.email cm)
                                      , ("uid", ToSElemD $ Cm.uid cm)
                                      , ("hidden", ToSElemD $ Cm.hidden cm)
                                      , ("response", ToSElemD $ emptyToNothingBS $ Cm.response cm)
                                      ]

emptyToNothingBS s = if LB.null s then Nothing else Just s
