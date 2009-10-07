{-# OPTIONS_GHC -fglasgow-exts #-}
module Blog.Templates
where

import Blog.Forms (formatWidget, CommentStage(..))
import Blog.Links
import Blog.Utils (escapeHtmlStringT)
import Data.Maybe (fromJust)
import Ella.Forms.Base
import Ella.Forms.Widgets (makeLabel)
import Ella.Forms.Widgets.TextInput (TextInput)
import Ella.GenUtils (utf8)
import System.Locale (defaultTimeLocale)
import System.Time (toUTCTime, formatCalendarTime)
import System.Time.Utils (epochToClockTime)
import Text.XHtml
import Text.StringTemplate
import Text.StringTemplate.Classes (SElem(..))
import qualified Blog.Category as C
import qualified Blog.Comment as Cm
import qualified Blog.Post as P
import qualified Blog.Settings as Settings
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as Map
import qualified Data.Text.Lazy as LT

-- Templates

get_templates :: IO (STGroup LT.Text)
get_templates = do
  g1 <- directoryGroup Settings.template_path
  let g2 = setEncoderGroup escapeHtmlStringT g1
      g3 = groupStringTemplates [("noescape", newSTMP "$it$" :: StringTemplate LT.Text)]
      g4 = mergeSTGroups g2 g3
  return g4

get_template :: String -> IO (StringTemplate LT.Text)
get_template name = do
  templates <- get_templates
  return $ fromJust $ getStringTemplate name templates

-- Widgets

commentFormatWidget c  = setVal (show $ fromEnum $ Cm.format c) formatWidget

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

formatName name = if null name
                  then "Anonymous Coward"
                  else name

showDate timestamp = formatCalendarTime defaultTimeLocale  "%e %B %Y" (toUTCTime $ epochToClockTime timestamp)

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
                                  ]

categoryTemplateInfo :: C.Category -> Map.Map String ToSElemD
categoryTemplateInfo c = Map.fromList [ ("name", ToSElemD $ C.name c)
                                      , ("url", ToSElemD $ categoryUrl c)
                                      ]

commentTemplateInfo :: Cm.Comment -> Map.Map String ToSElemD
commentTemplateInfo cm = Map.fromList [ ("name", ToSElemD $ Cm.name cm)
                                      , ("formattedName", ToSElemD $ formatName $ Cm.name cm)
                                      , ("isAuthor", ToSElemD $ Cm.name cm == Settings.blog_author_name)
                                      , ("date", ToSElemD $ showDate $ Cm.timestamp cm)
                                      , ("textFormatted", ToSElemD $ Cm.text_formatted cm)
                                      , ("email", ToSElemD $ Cm.email cm)
                                      ]
