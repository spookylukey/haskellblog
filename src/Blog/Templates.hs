{-# OPTIONS_GHC -fglasgow-exts #-}
module Blog.Templates
where

import Blog.Forms (emailWidget, nameWidget, messageWidget, formatWidget, usernameWidget, passwordWidget, CommentStage(..))
import Blog.Links
import Blog.Utils (escapeHtmlString)
import Data.List (intersperse)
import Data.Maybe (fromJust)
import Ella.Forms.Base
import Ella.Forms.Widgets (makeLabel)
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
import qualified Data.Map as Map


-- | Holds variables for the 'page' template
--
-- fields should be limited to type class HTML, but that makes record
-- update syntax impossible with current GHC.
data PageVars t1 t2 = {- (HTML t1, HTML t2) => -} PageVars
    { ptitle :: t1
    , pcontent :: t2
    }

defaultPageVars = PageVars { ptitle = ""
                           , pcontent = ""
                           }

-- Complete page template
page vars =
    (header
     << (meta ! [httpequiv "Content-Type",
                 content "text/html; charset=utf-8"]
         +++ thelink ! [rel "alternate",
                        thetype "application/rss+xml",
                        title "RSS",
                        href "/TODO"] << ""
         +++ thelink ! [rel "StyleSheet",
                        href "/newblog.css",
                        thetype "text/css"] << ""
         +++ thelink ! [rel "shortcut icon",
                        href "/favicon.ico",
                        thetype "image/x-icon"] << ""
         +++ primHtml "<!--[if lte IE 6]><link rel=\"stylesheet\" href=\"/newblog_IE6.css\" type=\"text/css\" /><![endif]-->"
         +++ thetitle << fulltitle
        ))
    +++
    body
    << thediv ! [identifier "container"]
           << ((thediv ! [identifier "toplinks"]
                           << unordList [ HotLink indexUrl (toHtml "Home") [theclass "first"]
                                        , hotlink categoriesUrl << "Categories"
                                        , hotlink feedsUrl << "Feeds"
                                        , hotlink aboutUrl << "About"
                                        ])
               +++
               (thediv ! [identifier "maintitle"]
                << thediv
                       << "All Unkept")
               +++
               (thediv ! [identifier "content"]
                << (thediv ! [identifier "contentinner"]
                           << pcontent vars))
               +++
               (thediv ! [identifier "footer"]
                << (h1 << "Links"
                    +++
                    (thediv ! [theclass "bloglinks"]
                     << (h2 << "Blog links:"
                        +++
                         unordList [ hotlink indexUrl << "Index"
                                   , hotlink feedsUrl << "Feeds"
                                   , hotlink categoriesUrl << "Categories"
                                   , hotlink aboutUrl << "About blog"
                                   ])
                    )
                    +++
                    (thediv ! [theclass "sitelinks"]
                     << (h2 << "Also on this site:"
                        +++
                         unordList [ hotlink "/" << "Index"
                                   , hotlink "/softprojects.html" << "Software"
                                   , hotlink "/bibleverses/" << "Bible memorisation"
                                   , hotlink "/personal.html" << "About me"
                                   ])
                    )
                   )
               )
              )
    where fulltitle = let pt = ptitle vars
                      in if null pt
                         then "All Unkept"
                         else pt ++ " « All Unkept"


-- Page specific templates

custom404page =
    page $ defaultPageVars
             { pcontent = h1 << "404 Not Found"
                          +++
                          p << "Sorry, the page you requested could not be found"
             , ptitle = "404 Not Found"
             }


commentNameLabel       = makeLabel "Name:" nameWidget
commentNameWidget c    = setVal (Cm.name c) nameWidget
commentEmailLabel      = makeLabel "Email:" emailWidget
commentEmailWidget c   = setVal (Cm.email c) emailWidget
commentFormatLabel     = "Format:"
commentFormatWidget c  = setVal (show $ fromEnum $ Cm.format c) formatWidget
commentMessageLabel    = "Message:"
commentMessageWidget c = setVal (Cm.text_raw c) messageWidget
commentSubmitButton    = submit "submit" "Post"
commentPreviewButton   = submit "preview" "Preview"



infoPage post =
    page $ defaultPageVars
             { pcontent = (h1 << P.title post)
                          +++
                          (thediv ! [theclass "post"]
                                      << (primHtml $ P.post_formatted post)
                          )
             , ptitle = P.title post
             }


loginPage :: Map.Map String String -> Map.Map String String -> Html
loginPage loginData loginErrors =
    page $ defaultPageVars
             { pcontent = (h1 << "Login")
                          +++
                          loginForm loginData loginErrors
             , ptitle = "Login"
             }

loginForm loginData loginErrors =
    (if not $ Map.null loginErrors
        then (thediv ! [theclass "validationerror"]
              << unordList (Map.elems loginErrors))
        else noHtml)
    +++
    form ! [ method "post", action ""]
    << (simpleTable [] [] [ [ toHtml $ makeLabel "User name:" usernameWidget
                            , toHtml $ setVal (fromJust $ Map.lookup "username" loginData) usernameWidget
                            ]
                          , [ toHtml $ makeLabel "Password: " passwordWidget
                            , toHtml $ setVal (fromJust $ Map.lookup "password" loginData) passwordWidget
                            ]
                          ]
        +++
        (submit "login" "Login")
       )

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

categoryLinks categories =
    intersperse (toHtml ", ") $ map categoryLink categories

metaInfoLine post categories divclass =
    (thediv ! [theclass divclass]
     << ("Posted in: "
         +++
         categoryLinks categories
         +++
         (toHtml " | ")
         +++
         (thespan ! [theclass "timestamp"]
          << (showDate $ P.timestamp post)
         )
        )
    )

formatName name = if null name
                  then "Anonymous Coward"
                  else name

categoryLink c = toHtml $ hotlink (categoryUrl c) << (C.name c)

postLink p = toHtml $ hotlink (postUrl p) << (P.title p)

showDate timestamp = formatCalendarTime defaultTimeLocale  "%e %B %Y" (toUTCTime $ epochToClockTime timestamp)


-- HStringTemplate related:

-- Allow for heterogeneous lists
data ToSElemD = forall a. ToSElem a => ToSElemD a

instance ToSElem ToSElemD where
    toSElem (ToSElemD x) = toSElem x

-- Allow Html to be inserted
instance ToSElem Html where
    toSElem x = BS (utf8 $ showHtmlFragment x)

encT = utf8 . escapeHtmlString -- use for text which might contain unicode or HTML chars
encH = utf8                    -- use for HTML

postTemplateInfo :: P.Post -> Map.Map String ToSElemD
postTemplateInfo p = Map.fromList [ ("title", ToSElemD $ encT $ P.title p)
                                  , ("date", ToSElemD $ showDate $ P.timestamp p)
                                  , ("summary", ToSElemD $ encH $ P.summary_formatted p)
                                  , ("full", ToSElemD $ encH $ P.post_formatted p)
                                  , ("url", ToSElemD $ encT $ postUrl p)
                                  , ("commentsOpen", ToSElemD $ P.comments_open p)
                                  ]

categoryTemplateInfo :: C.Category -> Map.Map String ToSElemD
categoryTemplateInfo c = Map.fromList [ ("name", ToSElemD $ encT $ C.name c)
                                      , ("url", ToSElemD $ encT $ categoryUrl c)
                                      ]

commentTemplateInfo :: Cm.Comment -> Map.Map String ToSElemD
commentTemplateInfo cm = Map.fromList [ ("name", ToSElemD $ encT $ Cm.name cm)
                                      , ("formattedName", ToSElemD $ encT $ formatName $ Cm.name cm)
                                      , ("isAuthor", ToSElemD $ Cm.name cm == Settings.blog_author_name)
                                      , ("date", ToSElemD $ showDate $ Cm.timestamp cm)
                                      , ("textFormatted", ToSElemD $ encH $ Cm.text_formatted cm)
                                      , ("email", ToSElemD $ encT $ Cm.email cm)
                                      ]
