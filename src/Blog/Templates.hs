{-# OPTIONS_GHC -fglasgow-exts #-}
module Blog.Templates
where

import Blog.Links
import Data.List (intersperse)
import Text.XHtml
import qualified Blog.Post as P
import qualified Blog.Category as C
import System.Locale (defaultTimeLocale)
import System.Time.Utils (epochToClockTime)
import System.Time (toUTCTime, formatCalendarTime)

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
                                        , hotlink "/about/" << "About"
                                        ])
               +++
               (thediv ! [identifier "maintitle"]
                << thediv
                       << "All Unkept")
               +++
               (thediv ! [identifier "content"]
                << (thediv ! [identifier "contentinner"]
                           << pcontent vars))
              )
    where fulltitle = let pt = ptitle vars
                      in if null pt
                         then "All Unkept"
                         else pt ++ " « All Unkept"


-- Page specific templates

mainIndexPage = page $ defaultPageVars
                { pcontent = h1 << "Recent posts"
                             +++
                             p << "This is a test"
                , ptitle = ""
                }

categoriesPage = page $ defaultPageVars
                 { pcontent = h1 << "Categories"
                              +++
                              p << "TODO"
                 , ptitle = "Categories"
                 }

postPage post categories =
    page $ defaultPageVars
             { pcontent = formatPost post categories
             , ptitle = P.title post
             }



formatPost post categories =
    (h1 << P.title post
     +++
     (thediv ! [theclass "timestamp"]
      << ("Posted: "
          ++
          (showDate $ P.timestamp post)))
     +++
     (thediv ! [theclass "postcategories"]
      << ((toHtml "Categories: ")
          +++
          (intersperse (toHtml ", ") $ map toHtml $ map categoryLink categories)))
     +++
     (thediv ! [theclass "post"]
      << (primHtml $ P.post_formatted post)
     )
    )


custom404page = page $ defaultPageVars { pcontent = h1 << "404 Not Found"
                                                    +++
                                                    p << "Sorry, the page you requested could not be found"
                                       , ptitle = "404 Not Found"
                                       }

-- Utilities

categoryLink c = hotlink (categoryUrl c) << (C.name c)

showDate timestamp = formatCalendarTime defaultTimeLocale  "%Y-%m-%d" (toUTCTime $ epochToClockTime timestamp)
