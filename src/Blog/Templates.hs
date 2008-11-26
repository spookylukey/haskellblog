{-# OPTIONS_GHC -fglasgow-exts #-}
module Blog.Templates
where

import Blog.Links
import Data.List (intersperse)
import Text.XHtml
import qualified Blog.Post as P
import qualified Blog.Category as C
import qualified Blog.Comment as Cm
import qualified Blog.Settings as Settings
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
                << unordList [ hotlink "/" << "Main site"
                             , hotlink aboutUrl << "About blog"
                             , hotlink "/personal.html" << "About me"
                             , hotlink "/softprojects.html" << "Software"
                             ]
               )
              )
    where fulltitle = let pt = ptitle vars
                      in if null pt
                         then "All Unkept"
                         else pt ++ " « All Unkept"


-- Page specific templates

mainIndexPage posts =
    page $ defaultPageVars
             { pcontent = h1 << "Recent posts"
                          +++
                          do post <- posts
                             return (
                                     (thediv ! [ theclass "summarylink" ]
                                      << postLink post)
                                     +++
                                     (thediv ! [ theclass "summary" ]
                                      << (primHtml $ P.summary_formatted post))
                                    )
             , ptitle = ""
             }

categoriesPage = page $ defaultPageVars
                 { pcontent = h1 << "Categories"
                              +++
                              p << "TODO"
                 , ptitle = "Categories"
                 }

postPage post categories comments related =
    page $ defaultPageVars
             { pcontent = formatPost post categories comments related
             , ptitle = P.title post
             }


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


formatPost post categories comments otherposts =
    (h1 ! [theclass "posttitle"] << P.title post
     +++
     metaInfoLine post categories "metainfo"
     +++
     (thediv ! [theclass "post"]
      << (primHtml $ P.post_formatted post)
     )
     +++
     (if null otherposts
      then thediv << ""
      else (thediv ! [theclass "related"]
            << ((h1 << "Related:")
                +++ (unordList $ map formatRelated otherposts))
           )
     )
     +++
     (thediv ! [theclass "comments"]
      << ((h1 << "Comments:")
          +++ if null comments
              then p << "No comments."
              else thediv << map formatComment comments
         )
     )
    )

commentclass comment = "comment" ++
    if (Cm.name comment == Settings.blog_author_name)
       then " author"
       else ""

formatComment comment =
    (thediv ! [theclass (commentclass comment)] <<
     (
      (thediv ! [theclass "commentby"] <<
       ((thespan ! [theclass "timestamp"] << (" " ++ showDate (Cm.timestamp comment) ++ " "))
         +++
         (thespan << (formatName $ Cm.name comment))
       )
      )
      +++
      (thediv ! [theclass "commenttext"] <<
       (primHtml $ Cm.text_formatted comment))
     )
    ) +++ hr

formatRelated = postLink

formatName name = if null name
                  then "Anonymous Coward"
                  else name

custom404page = page $ defaultPageVars { pcontent = h1 << "404 Not Found"
                                                    +++
                                                    p << "Sorry, the page you requested could not be found"
                                       , ptitle = "404 Not Found"
                                       }

-- Utilities

categoryLink c = toHtml $ hotlink (categoryUrl c) << (C.name c)

postLink p = toHtml $ hotlink (postUrl p) << (P.title p)


showDate timestamp = formatCalendarTime defaultTimeLocale  "%Y-%m-%d %H:%M" (toUTCTime $ epochToClockTime timestamp)
