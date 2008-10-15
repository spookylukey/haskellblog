{-# OPTIONS_GHC -fglasgow-exts #-}
module Blog.Templates
where

import Blog.Links
import Text.XHtml


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
         +++ thetitle << fulltitle
        ))
    +++
    body
    << thediv ! [identifier "container"]
           << ((thediv ! [identifier "maintitle"]
                << thediv
                       << "All Unkept")
               +++
               (thediv ! [identifier "toplinks"]
                           << unordList [ HotLink indexLink (toHtml "Home") [theclass "first"]
                                        , hotlink categoriesLink << "Categories"
                                        , hotlink "/about/" << "About"
                                        ])
               +++
               (thediv ! [identifier "content"]
                           << pcontent vars)
              )
    where fulltitle = let pt = ptitle vars
                      in if null pt
                         then "All Unkept"
                         else pt ++ " « All Unkept"


-- Page specific templates

mainIndexPage = page $ defaultPageVars
                { pcontent = h1 << "This is the title"
                             +++
                             p << "This is a test"
                , ptitle = "This is the title"
                }

categoriesPage = page $ defaultPageVars
                 { pcontent = h1 << "Categories"
                              +++
                              p << "TODO"
                 , ptitle = "Categories"
                 }
