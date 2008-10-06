{-# OPTIONS_GHC -fglasgow-exts #-}
module Blog.Templates
where

import Text.XHtml


data PageVars t1 t2 = (HTML t1, HTML t2) => PageVars
    { ptitle :: t1
    , pcontent :: t2
    }

-- Complete page template
page vars =
    header
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
        +++ (thetitle << ptitle vars)
       )
    +++
    body
    << thediv ! [identifier "container"]
           << pcontent vars


-- Page specific templates

mainIndexPage = page $ PageVars
                { ptitle = "All Unkept"
                , pcontent = (thediv ! [identifier "maintitle"]
                              << thediv
                                     << "All Unkept"
                              +++
                              thediv ! [identifier "content"]
                                         << h1 << "This is the title"
                              +++ p << "This is a test")
                }
