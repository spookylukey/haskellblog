module Blog.Templates
where

import Text.XHtml

-- Complete page template
page ptitle pcontent =
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
        +++ (thetitle << ptitle)
       )
    +++
    body
    << thediv ! [identifier "container"]
           << pcontent


-- Page specific templates

mainIndexPage = page "All Unkept"
                (thediv ! [identifier "maintitle"]
                            << thediv
                                   << "All Unkept"
                 +++
                 thediv ! [identifier "content"]
                            << h1 << "This is the title"
                            +++ p << "This is a test")
