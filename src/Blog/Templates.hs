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
                       href "style.css",
                       thetype "text/css"] << ""
        +++ thelink ! [rel "StyleSheet",
                       href "blog.css",
                       thetype "text/css"] << ""
        +++ thelink ! [rel "shortcut icon",
                       href "/favicon.ico",
                       thetype "image/x-icon"] << ""
        +++ (thetitle << ptitle)
       )
    +++
    body
    << pcontent


-- Page specific templates

mainIndexPage = page "All Unkept"
                (h1 << "All Unkept" +++
                 p << "This is a test")
