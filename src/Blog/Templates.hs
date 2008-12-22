{-# OPTIONS_GHC -fglasgow-exts #-}
module Blog.Templates
where

import Blog.Forms (emailWidget, nameWidget, messageWidget)
import Blog.Links
import Ella.Forms.Widgets (makeLabel)
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

mainIndexPage :: [(P.Post, [C.Category])] -- ^ list of posts (wtth their categories) to display
              -> Int                      -- ^ current page number being displayed
              -> Bool                     -- ^ True if there are more pages to display
              -> Html
mainIndexPage postInfo curpage moreposts =
    page $ defaultPageVars
             { pcontent = formatIndex postInfo curpage moreposts
             , ptitle = ""
             }

formatIndex :: [(P.Post, [C.Category])] -> Int -> Bool -> Html
formatIndex postInfo curpage shownext =
    (h1 << "Recent posts")
    +++
    (do (post, cats) <- postInfo
        return (
                (thediv ! [ theclass "summarylink" ]
                 << postLink post)
                +++
                (metaInfoLine post cats "metainfoindex")
                +++
                (thediv ! [ theclass "summary" ]
                 << (primHtml $ P.summary_formatted post))
               )
    ) +++ (
           pagingLinks indexUrl curpage shownext
          )

categoriesPage :: [C.Category] -> Html
categoriesPage cats =
    page $ defaultPageVars
             { pcontent = h1 << "Categories"
                          +++
                          (map formatCategoryLink cats)
             , ptitle = "Categories"
             }

formatCategoryLink cat =
    (thediv ! [theclass "category"]
     << categoryLink cat)

categoryPage :: C.Category -> [P.Post] -> Int -> Bool -> Html
categoryPage cat posts curpage moreposts =
    page $ defaultPageVars
         { pcontent = formatCategoryIndex cat posts curpage moreposts
         , ptitle = C.name cat
         }

formatCategoryIndex cat posts curpage moreposts = 
    (h1 << ("Category: " ++ C.name cat))
    +++
    (do post <- posts
        return (
                (thediv ! [ theclass "summarylink" ]
                 << postLink post)
                +++
                (thediv ! [ theclass "summary" ]
                 << (primHtml $ P.summary_formatted post))
               )
    ) +++ (
           pagingLinks (categoryUrl cat) curpage moreposts
          )


postPage :: P.Post        -- ^ The Post to display
         -> [C.Category]  -- ^ Categories the post is in
         -> [Cm.Comment]  -- ^ Comments belonging to the poast
         -> [P.Post]      -- ^ Related posts
         -> Html
postPage post categories comments related =
    page $ defaultPageVars
             { pcontent = formatPost post categories comments related
             , ptitle = P.title post
             }

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
     +++
     (if P.comments_open post
      then (thediv ! [identifier "addcomment"]
            << ((h1 << "Add comment:")
                +++
                commentForm post
               )
           )
      else (hr +++ p << "Closed for comments.")
     )
    )

commentForm post =
    form ! [method "post", action "#addcomment"]
    << (
        (table <<
         (
          (tr <<
           (td << makeLabel "Name:" nameWidget
            +++
            td << nameWidget
           ))
          +++
          (tr <<
           (td << makeLabel "Email:" emailWidget
            +++
            td << emailWidget
           ))))
        +++
        messageWidget
        +++
        br
        +++
        (submit "post" "Post")
        +++
        (submit "preview" "Preview")
       )

commentclass comment = "comment" ++
    if (Cm.name comment == Settings.blog_author_name)
       then " author"
       else ""

formatComment comment =
    (thediv ! [theclass (commentclass comment)] <<
     (
      (thediv ! [theclass "introline"] <<
       (
        (thespan << "On ")
        +++
        (thespan ! [theclass "timestamp"] << (showDate (Cm.timestamp comment)))
        +++
        (thespan << ", ")
        +++
        (thespan ! [theclass "commentby"] << (formatName $ Cm.name comment))
        +++
        (thespan << " wrote:")
       )
      )
      +++
      (thediv ! [theclass "commenttext"] <<
                 (primHtml $ Cm.text_formatted comment))
      )
     ) +++ hr

formatRelated = postLink

infoPage post =
    page $ defaultPageVars
             { pcontent = (h1 << P.title post)
                          +++
                          (thediv ! [theclass "post"]
                                      << (primHtml $ P.post_formatted post)
                          )
             , ptitle = P.title post
             }


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
          then makeLink url (page - 1) "<< Back"
          else thespan << "<< Back")
         +++
         (toHtml " | ")
         +++
         (if shownext
          then makeLink url (page + 1) "Next >>"
          else thespan << "Next >>")
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
