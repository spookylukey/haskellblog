module Blog.Feeds ( allPostsFeed
                  , allCommentsFeed
                  , categoryPostsFeed
                  , postCommentFeed
                  )

where

import Blog.Links
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import Text.Atom.Feed
import qualified Blog.Category as C
import qualified Blog.Comment as Cm
import qualified Blog.Post as P
import qualified Blog.Settings as Settings
import qualified Data.ByteString.Lazy.UTF8 as UTF8

rfc3339 :: UTCTime -> String
rfc3339 d = formatTime defaultTimeLocale "%FT%TZ" d

formatTimestamp :: Int -> String
formatTimestamp = rfc3339 . posixSecondsToUTCTime . realToFrac

fullUrl url = Settings.absolute_root ++ url

selfLink url = Link { linkHref = fullUrl url
                    , linkRel = Just $ Left "self"
                    , linkType = Nothing
                    , linkHrefLang = Nothing
                    , linkTitle = Nothing
                    , linkLength = Nothing
                    , linkAttrs = []
                    , linkOther = []
                    }

htmlLink url = Link { linkHref = fullUrl url
                    , linkRel = Just $ Left "alternate"
                    , linkType = Just "text/html"
                    , linkHrefLang = Nothing
                    , linkTitle = Nothing
                    , linkLength = Nothing
                    , linkAttrs = []
                    , linkOther = []
                    }

authors = [ Person { personName = "Luke Plant"
                   , personURI = Just "http://lukeplant.me.uk/"
                   , personEmail = Nothing
                   , personOther = []
                   }
          ]

commentAuthor name = Person { personName = name
                            , personURI = Nothing
                            , personEmail = Nothing
                            , personOther = []
                            }

mkPostEntry post =
    Entry { entryId = fullUrl $ postUrl post
          , entryTitle = TextString $ UTF8.toString $ P.title post
          , entryUpdated = formatTimestamp $ P.timestamp post
          , entryAuthors = authors
          , entryCategories = []
          , entryContent = Just $ HTMLContent $ UTF8.toString $ P.post_formatted post
          , entryContributor = []
          , entryLinks = [ htmlLink $ postUrl post
                         ]
          , entryPublished = Just $ formatTimestamp $ P.timestamp post
          , entryRights = Nothing
          , entrySource = Nothing
          , entrySummary = Just $ HTMLString $ UTF8.toString $ P.summary_formatted post
          , entryInReplyTo = Nothing
          , entryInReplyTotal = Nothing
          , entryAttrs = []
          , entryOther = []
          }

-- | creates a Feed for a list of posts, which must already be in descending
-- order by timestamp (newest first)
allPostsFeed :: [P.Post] -> Feed
allPostsFeed posts =
    Feed { feedId = fullUrl $ allPostsFeedUrl
         , feedTitle = TextString "All Unkept"
         , feedUpdated = if null posts
                         then "" -- the best we can do
                         else formatTimestamp $ P.timestamp $ head $ posts
         , feedAuthors = authors
         , feedCategories = [] -- Possible TODO
         , feedContributors = []
         , feedGenerator = Nothing
         , feedIcon = Just "http://lukeplant.me.uk/favicon.ico"
         , feedLinks = [ selfLink allPostsFeedUrl
                       , htmlLink indexUrl
                       ]
         , feedLogo = Nothing -- TODO
         , feedRights = Just $ TextString "© Luke Plant"
         , feedSubtitle = Nothing
         , feedEntries = map mkPostEntry posts
         , feedAttrs = []
         , feedOther = []
         }

categoryPostsFeed cat posts =
    let basefeed = allPostsFeed posts
        url = categoryPostsFeedUrl cat
    in basefeed { feedId = fullUrl $ url
                , feedTitle = TextString $ "All Unkept - " ++ C.name cat
                , feedLinks = [ selfLink url
                              , htmlLink $ categoryUrl cat
                              ]
                }

--mkCommentEntry :: (Cm.Comment, P.Post) -> Entry
mkCommentEntry (comment, post) =
    Entry { entryId = fullUrl $ commentUrl comment post
          , entryTitle = TextString ("Comment #" ++ (show $ Cm.uid comment) ++ " on post " ++ (UTF8.toString $ P.title post))
          , entryUpdated = formatTimestamp $ Cm.timestamp comment
          , entryAuthors = [commentAuthor $ UTF8.toString $ Cm.name comment]
          , entryCategories = []
          , entryContent = Just $ HTMLContent $ UTF8.toString $ Cm.text_formatted comment
          , entryContributor = []
          , entryLinks = [ htmlLink $ commentUrl comment post
                         ]
          , entryPublished = Just $ formatTimestamp $ Cm.timestamp comment
          , entryRights = Nothing
          , entrySource = Nothing
          , entrySummary = Nothing
          , entryInReplyTo = Nothing
          , entryInReplyTotal = Nothing
          , entryAttrs = []
          , entryOther = []
          }

allCommentsFeed :: [(Cm.Comment, P.Post)] -> Feed
allCommentsFeed commentsAndPosts =
    Feed { feedId = fullUrl $ allCommentsFeedUrl
         , feedTitle = TextString "All Unkept - Comments"
         , feedUpdated = if null commentsAndPosts
                         then "" -- the best we can do
                         else formatTimestamp $ Cm.timestamp $ fst $ head $ commentsAndPosts
         , feedAuthors = []
         , feedCategories = [] -- Possible TODO
         , feedContributors = []
         , feedGenerator = Nothing
         , feedIcon = Just "http://lukeplant.me.uk/favicon.ico"
         , feedLinks = [ selfLink allCommentsFeedUrl
                       ]
         , feedLogo = Nothing -- TODO
         , feedRights = Just $ TextString "© Luke Plant"
         , feedSubtitle = Nothing
         , feedEntries = map mkCommentEntry commentsAndPosts
         , feedAttrs = []
         , feedOther = []
         }


postCommentFeed comments post =
    let basefeed = allCommentsFeed $ zip comments (repeat post)
        url = postCommentFeedUrl post
    in basefeed { feedId = fullUrl url
                , feedTitle = TextString $ "All Unkept - Comments on " ++ (UTF8.toString $ P.title post)
                , feedLinks = [ selfLink url
                              , htmlLink (url ++ "#comments")
                              ]
                }

