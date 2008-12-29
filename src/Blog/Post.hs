module Blog.Post where

import Blog.Formats (Format)

data Post = Post {
      uid :: Int,
      title :: String,
      slug :: String,
      post_raw :: String,
      post_formatted :: String,
      summary_raw :: String,
      summary_formatted :: String,
      format :: Format,
      timestamp :: Int,
      comments_open :: Bool
    } deriving (Show, Eq)

