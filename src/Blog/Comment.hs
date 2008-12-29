module Blog.Comment where

import Blog.Formats (Format)

data Comment = Comment {
      uid :: Int
    , post_id :: Int
    , timestamp :: Int
    , name :: String
    , email :: String
    , text_raw :: String
    , text_formatted :: String
    , format :: Format
    } deriving (Show, Eq)
