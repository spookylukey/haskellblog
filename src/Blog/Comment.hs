{-# LANGUAGE DeriveDataTypeable #-}
module Blog.Comment where

import Blog.Formats (Format)
import Data.Data
import Data.Typeable

data Comment = Comment {
      uid :: Int
    , post_id :: Int
    , timestamp :: Int
    , name :: String
    , email :: String
    , text_raw :: String
    , text_formatted :: String
    , format :: Format
    } deriving (Show, Eq, Data, Typeable)
