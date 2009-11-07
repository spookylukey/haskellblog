{-# LANGUAGE DeriveDataTypeable #-}
module Blog.Post where

import Blog.Formats (Format)
import Data.Data
import Data.Typeable
import qualified Data.ByteString.Lazy as LB

data Post = Post {
      uid :: Int,
      title :: LB.ByteString,
      slug :: LB.ByteString,
      post_raw :: LB.ByteString,
      post_formatted :: LB.ByteString,
      summary_raw :: LB.ByteString,
      summary_formatted :: LB.ByteString,
      format :: Format,
      timestamp :: Int,
      comments_open :: Bool
    } deriving (Show, Eq, Data, Typeable)
