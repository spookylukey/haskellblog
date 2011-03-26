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
      postRaw :: LB.ByteString,
      postFormatted :: LB.ByteString,
      summaryRaw :: LB.ByteString,
      summaryFormatted :: LB.ByteString,
      format :: Format,
      timestamp :: Int,
      commentsOpen :: Bool
    } deriving (Show, Eq, Data, Typeable)
