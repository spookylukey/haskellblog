{-# LANGUAGE DeriveDataTypeable #-}
module Blog.Comment where

import Blog.Formats (Format)
import qualified Data.ByteString.Lazy as LB
import Data.Data
import Data.Typeable

data Comment = Comment {
      uid :: Int
    , post_id :: Int
    , timestamp :: Int
    , name :: LB.ByteString
    , email :: LB.ByteString
    , textraw :: LB.ByteString
    , textformatted :: LB.ByteString
    , format :: Format
    , hidden :: Bool
    , response :: LB.ByteString
    } deriving (Show, Eq, Data, Typeable)
