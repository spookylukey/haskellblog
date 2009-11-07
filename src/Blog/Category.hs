{-# LANGUAGE DeriveDataTypeable #-}
module Blog.Category where

import Data.Typeable
import Data.Data
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.UTF8 as UTF8


data Category = Category { uid :: Int,
                           name :: LB.ByteString,
                           slug :: LB.ByteString
                         } deriving (Show, Eq, Data, Typeable)

newCategory name = Category { uid = undefined
                            , name = UTF8.fromString name
                            , slug = undefined
                            }
