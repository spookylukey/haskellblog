{-# LANGUAGE DeriveDataTypeable #-}
module Blog.Category where

import Data.Typeable
import Data.Data

data Category = Category { uid :: Int,
                           name :: String,
                           slug :: String
                         } deriving (Show, Eq, Data, Typeable)

newCategory name = Category { uid = undefined
                            , name = name
                            , slug = undefined
                            }