module Blog.Category where

import Database.HDBC
import Blog.DBUtils (makeSlugGeneric)
import qualified Blog.DB as DB

data Category = Category { uid :: Int,
                           name :: String,
                           slug :: String
                         } deriving (Show, Eq)

