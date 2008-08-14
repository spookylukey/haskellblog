module Blog.Category where

import Database.HDBC
import Blog.DBUtils (makeSlugGeneric)
import qualified Blog.DB as DB

data Category = Category { uid :: Int,
                           name :: String,
                           slug :: String
                         } deriving (Show, Eq)

addCategory cn c =  do theslug <- makeCategorySlug cn c
                       let c2 = c { slug = theslug }
                       DB.doInsert cn "categories"
                             ["name",
                              "slug"]
                             [toSql $ name c2,
                              toSql $ slug c2]
                       [[newid]] <- quickQuery cn "SELECT last_insert_rowid();" [];
                       return c2 { uid = fromSql $ newid }

makeCategorySlug cn cat = makeSlugGeneric cn (name cat) "categories"
