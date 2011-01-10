module Tests.Blog.DBUtils

where

import Blog.DBUtils
import Control.Exception (bracket)
import Database.HDBC
import Test.HUnit
import qualified Tests.Blog.TestDB as TestDB

withDB = bracket (TestDB.connect) (\cn -> do { disconnect cn; return () })

makeTestSlugTable cn = do
  quickQuery cn "CREATE TABLE slugs (id INTEGER PRIMARY KEY AUTOINCREMENT, title TEXT, slug TEXT);" []

insertSlug cn title slug = do
  quickQuery cn "INSERT INTO slugs (title, slug) VALUES (?, ?);" [toSql title, toSql slug]

slugFromTitle1 =  "this-is-a-title" ~=? (slugFromTitle "This is a % $ /title ^£$")
slugFromTitle2 =  "doesnt-work" ~=? (slugFromTitle "Doesn't work")

makeSlugGeneric1 =
    withDB (\cn ->
            do
              makeTestSlugTable cn
              slug1 <- makeSlugGeneric cn "This is a title" "slugs"
              assertEqual "" "this-is-a-title" slug1
              insertSlug cn "This is a title" slug1
              slug2 <- makeSlugGeneric cn "This is a title" "slugs"
              insertSlug cn "This is a title" slug2
              assertEqual "" "this-is-a-title2" slug2
           )

tests = test [
         slugFromTitle1,
         slugFromTitle2,
         TestCase makeSlugGeneric1
        ]
