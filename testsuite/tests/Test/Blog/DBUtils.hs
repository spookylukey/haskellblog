module Tests.Blog.DBUtils

where

import Blog.DBUtils
import Database.HDBC
import Test.HUnit
import qualified Test.Blog.TestDB as TestDB

makeTestSlugTable cn = do
  quickQuery cn "CREATE TABLE slugs (id INTEGER PRIMARY KEY AUTOINCREMENT, title TEXT, slug TEXT);" []

insertSlug cn title slug = do
  quickQuery cn "INSERT INTO slugs (title, slug) VALUES (?, ?);" [toSql title, toSql slug]

slugFromTitle1 =  "this-is-a-title" ~=? (slugFromTitle "This is a % $ /title ^£$")
makeSlugGeneric1 = do
  cn <- TestDB.connect;
  makeTestSlugTable cn
  slug1 <- makeSlugGeneric cn "This is a title" "slugs" 1
  assertEqual "" "this-is-a-title" slug1
  insertSlug cn "This is a title" slug1
  slug2 <- makeSlugGeneric cn "This is a title" "slugs" 1
  insertSlug cn "This is a title" slug2
  assertEqual "" "this-is-a-title2" slug2

tests = test [
         slugFromTitle1,
         TestCase makeSlugGeneric1
        ]
