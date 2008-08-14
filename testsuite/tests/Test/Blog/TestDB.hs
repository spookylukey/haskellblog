module Test.Blog.TestDB where

import Database.HDBC.Sqlite3 (connectSqlite3)
import qualified Blog.Settings as Settings

connect = connectSqlite3 Settings.testdb_sqlite_path
