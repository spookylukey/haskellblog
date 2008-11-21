module Tests.Blog.TestDB where

import Database.HDBC.Sqlite3 (connectSqlite3)
import qualified Blog.Settings as Settings

-- TODO - make testdb_sqlite_path into a command-line argument so that
-- the tests can be run without having to change the settings
connect = connectSqlite3 Settings.testdb_sqlite_path
