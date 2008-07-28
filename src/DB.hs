module DB where

import Database.HDBC (handleSqlError)
import Database.HDBC.Sqlite3 (connectSqlite3 {-, setBusyTimeout-})
import qualified Settings 

connect = connectSqlite3 Settings.sqlite_path

