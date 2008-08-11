module Blog.DB where

import Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3 {-, setBusyTimeout-})
import qualified Blog.Settings  as Settings
import List

connect = connectSqlite3 Settings.sqlite_path

doInsert conn table columns values = let stmnt = mkInsertStatement table columns
                                     in run conn stmnt values

mkInsertStatement table columns = let joinC = concat . intersperse ", "
                                      colSql = joinC columns
                                      valSql = joinC $ take (length columns) $ repeat "?"
                                   in "INSERT INTO " ++ table ++ 
                                      " (" ++ colSql ++ ")" ++
                                      " VALUES " ++
                                      " (" ++ valSql ++ ");"
