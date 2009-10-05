module Blog.DB ( connect
               , commit
               , doInsert
               , mkInsertStatement
               , doUpdate
               , mkUpdateStatement
               )
where

import Database.HDBC (run, prepare, execute, commit, rollback, finish, SqlError(..), catchSql, throwSqlError)
import Database.HDBC.Sqlite3 (connectSqlite3)
import Data.List
import Data.Maybe (fromMaybe)

import Ella.GenUtils (exactParse)
import qualified Blog.Settings as Settings

connect = connectSqlite3 Settings.sqlite_path

doSql conn sql values = do
    stmnt <- prepare conn sql
    catchSql (do
               execute stmnt values
               commit conn
             )
              (\ e1@(SqlError s1 sne1 m1) -> do
                 rollback conn
                 finish stmnt
               `catchSql` \e2@(SqlError s2 sne2 m2) ->
                   throwSqlError SqlError { seState = s1, seNativeError = sne1,
                                            seErrorMsg = "Part 1: " ++ m1 ++
                                                         "; Part 2: " ++ m2 }
              )

doInsert conn table columns values =
    let sql = mkInsertStatement table columns
    in doSql conn sql values

doUpdate conn table columns values whereClause whereClauseVals =
    let sql = mkUpdateStatement table columns
    in doSql conn (sql ++ " " ++ whereClause ++ ";") (values ++ whereClauseVals)

mkInsertStatement table columns = let joinC = concat . intersperse ", "
                                      colSql = joinC columns
                                      valSql = joinC $ take (length columns) $ repeat "?"
                                   in "INSERT INTO " ++ table ++ 
                                      " (" ++ colSql ++ ")" ++
                                      " VALUES " ++
                                      " (" ++ valSql ++ ");"

mkUpdateStatement table columns = let joinC = concat . intersperse ", "
                                      valSql = joinC $ [c ++ "=?" | c <- columns]
                                  in "UPDATE " ++ table ++
                                     " SET " ++ valSql
