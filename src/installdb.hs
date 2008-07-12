import Network.CGI
import Text.XHtml

import Control.Exception
import Control.Monad
import Database.HDBC

import qualified DB

msgpage :: String -> Html
msgpage msg = body << paragraph << msg

cgiMain :: CGI CGIResult
cgiMain = 
    do
      sqlmsg <- liftIO runSql
      output $ renderHtml $ msgpage sqlmsg


main :: IO ()
main = runCGI $ handleErrors cgiMain
--main = runSql >>= putStr
  
{-runSql' = do
  c <- DB.connect
  res <- quickQuery c "Select 1 + 1;" []
  disconnect c
  return $ show res
-}

runSql :: IO String
runSql = catchDyn (do
	             c <- DB.connect
                     --dropTables c
                     --commit c
		     createTables c
                     commit c
		     insertRecords c
                     commit c
		     rs <- retrieveRecords c
                     return (
			 " Records inserted in table Test are: \n" ++
                         "*************************************\n" ++
			 (show rs) ++ "\n" ++
			 "*************************************\n\n" {- ++		
			 " The tables in your database are:    "     ++ 
			 "*************************************\n" ++
			 (concatMap show mi) ++ "\n" ++
			 "*************************************\n\n" -}
                            )
                  )
                   (\e -> return $ show (e :: SqlError))


createTables :: IConnection conn => conn -> IO ()
createTables c = do
	run c "create table Test(id integer not null, name varchar(255) not null);" [] 
        return ()

dropTables :: IConnection conn => conn -> IO ()
dropTables c = do
	run c "drop table Test;" []
        return ()
	
insertRecords :: IConnection conn => conn -> IO ()
insertRecords c = do
	run c "insert into Test(id,name) values (1,'Test1');" []
	run c "insert into Test(id,name) values (2,'Test2');" []
	run c "insert into Test(id,name) values (3,'Test3');" []
	run c "insert into Test(id,name) values (4,'Test4');" []
        return ()

retrieveRecords :: IConnection conn => conn -> IO [(Int,String)]
retrieveRecords c = do
	liftM (map getRow) (quickQuery c "select id, name from Test;" [])
	where
		getRow row = (fromSql (row!!0), fromSql (row!!1))

retrieveRecords' :: IConnection conn => conn -> IO [[SqlValue]] 
retrieveRecords' c = quickQuery c "select 1 + 1;" [] 



{-
getMetaInfo :: IConnection conn => conn -> IO [(String,[FieldDef])]
getMetaInfo c = do
	ts <- tables c
	mapM (\t -> describe c t >>= \cs -> return (t,cs)) ts
-}