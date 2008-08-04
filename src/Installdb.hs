
import Control.Exception
import Control.Monad
import Database.HDBC

import qualified DB

main :: IO ()
main = runSql >>= putStr

runSql :: IO String
runSql = catchDyn (do
	             c <- DB.connect
		     createTables c
                     commit c
                     return "OK\n"
                  )
                   (\e -> return $ show (e :: SqlError))

createTables :: IConnection conn => conn -> IO ()
createTables c = do
  let commands =
          ["\n\
           \  CREATE TABLE metainfo (\n\
           \    key TEXT,\n\
           \    value TEXT\n\
           \  );",
           "\n\
           \  CREATE TABLE formats (\n\
           \    id INTEGER PRIMARY KEY,\n\
           \    name TEXT,\n\
           \    posts_enabled INTEGER,\n\
           \    comments_enabled INTEGER\n\
           \  );",
           "\n\
           \  CREATE TABLE posts (\n\
           \    id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
           \    title TEXT,\n\
           \    slug TEXT,\n\
           \    post_raw TEXT,\n\
           \    post_formatted TEXT,\n\
           \    summary_raw TEXT,\n\
           \    summary_formatted TEXT,\n\
           \    format_id INTEGER REFERENCES formats(id),\n\
           \    timestamp INTEGER,\n\
           \    comments_open INTEGER\n\
           \  );",
           "\n\
           \  CREATE TABLE categories (\n\
           \    id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
           \    name TEXT,\n\
           \    slug TEXT\n\
           \  );",
           "\n\
           \  CREATE TABLE post_categories (\n\
           \    post_id INTEGER REFERENCES posts(id),\n\
           \    category_id INTEGER REFERENCES categories(id)\n\
           \  );",
           "\n\
           \  CREATE TABLE comments (\n\
           \    id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
           \    post_id INTEGER REFERENCES posts(id),\n\
           \    timestamp INTEGER,\n\
           \    name TEXT,\n\
           \    email TEXT,\n\
           \    text_raw TEXT,\n\
           \    text_formatted TEXT,\n\
           \    format_id INTEGER REFERENCES format(id)\n\
           \  ); \n\
           \ "]
  sequence_ $ map (\cmd -> run c cmd []) commands
