module Settings where

dbhost = "localhost"
dbname = "hdb_test_db"
dbusername = "tester"
dbpassword = "test"
odbc_dsn="Driver={MySQL ODBC 3.51 Driver};Server=" ++ dbhost ++ 
	";Database=" ++ dbname ++ 
	"; User=" ++ dbusername ++ 
	";Password=" ++ dbpassword ++ 
	";Option=3;"

sqlite_path="/home/luke/httpd/lukeplant.me.uk/web/cgi-bin/data/test1.db"
