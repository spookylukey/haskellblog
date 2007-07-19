import Database.HSQL.MySQL (connect)
import Database.HSQL
import Control.Exception
import qualified Settings 

sqlDebug block = catchDyn block handler	
  where handler :: SqlError -> IO ()
        handler err = print err

main = sqlDebug (do
    c <- connect Settings.dbhost Settings.dbname Settings.dbusername Settings.dbpassword
    disconnect c
    return ())
