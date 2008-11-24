module Blog.Settings where

sqlite_path = "/home/luke/httpd/lukeplant.me.uk/web/cgi-bin/data/test1.db"
root_url = "/blog/"
prog_uri = "/cgi-bin/blog.cgi" -- Used for redirecting

-- Testing
testdb_sqlite_path = "/home/luke/devel/haskell/blog/testsuite/test.db"

-- Migration time settings:

old_data_path = "/home/luke/httpd/lukeplant.me.uk/web/blog/data/"
redirect_file_template = "/home/luke/devel/haskell/blog/src/blog.php.tpl"
redirect_file_output = "/home/luke/devel/haskell/blog/src/blog.php"
