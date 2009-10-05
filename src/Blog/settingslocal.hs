module Blog.Settings where

cgi_root_path = "/home/luke/httpd/lukeplant.me.uk/web/cgi-bin"
sqlite_path = cgi_root_path ++ "/data/test1.db"
template_path = cgi_root_path ++ "/data/blogtemplates/"

root_url = "/blog/"
prog_uri = "/cgi-bin/blog.cgi" -- Used for redirecting
blog_author_name = "luke"
reserved_names = [blog_author_name]
admin_usernames = [blog_author_name]

post_page_size = 20 :: Int
domain = "lukeplant_local"

secret = "123"

max_comment_message_size = 5000 :: Int
max_comment_name_size = 100 :: Int
max_comment_email_size = 320 :: Int

-- Testing
testdb_sqlite_path = "/home/luke/devel/haskell/blog/testsuite/test.db"

-- Migration time settings:

old_data_path = "/home/luke/httpd/lukeplant.me.uk/web/blog/data/"
redirect_file_template = "/home/luke/devel/haskell/blog/src/blog.php.tpl"
redirect_file_output = "/home/luke/devel/haskell/blog/src/blog.php"
