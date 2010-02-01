module Blog.Settings where

cgi_root_path = "/home/luke/httpd/lukeplant.me.uk/web/cgi-bin"
sqlite_path = cgi_root_path ++ "/data/blog.db"
template_path = cgi_root_path ++ "/data/blogtemplates/"

root_url = "/blog/"
prog_uri = "/cgi-bin/blog.cgi" -- Used for redirecting
blog_author_name = "luke"
reserved_names = [blog_author_name]
admin_usernames = [blog_author_name]
login_session_length = 3600 * 24 * 31 -- one month

post_page_size = 20 :: Int
comment_page_size = 30 :: Int
admin_post_page_size = 100 :: Int
feed_post_page_size = 8 :: Int
feed_comment_page_size = 15 :: Int
domain = "lukeplant_local"
absolute_root = "http://" ++ domain

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
syndication_redirect_file_template = "/home/luke/devel/haskell/blog/src/syndicate.php.tpl"
syndication_redirect_file_output = "/home/luke/devel/haskell/blog/src/syndicate.php"
