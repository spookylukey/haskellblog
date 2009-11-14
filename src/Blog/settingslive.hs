module Blog.Settings where

cgi_root_path = "/home/lukeplantuk/public_html/cgi-bin/"
sqlite_path = cgi_root_path ++ "/data/blog.db"
template_path = cgi_root_path ++ "/data/blogtemplates/"

root_url = "/blog/"
prog_uri = "/cgi-bin/blog.cgi" -- Used for redirecting
blog_author_name = "luke"
reserved_names = [blog_author_name]
admin_usernames = [blog_author_name]

post_page_size = 20 :: Int
comment_page_size = 30 :: Int
admin_post_page_size = 100 :: Int
feed_post_page_size = 8 :: Int
feed_comment_page_size = 15 :: Int
domain = "lukeplant.me.uk"
absolute_root = "http://" ++ domain

secret = undefined -- must be changed before building

max_comment_message_size = 5000 :: Int
max_comment_name_size = 100 :: Int
max_comment_email_size = 320 :: Int

-- Testing
testdb_sqlite_path = "/home/build/build/haskellblog/testsuite/test.db"

-- Migration time settings:

-- Not needed for live
old_data_path = undefined
redirect_file_template = undefined
redirect_file_output = undefined
syndication_redirect_file_template = undefined
syndication_redirect_file_output = undefined
