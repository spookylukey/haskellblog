module Blog.Routes where

import Blog.Views
import Blog.Processors
import Ella.Framework
import Ella.Processors.General (addSlashRedirectView)
import Ella.GenUtils (apply)

-- * Routes

-- These need to be manually synced with Blog.Links

views  = [ addSlashRedirectView
         , canonicalUri
         , empty                                      //-> mainIndex              $ []
         , "posts/" <+/> anyParam                     //-> postView               $ []
         , "posts/" <+/> empty                        //-> postsRedirectView      $ []
         , "categories/" <+/> empty                   //-> categoriesView         $ []
         , "categories/" <+/> anyParam                //-> categoryView           $ []
         , "login/" <+/> empty                        //-> loginView              $ []
         , "logout/" <+/> empty                       //-> logoutView             $ []
         , "admin/" <+/> empty                        //-> adminMenu              $ [adminRequired]
         , "admin/category/" <+/> empty               //-> adminCategories        $ [adminRequired]
         , "admin/post/" <+/> empty                   //-> adminPosts             $ [adminRequired]
         , "admin/post/new/" <+/> empty               //-> adminNewPost           $ [adminRequired]
         , "admin/post/edit/" <+/> anyParam           //-> adminEditPost          $ [adminRequired]
         , "admin/ajax/commentvisible/" <+/> empty    //-> adminCommentVisible    $ [adminRequired]
         , "admin/ajax/commentresponse/" <+/> empty   //-> adminCommentResponse   $ [adminRequired]
         , "admin/ajax/commentdelete/" <+/> empty     //-> adminCommentDelete     $ [adminRequired]
         , "debug/" <+/> anyParam                     //-> debug                  $ []
         ]
