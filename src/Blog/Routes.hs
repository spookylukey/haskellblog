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
         , "atom/" <+/> empty                         //-> allPostsFeedView       $ []
         , "posts/" <+/> anyParam                     //-> postView               $ []
         , "posts/" <+/> stringParam </+> "atom/"     //-> postCommentFeedView    $ []
         , "posts/" <+/> empty                        //-> postsRedirectView      $ []
         , "categories/" <+/> empty                   //-> categoriesView         $ []
         , "categories/" <+/> anyParam                //-> categoryView           $ []
         , "categories/" <+/> stringParam</+>"atom/"  //-> categoryPostsFeedView  $ []
         , "comments/" <+/> empty                     //-> allCommentsView        $ []
         , "comments/atom/" <+/> empty                //-> allCommentsFeedView    $ []
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
         , "admin/ajax/addspamword/" <+/> empty       //-> addSpamWordView        $ [adminRequired]
         , "debug/" <+/> anyParam                     //-> debug                  $ []
         ]
