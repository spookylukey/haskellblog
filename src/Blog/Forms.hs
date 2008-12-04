module Blog.Forms

where

import Ella.Forms.Widgets

nameWidget = TextInput { defaultVal = ""
                       , size = Just 20
                       , maxlength = Just 50
                       , name = "name"
                       , identifier = Just "id_name"
                       }

emailWidget = TextInput { defaultVal = ""
                        , size = Just 20
                        , maxlength = Just 320
                        , name = "email"
                        , identifier = Just "id_email"
                        }
