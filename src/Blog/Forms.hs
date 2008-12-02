module Blog.Forms

where

import Ella.Forms.Widgets

nameWidget = TextInput { defaultVal = ""
                       , size = Just 20
                       , maxlength = Just 50
                       }

emailWidget = TextInput { defaultVal = ""
                        , size = Just 20
                        , maxlength = Just 320
                        }
