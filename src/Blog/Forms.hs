{-# LANGUAGE DisambiguateRecordFields #-}

module Blog.Forms

where

import Ella.Forms.Widgets.TextInput (TextInput(..))
import Ella.Forms.Widgets.Textarea  (Textarea(..))

nameWidget = TextInput { value = ""
                       , size = Just 20
                       , maxlength = Just 50
                       , name = "name"
                       , identifier = "id_name"
                       }

emailWidget = TextInput { value = ""
                        , size = Just 20
                        , maxlength = Just 320
                        , name = "email"
                        , identifier = "id_email"
                        }

messageWidget = Textarea { value = ""
                         , cols = Just 60
                         , rows = Just 20
                         , name = "message"
                         , identifier = "id_message"
                         }
