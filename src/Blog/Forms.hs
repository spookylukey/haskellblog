{-# LANGUAGE DisambiguateRecordFields #-}

module Blog.Forms

where

import Ella.Forms.Widgets.TextInput (TextInput(..))
import Ella.Forms.Widgets.Textarea  (Textarea(..))
import qualified Ella.Forms.Widgets.TextInput as TI
import qualified Ella.Forms.Widgets.Textarea as TA
import qualified Blog.Comment as Cm
import qualified Blog.Post as P
import Blog.Formats (plaintext)

import Data.Maybe (fromJust)
import System.Posix.Time (epochTime)


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

-- | Enum for the different stages of submitting a comment
data CommentStage = NoComment
                  | CommentPreview
                  | CommentInvalid
                  | CommentAccepted
                    deriving (Eq, Ord, Enum, Read, Show)


-- | extract the posted data from a POST request and build
-- a Comment from it, returning a Comment and a list of validaion errors
validateComment postedData blogpost =
    do
    -- TODO - protect name -- some names are reversed for logged in users.
    -- TODO - posts that are closed for comments
    -- TODO - actual validation on fields
    -- TODO - addCommentToPost utility
      ts <- epochTime
      let text = fromJust $ postedData "message"
      let errors = []
      return (Cm.Comment {
                      Cm.uid = undefined
                    , Cm.post_id = P.uid blogpost
                    , Cm.timestamp = ts
                    , Cm.name = fromJust $ Map.lookup "name" postedData
                    , Cm.email = fromJust $ Map.lookup "name" postedData
                    , Cm.text_raw = text
                    , Cm.text_formatted = text -- TODO fix, security
                    , Cm.format_id = plaintext
                    }, errors)

