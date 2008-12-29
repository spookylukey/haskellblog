{-# LANGUAGE DisambiguateRecordFields #-}

module Blog.Forms

where

import Ella.Forms.Widgets.TextInput (TextInput(..))
import Ella.Forms.Widgets.Textarea  (Textarea(..))
import qualified Ella.Forms.Widgets.TextInput as TI
import qualified Ella.Forms.Widgets.Textarea as TA
import Ella.Param (captureOrDefault)

import qualified Blog.Comment as Cm
import qualified Blog.Post as P
import Blog.Formats (plaintext)

import Data.Maybe (fromJust)
import System.Posix.Time (epochTime)
import System.Posix.Types

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


-- | An empty comment used for populating the default form.
emptyComment = Cm.Comment {
                 uid = undefined
               , post_id = undefined
               , timestamp = undefined
               , name = ""
               , email = ""
               , text_raw = ""
               , text_formatted = undefined
               , format_id = undefined
               }


-- | extract the posted data from a POST request and build
-- a Comment from it, returning a Comment and a list of validaion errors
validateComment postedData blogpost =
    do
    -- TODO - protect name -- some names are reversed for logged in users.
    -- TODO - posts that are closed for comments
    -- TODO - actual validation on fields
    -- TODO - addCommentToPost utility
      ts <- epochTime
      let text = postedData "message" `captureOrDefault` ""
      let name = postedData "name" `captureOrDefault` ""
      let email = postedData "email" `captureOrDefault` ""
      let errors = (if null text
                   then ["'Message' is a required field."]
                   else [])
                   ++
                   (if null name
                   then ["'Name' is a required field."]
                   else [])

      return (Cm.Comment {
                      uid = undefined
                    , post_id = P.uid blogpost
                    , timestamp = floor $ toRational ts
                    , name = name
                    , email = email
                    , text_raw = text
                    , text_formatted = text -- TODO fix, security
                    , format_id = plaintext
                    }, errors)

