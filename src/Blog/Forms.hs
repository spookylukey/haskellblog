{-# LANGUAGE DisambiguateRecordFields #-}

module Blog.Forms

where

import Blog.Formats (Format(..), getFormatter)
import Blog.Model (checkPassword)
import Blog.Utils (getTimestamp)
import Control.Monad (liftM)
import Data.Maybe (fromJust)
import Ella.Forms.Widgets.TextInput (TextInput(..))
import Ella.Forms.Widgets.Textarea  (Textarea(..))
import Ella.GenUtils (exactParse)
import Ella.Param (captureOrDefault, Param(..))
import Data.String.Utils (strip)
import qualified Blog.Comment as Cm
import qualified Blog.Post as P
import qualified Data.Map as Map
import qualified Ella.Forms.Widgets.RadioButtonList as RBL
import qualified Ella.Forms.Widgets.TextInput as TI
import qualified Ella.Forms.Widgets.Textarea as TA
import qualified Text.XHtml as X

-- Widgets

nameWidget = TextInput { value = ""
                       , size = Just 20
                       , maxlength = Just 50
                       , name = "name"
                       , identifier = "id_name"
                       , password = False
                       }

emailWidget = TextInput { value = ""
                        , size = Just 20
                        , maxlength = Just 320
                        , name = "email"
                        , identifier = "id_email"
                        , password = False
                        }

commentAllowedFormats =  [Plaintext, RST]

formatWidget = RBL.RadioButtonList { value = ""
                                   , name = "format"
                                   , identifier = "id_format"
                                   , values = map (show . fromEnum) commentAllowedFormats
                                   , captions = map X.toHtml ["Plain text", "Restructured text"]
                                   }

messageWidget = Textarea { value = ""
                         , cols = Just 60
                         , rows = Just 20
                         , name = "message"
                         , identifier = "id_message"
                         }

passwordWidget = TextInput { value = ""
                           , size = Just 20
                           , maxlength = Just 20
                           , name = "password"
                           , identifier = "id_password"
                           , password = True
                           }

usernameWidget = nameWidget { TI.name = "username"
                            , TI.identifier = "id_username"
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
               , format = Plaintext
               }

instance Param Format where
    -- Read integer then convert.
    -- TODO error handling for out of bounds.
    capture s = liftM toEnum $ exactParse s

-- | extract the posted data from a POST request and build
-- a Comment from it, returning a Comment and a list of validation errors
validateComment creds postedData blogpost =
    do
    -- TODO - protect name -- some names are reserved for logged in users.
    -- TODO - posts that are closed for comments
    -- TODO - nicer mechanism for validation
    -- TODO - validate lengths of fields
    -- TODO - CSRF protection

    -- TODO - Spam protection
    --    Method - add 10 second minimum time for adding comment.  On
    --             first request, send back field with hash of time +
    --             IP address + secret, and field with time only. Time
    --             and hash fields are propagated if the user presses
    --             preview.  If hash doesn't match when user presses
    --             submit or if timedelta less than 10 seconds,
    --             emit validation error.

      ts <- getTimestamp
      let text = postedData "message" `captureOrDefault` ""
      let name = strip (postedData "name" `captureOrDefault` "")
      let email = postedData "email" `captureOrDefault` ""
      let format = postedData "format" `captureOrDefault` Plaintext
      let errors = (if null text
                   then [("message", "'Message' is a required field.")]
                   else []) ++
                   (if not $ format `elem` commentAllowedFormats
                    then [("format", "Please choose a format from the list")]
                    else [])

      return (Cm.Comment {
                      uid = undefined
                    , post_id = P.uid blogpost
                    , timestamp = ts
                    , name = name
                    , email = email
                    , text_raw = text
                    , text_formatted = getFormatter format $ text
                    , format = format
                    }, Map.fromList errors)


emptyLoginData = Map.fromList [("username", "")
                              ,("password", "")]

validateLogin postedData cn = do
  -- TODO - validation on field lengths
    let username = strip (postedData "username" `captureOrDefault` "")
    let password = postedData "password" `captureOrDefault` ""
    let loginData = Map.fromList [ ("username", username)
                                 , ("password", password)]
    let errors = (if null username
                  then [("username", "Please enter a user name")]
                  else [])
                 ++
                 (if null password
                  then [("password", "Please enter a password")]
                  else [])
    if null errors
       then do
         passwordCheck <- checkPassword cn username password
         if passwordCheck
            then return (loginData, Map.empty)
            else return (loginData, Map.fromList [("password", "Password not correct.")])
       else do
         return (loginData, Map.fromList errors)
