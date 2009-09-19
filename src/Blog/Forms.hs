{-# LANGUAGE DisambiguateRecordFields #-}

module Blog.Forms

where

import Blog.Formats (Format(..), getFormatter)
import Blog.Model (checkPassword)
import Control.Monad (liftM)
import Data.Maybe (fromJust, isNothing)
import Ella.Forms.Widgets.TextInput (TextInput(..))
import Ella.Forms.Widgets.Textarea  (Textarea(..))
import Ella.GenUtils (exactParse, getTimestamp)
import Ella.Param (captureOrDefault, Param(..))
import Data.String.Utils (strip)
import qualified Blog.Comment as Cm
import qualified Blog.Post as P
import qualified Blog.Settings as Settings
import qualified Data.Map as Map
import qualified Ella.Forms.Widgets.RadioButtonList as RBL
import qualified Ella.Forms.Widgets.TextInput as TI
import qualified Ella.Forms.Widgets.Textarea as TA
import qualified Text.XHtml as X

-- Widgets

nameWidget = TextInput { value = ""
                       , size = Just 20
                       , maxlength = Just Settings.max_comment_name_size
                       , name = "name"
                       , identifier = "id_name"
                       , password = False
                       }

emailWidget = TextInput { value = ""
                        , size = Just 20
                        , maxlength = Just $ Settings.max_comment_email_size
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
                         , cols = Just 80
                         , rows = Just 25
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

-- | Creates a timestamp/hash for hidden fields for spam protection.
initialCommentExtra req = getTimestamp >>= return

-- | extract the posted data from a POST request and build
-- a Comment from it, returning a Comment and a list of validation errors
validateComment creds postedData blogpost =
    do
    -- TODO - nicer mechanism for validation

    -- Spam protection
    --  - send a hidden field with deliberately incorrect name.
    --    A bit of javascript corrects the field name.
    --    Without this correction, submission fails.  This
    --    will catch most bots.
    --  - add a timestamp and a 10 second minimum time for adding comment.
    --    this will catch most humans using a browser

      ts <- getTimestamp
      let text = postedData "message" `captureOrDefault` ""
      let name = strip (postedData "name" `captureOrDefault` "")
      let email = postedData "email" `captureOrDefault` ""
      let format = postedData "format" `captureOrDefault` Plaintext
      let test_ts = postedData "timestamp" `captureOrDefault` 0 :: Int
      let tests = [ (null text,
                     ("message", "'Message' is a required field."))
                  , (not $ format `elem` commentAllowedFormats,
                     ("format", "Please choose a format from the list"))
                  , (name `elem` Settings.reserved_names && not (maybe False (==name) creds),
                     ("name", "That name is reserved."))
                  , (not $ P.comments_open blogpost,
                     ("", "Post is closed for comments"))
                  , (length text > Settings.max_comment_message_size,
                     ("message", "Message too long"))
                  , (length email > Settings.max_comment_email_size,
                     ("email", "E-mail address too long"))
                  , (length name > Settings.max_comment_name_size,
                     ("name", "Name too long"))
                  , (test_ts == 0, -- field missing
                     ("timestamp", "Appears to be spam"))
                  , (ts < test_ts + 10,
                     ("timestamp", "That didn't take long to write! Spammer?"))
                  ]
      let errors = map snd $ filter fst $ tests

      return (Cm.Comment {
                      uid = undefined
                    , post_id = P.uid blogpost
                    , timestamp = ts
                    , name = name
                    , email = email
                    , text_raw = text
                    , text_formatted = getFormatter format $ text
                    , format = format
                    }
             , Map.fromList errors
             , if test_ts > 0 then test_ts else ts
             )


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
