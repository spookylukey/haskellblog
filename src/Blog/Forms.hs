{-# LANGUAGE DisambiguateRecordFields #-}

module Blog.Forms

where

import Blog.Formats (Format(..), getFormatter)
import Blog.Model (checkPassword)
import Control.Monad (liftM)
import Data.Maybe (fromJust, isNothing, catMaybes)
import Ella.Forms.Base
import Ella.GenUtils (exactParse, getTimestamp, utf8)
import Ella.Param (captureOrDefault, Param(..))
import Ella.Request (getPOST, getPOSTlist, hasPOST)
import Data.String.Utils (strip)
import qualified Blog.Category as Ct
import qualified Blog.Comment as Cm
import qualified Blog.Post as P
import qualified Blog.Settings as Settings
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Ella.Forms.Widgets.RadioButtonList as RBL
import qualified Ella.Forms.Widgets.OptionList as OL
import qualified Text.XHtml as X

-- Widgets
formatNames = Map.fromList [ (Plaintext, "Plain text")
                           , (RST, "Restructured text")
                           , (Rawhtml, "HTML")
                           ]

formatWidget formats = RBL.RadioButtonList { selectedValue = ""
                                           , name = "format"
                                           , identifier = "id_format"
                                           , values = map (show . fromEnum) formats
                                           , captions = map (X.toHtml . fromJust . (\f -> Map.lookup f formatNames)) formats
                                           }

commentAllowedFormats =  [Plaintext, RST]

formatWidgetForComment c  = setVal (show $ fromEnum $ Cm.format c) (formatWidget commentAllowedFormats)

postAllowedFormats = [Plaintext, RST, Rawhtml]

formatWidgetForPost p = setVal (show $ fromEnum $ P.format p) (formatWidget postAllowedFormats)

categoriesWidget categories = OL.OptionList { selectedValues = []
                                            , name = "categories"
                                            , identifier = "id_categories"
                                            , values = map (show . Ct.uid) categories
                                            , captions = map Ct.name categories
                                            , multiple = True
                                            , size = 10
                                            }

categoriesWidgetForPost :: [Int] -> [Ct.Category] -> OL.OptionList
categoriesWidgetForPost catids categories = setVal (map show catids) (categoriesWidget categories)

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
               , name = LB.empty
               , email = LB.empty
               , text_raw = LB.empty
               , text_formatted = undefined
               , format = Plaintext
               , hidden = False
               , response = LB.empty
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
                      uid = 0 -- for sake of preview
                    , post_id = P.uid blogpost
                    , timestamp = ts
                    , name = utf8 name
                    , email = utf8 email
                    , text_raw = utf8 text
                    , text_formatted = utf8 $ getFormatter format $ text
                    , format = format
                    , hidden = False
                    , response = LB.empty
                    }
             , errors
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
            then return (loginData, [])
            else return (loginData, [("password", "Password not correct.")])
       else do
         return (loginData, errors)

emptyPost = P.Post { uid = undefined
                   , title = LB.empty
                   , slug = undefined
                   , post_raw = LB.empty
                   , post_formatted = undefined
                   , summary_raw = LB.empty
                   , summary_formatted = undefined
                   , format = RST
                   , timestamp = undefined
                   , comments_open = True
                   }

-- | Extract a 'post', the 'post categories' and any errors
-- from the POST request
validatePost req basePost = do
  let title = getPOST req "title" `captureOrDefault` ""
  let categories = catMaybes $ map capture $ getPOSTlist req "categories" :: [Int]
  let summary_raw = getPOST req "summary_raw" `captureOrDefault` ""
  let post_raw = getPOST req "post_raw" `captureOrDefault` ""
  let format = getPOST req "format" `captureOrDefault` Plaintext
  let comments_open = hasPOST req "comments_open"
  let tests = [ (null title,
                 ("title", "'Title' is a required field."))
              , (null summary_raw,
                  ("summary", "'Summary' is a required field."))
              , (null post_raw,
                 ("post", "'Full post' is a required field."))
              ]
  let errors = map snd $ filter fst $ tests
  return (basePost { P.title = utf8 title
                   , P.summary_raw = utf8 summary_raw
                   , P.summary_formatted = utf8 $ getFormatter Plaintext $ summary_raw
                   , P.post_raw = utf8 post_raw
                   , P.post_formatted = utf8 $ getFormatter format $ post_raw
                   , P.format = format
                   , P.comments_open = comments_open
                   }
         , categories
         , errors
         )
