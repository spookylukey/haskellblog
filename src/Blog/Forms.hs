{-# LANGUAGE DisambiguateRecordFields #-}

module Blog.Forms

where

import Blog.Formats (Format(..), getFormatter)
import Blog.Model (checkPassword, getSpamWords)
import Control.Monad (liftM)
import Data.List (isInfixOf)
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
import qualified Data.ByteString.Lazy.UTF8 as UTF8
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
                                            , captions = map (UTF8.toString . Ct.name) categories
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
               , postId = undefined
               , timestamp = undefined
               , name = LB.empty
               , email = LB.empty
               , textRaw = LB.empty
               , textFormatted = undefined
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
validateComment cn creds postedData blogpost =
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
      spamwords <- getSpamWords cn
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
                  , (not $ P.commentsOpen blogpost,
                     ("", "Post is closed for comments"))
                  , (length text > Settings.max_comment_message_size,
                     ("message", "Message too long"))
                  , (length email > Settings.max_comment_email_size,
                     ("email", "E-mail address too long"))
                  , (length name > Settings.max_comment_name_size,
                     ("name", "Name too long"))
                  , (test_ts == 0, -- field missing
                     ("timestamp", "Appears to be spam"))
                  , (ts < test_ts + 15,
                     ("timestamp", "That didn't take long to write! Spammer?"))
                  , (any (\w -> w `isInfixOf` text) spamwords,
                     ("text", "Spam.  No thanks."))
                  ]
      let errors = map snd $ filter fst $ tests

      return (Cm.Comment {
                      uid = 0 -- for sake of preview
                    , postId = P.uid blogpost
                    , timestamp = ts
                    , name = utf8 name
                    , email = utf8 email
                    , textRaw = utf8 text
                    , textFormatted = utf8 $ getFormatter format $ text
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
                   , postRaw = LB.empty
                   , postFormatted = undefined
                   , summaryRaw = LB.empty
                   , summaryFormatted = undefined
                   , format = RST
                   , timestamp = undefined
                   , commentsOpen = True
                   }

-- | Extract a 'post', the 'post categories' and any errors
-- from the POST request
validatePost req basePost = do
  let title = getPOST req "title" `captureOrDefault` ""
  let categories = catMaybes $ map capture $ getPOSTlist req "categories" :: [Int]
  let summaryRaw = getPOST req "summaryRaw" `captureOrDefault` ""
  let postRaw = getPOST req "postRaw" `captureOrDefault` ""
  let format = getPOST req "format" `captureOrDefault` Plaintext
  let commentsOpen = hasPOST req "commentsOpen"
  let tests = [ (null title,
                 ("title", "'Title' is a required field."))
              , (null summaryRaw,
                  ("summary", "'Summary' is a required field."))
              , (null postRaw,
                 ("post", "'Full post' is a required field."))
              ]
  let errors = map snd $ filter fst $ tests
  return (basePost { P.title = utf8 title
                   , P.summaryRaw = utf8 summaryRaw
                   , P.summaryFormatted = utf8 $ getFormatter Plaintext $ summaryRaw
                   , P.postRaw = utf8 postRaw
                   , P.postFormatted = utf8 $ getFormatter format $ postRaw
                   , P.format = format
                   , P.commentsOpen = commentsOpen
                   }
         , categories
         , errors
         )
