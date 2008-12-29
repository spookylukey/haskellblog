{-# LANGUAGE BangPatterns, FlexibleContexts  #-}
module Blog.Utils where

import Data.Char
import System.Environment(getArgs)
import Text.Regex.Base
import Text.Regex.PCRE
import qualified Data.ByteString.Lazy.Char8 as BL


-- | Replace using a regular expression.
regexReplace ::
    (RegexMaker Regex CompOption ExecOption source) =>
    source                 -- ^ regular expression
    -> BL.ByteString       -- ^ replacement text
    -> BL.ByteString       -- ^ text to operate on
    -> BL.ByteString
regexReplace !regex !replacement !text = go text []
 where go str res =
           if BL.null str
           then BL.concat . reverse $ res
           else case (str =~~ regex) :: Maybe (BL.ByteString, BL.ByteString, BL.ByteString) of
                  Nothing -> BL.concat . reverse $ (str:res)
                  Just (bef, _ , aft) -> go aft (replacement:bef:res)

-- | Regex replace, with a function to transform matched strings
regexReplaceCustom ::
  (RegexMaker Regex CompOption ExecOption source) =>
  source                               -- ^ regular expression
  -> (BL.ByteString -> BL.ByteString)  -- ^ transformation function applied to all matches
  -> BL.ByteString                     -- ^ text to operate on
  -> BL.ByteString
regexReplaceCustom !regex replacef !text = go text []
 where go str res =
           if BL.null str
           then BL.concat . reverse $ res
           else case (str =~~ regex) :: Maybe (BL.ByteString, BL.ByteString, BL.ByteString) of
                  Nothing -> BL.concat . reverse $ (str:res)
                  Just (bef, match , aft) -> go aft (replacef(match):bef:res)

split :: String -> Char -> [String]
split [] delim = [""]
split (c:cs) delim
   | c == delim = "" : rest
   | otherwise = (c : head rest) : tail rest
   where
       rest = split cs delim
