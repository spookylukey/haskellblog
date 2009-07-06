{-# LANGUAGE BangPatterns, FlexibleContexts  #-}
module Blog.Utils where

import Data.Char
import System.Posix.Types
import Text.Regex.Base
import Text.Regex.PCRE
import qualified Data.ByteString.Lazy.Char8 as BL


-- | Replace using a regular expression. ByteString version
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
-- Could be implemented like this:
-- > regexReplace r rep t = regexReplaceCustom r (const rep) t


-- | Regex replace, with a function to transform matched strings. ByteString version
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


-- | Replace using a regular expression. String version
regexReplaceS ::
    (RegexMaker Regex CompOption ExecOption source) =>
    source                 -- ^ regular expression
    -> String              -- ^ replacement text
    -> String               -- ^ text to operate on
    -> String
regexReplaceS !regex !replacement !text = go text []
 where go str res =
           if null str
           then concat . reverse $ res
           else case (str =~~ regex) :: Maybe (String, String, String) of
                  Nothing -> concat . reverse $ (str:res)
                  Just (bef, _ , aft) -> go aft (replacement:bef:res)

-- | Regex replace, with a function to transform matched strings. String version
regexReplaceSCustom ::
  (RegexMaker Regex CompOption ExecOption source) =>
  source                               -- ^ regular expression
  -> (String -> String)                -- ^ transformation function applied to all matches
  -> String                            -- ^ text to operate on
  -> String
regexReplaceSCustom !regex replacef !text = go text []
 where go str res =
           if null str
           then concat . reverse $ res
           else case (str =~~ regex) :: Maybe (String, String, String) of
                  Nothing -> concat . reverse $ (str:res)
                  Just (bef, match , aft) -> go aft (replacef(match):bef:res)

split :: String -> Char -> [String]
split [] delim = [""]
split (c:cs) delim
   | c == delim = "" : rest
   | otherwise = (c : head rest) : tail rest
   where
       rest = split cs delim

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace find repl s =
    if take (length find) s == find
        then repl ++ (replace find repl (drop (length find) s))
        else [head s] ++ (replace find repl (tail s))

escapeHtmlString s = replace "<" "&lt;" $ replace ">" "&gt;" $ replace "\"" "&quot;" $ replace "&" "&amp;" s
