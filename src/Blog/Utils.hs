{-# LANGUAGE BangPatterns, FlexibleContexts  #-}
module Blog.Utils where

import Data.Char
import System.Posix.Types
import Text.Regex.Base
import Text.Regex.PCRE
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text.Lazy as LT

-- | Replace using a regular expression. ByteString version
regexReplace ::
    (RegexMaker Regex CompOption ExecOption source) =>
    source                 -- ^ regular expression
    -> LB.ByteString       -- ^ replacement text
    -> LB.ByteString       -- ^ text to operate on
    -> LB.ByteString
regexReplace !regex !replacement !text = go text []
 where go str res =
           if LB.null str
           then LB.concat . reverse $ res
           else case (str =~~ regex) :: Maybe (LB.ByteString, LB.ByteString, LB.ByteString) of
                  Nothing -> LB.concat . reverse $ (str:res)
                  Just (bef, _ , aft) -> go aft (replacement:bef:res)
-- Could be implemented like this:
-- > regexReplace r rep t = regexReplaceCustom r (const rep) t


-- | Regex replace, with a function to transform matched strings. ByteString version
regexReplaceCustom ::
  (RegexMaker Regex CompOption ExecOption source) =>
  source                               -- ^ regular expression
  -> (LB.ByteString -> LB.ByteString)  -- ^ transformation function applied to all matches
  -> LB.ByteString                     -- ^ text to operate on
  -> LB.ByteString
regexReplaceCustom !regex replacef !text = go text []
 where go str res =
           if LB.null str
           then LB.concat . reverse $ res
           else case (str =~~ regex) :: Maybe (LB.ByteString, LB.ByteString, LB.ByteString) of
                  Nothing -> LB.concat . reverse $ (str:res)
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

-- | Replace a string of Text in a Text with another Text
replaceLT find repl src
    | LT.null src = src
    | otherwise = let l = LT.length find
                  in if LT.take (fromIntegral l) src == find
                     then LT.append repl (replaceLT find repl (LT.drop (fromIntegral l) src))
                     else LT.cons (LT.head src) (replaceLT find repl (LT.tail src))

escapeHtmlStringT = repl "<" "&lt;" .
                    repl ">" "&gt;" .
                    repl "\"" "&quot;" .
                    repl "\'" "&#39;" .
                    repl "&" "&amp;"
    where repl x y = replaceLT (LT.pack x) (LT.pack y)

-- | Replace a ByteString in a ByteString with another ByteString
replaceBS find repl src
    | LB.null src = src
    | otherwise = let l = LB.length find
                  in if LB.take (fromIntegral l) src == find
                     then LB.append repl (replaceBS find repl (LB.drop (fromIntegral l) src))
                     else LB.cons (LB.head src) (replaceBS find repl (LB.tail src))

escapeHtmlStringBS = repl "<" "&lt;" .
                     repl ">" "&gt;" .
                     repl "\"" "&quot;" .
                     repl "\'" "&#39;" .
                     repl "&" "&amp;"
    where repl x y = replaceBS (LB.pack x) (LB.pack y)
