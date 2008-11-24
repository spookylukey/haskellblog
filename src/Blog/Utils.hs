{-# OPTIONS_GHC -fbang-patterns  #-}
module Blog.Utils where

import Data.Char
import System.Environment(getArgs)
import Text.Regex.Base
import Text.Regex.PCRE
import qualified Data.ByteString.Lazy.Char8 as BL

regexReplace !re !rep !source = go source []
 where go str res =
         if BL.null str
             then BL.concat . reverse $ res
             else case (str =~~ re) :: Maybe (BL.ByteString, BL.ByteString, BL.ByteString) of
               Nothing -> BL.concat . reverse $ (str:res)
               Just (bef, _ , aft) -> go aft (rep:bef:res)

split :: String -> Char -> [String]
split [] delim = [""]
split (c:cs) delim
   | c == delim = "" : rest
   | otherwise = (c : head rest) : tail rest
   where
       rest = split cs delim
