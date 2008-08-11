{-# OPTIONS_GHC -fbang-patterns  #-}
module Blog.Utils where

import Data.Char
import System.Environment(getArgs)
import Text.Regex.Base
import Text.Regex.PCRE
import qualified Data.ByteString.Char8 as B

regexReplace !re !rep !source = go source []
 where go str res =
         if B.null str
             then B.concat . reverse $ res
             else case (str =~~ re) :: Maybe (B.ByteString, B.ByteString, B.ByteString) of
               Nothing -> B.concat . reverse $ (str:res)
               Just (bef, _ , aft) -> go aft (rep:bef:res)
