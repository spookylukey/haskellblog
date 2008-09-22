module Tests.Web.Response

where

import Web.Response
import Test.HUnit
import qualified Data.ByteString.Lazy.Char8 as BS

testAddContent1 = "Hello" ~=? (BS.unpack $ content $ addContent (BS.pack "Hello") $ emptyResponse)

tests = test [
         testAddContent1
        ]
