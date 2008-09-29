{-# OPTIONS_GHC -fglasgow-exts -XOverloadedStrings #-}
module Tests.Web.Framework

where

import Test.HUnit
import Web.Framework
import Web.Request
import Web.Response
import Data.Maybe (isNothing)
import Control.Monad (liftM)

req1 = mkRequest [("REQUEST_METHOD","GET")] ""
resp1 = buildResponse utf8HtmlResponse [ addContent "resp1" ]
resp2 = buildResponse utf8HtmlResponse [ addContent "resp2" ]

alwaysFailView = const (return Nothing)
alwaysSucceedView1 = const (return $ Just resp1)
alwaysSucceedView2 = const (return $ Just resp2)

testDispatchRequest1 = (dispatchRequest req1 [] 
                        >>= return . isNothing)
                       ~? "With no views, nothing is dispatched"

testDispatchRequest2 = (dispatchRequest req1 [alwaysFailView] 
                        >>= return . isNothing)
                       ~? "Should get Nothing if all view return Nothing"

testDispatchRequest3 = (do
                         resp <- dispatchRequest req1 [alwaysFailView,
                                                       alwaysSucceedView1,
                                                       alwaysSucceedView2]
                         return $ (resp == (Just resp1) && resp /= (Just resp2)))
                       ~? "Dispatch should return first that succeeds"

tests = test [
         testDispatchRequest1
        , testDispatchRequest2
        , testDispatchRequest3
        ]
