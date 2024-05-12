{-# LANGUAGE OverloadedStrings #-}


module Spec where

import Lib
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.ByteString.Lazy.Char8 as LBSC




main :: IO ()
main = do
    defaultMain (testGroup "My Library Tests" [getHTMLTest])

getHTMLTestHelper :: String -> String
getHTMLTestHelper url = do
    response_html <- getHTML url
    case response_html of
      Left e -> do
        let error = head (words (show e))
        return error
      Right html -> do
        return (LBSC.unpack html)


getHTMLTestHelper1 :: String -> String
getHTMLTestHelper1 url = do
    getHTMLTestHelper url


getHTMLTest :: TestTree
getHTMLTest = testCase "Testing getHTML"
    (assertEqual "Should throw StatusCodeException!" "StatusCodeException" (getHTMLTestHelper1 "https://www.google.com/teapot"))

-- add5Test :: TestTree
-- add5Test = testCase "Testing add5"
--   (assertEqual "Should add 5 to get 10" 10 (add5 5))