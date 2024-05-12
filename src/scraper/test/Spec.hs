{-# LANGUAGE OverloadedStrings #-}


module Main where

import Lib
import Test.HUnit
import Control.Exception (SomeException)
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Data.Matrix as DM
import Numeric.LinearAlgebra as NLA
import GHC.Float (int2Double)

import Data.List (sort)

-- Test data
vocab1 = ["apple", "banana", "orange"]
doc1 = "the apple is red"
doc2 = "banana is yellow"
doc3 = "this is a test document"



testHTML :: Test
testHTML = TestList [getHTMLValid, getHTMLBadURL, getHTMLNon200]

testUniqueWords = TestList [testEmpty, testUnique, testWithDup]


-- The main function that runs the tests
main :: IO Counts
main = do
    putStrLn "\nTesting getHTML\n"
    runTestTT testHTML
    putStrLn "\nTesting getWords\n"
    runTestTT testGetWords
    putStrLn "\nTesting getUniqueWords\n"
    runTestTT testUniqueWords
    putStrLn "\nTesting WordCounts\n"
    runTestTT testWordCounts
    putStrLn "\nTesting MatrixRow\n"
    runTestTT testMatrixRow
    putStrLn "\nTesting Vectorizer\n"
    runTestTT testMyVectorizer





-- trainingRelated ==========================================================
-- testWordCounts :: Test
testWordCounts = TestList [
    TestLabel "Empty document" $
    TestCase (assertEqual "wordCounts \"\" should return []" [] (wordCounts "")),

    TestLabel "Single word document" $
    TestCase (assertEqual "wordCounts \"hello\" should return [(\"hello\",1)]" [("hello",1)] (wordCounts "hello")),

    TestLabel "Multiple words document" $
    TestCase (assertEqual "wordCounts doc1 should return [(\"the\",1),(\"apple\",1),(\"is\",1),(\"red\",1)]" (sort [("the",1),("apple",1),("is",1),("red",1)]) (sort $ wordCounts doc1))
  ]

-- testMatrixRow :: Test
testMatrixRow = TestList [
    TestLabel "Empty vocabulary" $
    TestCase (assertEqual "matrixRow [] doc1 should return []" [] (matrixRow [] doc1)),

    TestLabel "Non-empty vocabulary" $
    TestCase (assertEqual "matrixRow vocab1 doc1 should return [1,0,0]" [1,0,0] (matrixRow vocab1 doc1)),

    TestLabel "Multiple documents" $
    TestCase (assertEqual "matrixRow vocab1 doc2 should return [0,1,0]" [0,1,0] (matrixRow vocab1 doc2))
  ]

-- testMyVectorizer :: Test
testMyVectorizer = TestList [
    TestLabel "Empty vocabulary and documents" $
    TestCase (assertEqual "myVectorizer [] [] should return an empty matrix" (NLA.fromLists [[0.0]]) (myVectorizer [] [])),

    TestLabel "Non-empty vocabulary and documents" $
    TestCase (assertEqual "myVectorizer vocab1 [doc1, doc2] should return [[1,0,0],[0,1,0]]" (NLA.fromLists [[1.0,0.0,0.0],[0.0,1.0,0.0]]) (myVectorizer vocab1 [doc1, doc2])),

    TestLabel "Multiple documents" $
    TestCase (assertEqual "myVectorizer vocab1 [doc1, doc2, doc3] should return [[1,0,0],[0,1,0],[0,0,0]]" (NLA.fromLists [[1.0,0.0,0],[0.0,1.0,0.0],[0.0,0.0,0.0]]) (myVectorizer vocab1 [doc1, doc2, doc3]))
    ]


-- getWords ==========================================================
testGetWords = TestList
  [ TestLabel "Empty String" $
    TestCase (assertEqual "getWords [] should return []" [] (getWords []))

  , TestLabel "Single string" $
    TestCase (assertEqual " " [["hello", "world"]] (getWords ["Hello World"]))

  , TestLabel "Multiple strings" $
    TestCase (assertEqual "getWords [\"Hello World \", \"World\", \"Test\"] should return [[\"hello\"],[\"world\"],[\"test\"]]" [["hello","world"],["world"],["test"]] (getWords ["Hello World", "World", "Test"]))

    , TestLabel "Large example" $
    TestCase (assertEqual "it should be correct" [["comments","are","fundamental","parts","of","a","computer","program,","where","you","leave"],["explanatory","remarks","to","yourself","and","others","that","may","be","collaborating","with"],["you","regarding","your"],["code."],["all","code","you","create","for","this","course","must","include","robust","comments."],["typically","each","comment","is","a","few","words","or","more,","providing","the","reader","an"],["opportunity","to","understand","what","is","happening","in","a","specific","block","of","code."],["further,","such","comments","serve","as","a","reminder","for","you","later","when","you","need","to"],["revise","your","code."],["comments","involve","placing","backslashes","into","your","code,","followed","by","a","comment.","modify","your","code","as","follows","to","integrate","comments:"],[]] (getWords (lines "Comments are fundamental parts of a computer program, where you leave \nexplanatory remarks to yourself and others that may be collaborating with \nyou regarding your \ncode.\nAll code you create for this course must include robust comments.\nTypically each comment is a few words or more, providing the reader an \nopportunity to understand what is happening in a specific block of code. \nFurther, such comments serve as a reminder for you later when you need to \nrevise your code.\nComments involve placing backslashes into your code, followed by a comment. Modify your code as follows to integrate comments:\n\n")))
  ]

-- UniqueWords ==========================================================
testEmpty :: Test
testEmpty = TestCase $ assertEqual "for (getUniqueWords [])," [] (getUniqueWords [])

testUnique :: Test
testUnique = TestCase $ assertEqual "for (getUniqueWords [\"these\", \"are\", \"words\"]),"
    ["these", "are", "words"] (getUniqueWords ["these", "are", "words"])

testWithDup :: Test
testWithDup = TestCase $ assertEqual "for (getUniqueWords [\"apple\", \"banana\", \"apple\", \"banana\", \"cherry\", \"apple\"]),"
    ["apple", "banana", "cherry"] (getUniqueWords ["apple", "banana", "apple", "banana", "cherry", "apple"])


-- GETHTML ==========================================================
-- testing exact content of the URL is not done since URL content can change thus making it possible for the test to fail even when it's correctly fetching the HTML

getHTMLValid :: Test
getHTMLValid = TestCase $ do
    result <- getHTML "https://example.com"
    case result of
        Left _ -> assertFailure "Expected Right, but got Left"
        Right _ -> return ()

getHTMLBadURL :: Test
getHTMLBadURL = TestCase $ do
    result <- getHTML "something thats not a valid url"
    case result of
        Left _ -> return ()
        Right _ -> assertFailure "Expected Left, but got Right"

getHTMLNon200 :: Test
getHTMLNon200 = TestCase $ do
    result <- getHTML "http://httpstat.us/404"
    case result of
        Left _ -> return ()
        Right _ -> assertFailure "Expected Left, but got Right"


