{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text
import Data.Char (isSpace)         
import Text.HTML.Scalpel


trim :: String -> String
trim = f . f
  where f = Prelude.reverse . Prelude.dropWhile isSpace

scraper :: IO (Maybe Text)
scraper = scrapeURL "https://eli.thegreenplace.net/2018/type-erasure-and-reification/" heading

  where
    heading :: Scraper Text Text
    heading = text "h2"

main :: IO ()
main = do
  result <- scraper
  case result of
    Just x  -> print (trim (unpack x))
    Nothing -> print "Didn't find the necessary items."