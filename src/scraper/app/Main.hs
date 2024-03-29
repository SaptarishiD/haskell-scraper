{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text
import Data.Char (isSpace)         
import Text.HTML.Scalpel
import Text.Read (Lexeme(String))

-- trims the whitespace and newlines from a string. Possibly won't use this so that the formatting in the output is nice cause of the spaces
trim :: String -> String
trim = f . f
  where f = Prelude.reverse . Prelude.dropWhile isSpace

myScraper :: IO (Maybe [String])
myScraper = scrapeURL "https://eli.thegreenplace.net/2018/type-erasure-and-reification/" headings
  where
    headings :: Scraper String [String] 
    headings = chroots "h2" heading -- need to be careful about the first header also cause that's h1

    heading :: Scraper String String
    heading = text "h2"

main :: IO ()
main = do
  myResult <- myScraper
  case myResult of
    Just x  -> print (Prelude.map trim x)
    Nothing -> print "Could not find the required elements"