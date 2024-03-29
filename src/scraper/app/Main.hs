{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text
import Data.Char (isSpace)         
import Text.HTML.Scalpel
import Text.Pandoc.Builder
import Text.Pandoc.Writers.Docx
import qualified Data.Text.IO as T


-- trims the whitespace and newlines from a string. Possibly won't use this so that the formatting in the output is nice cause of the spaces
trim :: String -> String
trim = f . f
  where f = Prelude.reverse . Prelude.dropWhile isSpace



headingScraper :: IO (Maybe [String])
headingScraper = scrapeURL "https://eli.thegreenplace.net/2018/type-erasure-and-reification/" headings

  where -- need to be careful about the first header also cause that's h1
    headings :: Scraper String [String] 
    headings = (:) <$> heading1 <*> chroots "h2" heading2  -- easier way to do this?

    heading1 :: Scraper String String
    heading1 = text "h1"
    
    heading2 :: Scraper String String
    heading2 = text "h2"


paraScraper :: IO (Maybe [String])
paraScraper = scrapeURL "https://eli.thegreenplace.net/2018/type-erasure-and-reification/" paragraphs

  where 
    paragraphs :: Scraper String [String] 
    paragraphs = chroots "p" paragraph
    
    paragraph :: Scraper String String
    paragraph = text "p"


codeScraper :: IO (Maybe [String])
codeScraper = scrapeURL "https://eli.thegreenplace.net/2018/type-erasure-and-reification/" code_snippets

  where 
    code_snippets :: Scraper String [String] 
    code_snippets = chroots "pre" snippet
    
    snippet :: Scraper String String
    snippet = text "pre"


convertToDocx :: FilePath -> FilePath -> IO ()
convertToDocx txtFile docxFile = do
    txtContents <- T.readFile txtFile
    let mydoc = docx (plain (T.pack txtContents))
    T.writeFile docxFile $ writeDocx mydoc


main :: IO ()
main = do
  headingResult <- headingScraper
  case headingResult of
    Just x  -> do
        writeFile "output_files/headings.txt" (Prelude.unlines (Prelude.map trim x) )
        convertToDocx "output_files/headings.txt" "output_files/headings.docx"
    Nothing -> print "Could not find the required elements"
    -- return somehow or have more things in the Just x statement

  paraResult <- paraScraper
  case paraResult of
    Just x  -> writeFile "output_files/paragraphs.txt" (Prelude.unlines ( Prelude.map (\a -> "\n========================================\n" ++ a ++ "\n========================================\n") x))
    Nothing -> print "Could not find the required elements"

  codeResult <- codeScraper
  case codeResult of
    Just x  -> writeFile "output_files/code_snippets.txt" (Prelude.unlines ( Prelude.map (\a -> "\n========================================\n" ++ a ++ "\n========================================\n") x))
    Nothing -> print "Could not find the required elements"
