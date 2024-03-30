{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text
import Data.Text.IO
import Data.Text.Encoding
import Data.Text.Conversions

import Data.Char (isSpace)
import Data.ByteString      
import Text.HTML.Scalpel
import Text.Pandoc
import Text.Pandoc.Sources
import Text.Pandoc.Class (runIO)
import Text.Pandoc.Writers.Docx
import qualified Data.Text as T
import qualified Data.Text.IO as TIO



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
    heading1 = Text.HTML.Scalpel.text "h1"
    
    heading2 :: Scraper String String
    heading2 = Text.HTML.Scalpel.text "h2"


paraScraper :: IO (Maybe [String])
paraScraper = scrapeURL "https://eli.thegreenplace.net/2018/type-erasure-and-reification/" paragraphs

  where 
    paragraphs :: Scraper String [String] 
    paragraphs = chroots "p" paragraph
    
    paragraph :: Scraper String String
    paragraph = Text.HTML.Scalpel.text "p"


codeScraper :: IO (Maybe [String])
codeScraper = scrapeURL "https://eli.thegreenplace.net/2018/type-erasure-and-reification/" code_snippets

  where 
    code_snippets :: Scraper String [String] 
    code_snippets = chroots "pre" snippet
    
    snippet :: Scraper String String
    snippet = Text.HTML.Scalpel.text "pre"


------------------------------------------------------------------------------------------------------------------------
-- have the array of Maybe Strings from headingScraper. Need to bind the result of the IO operation to the headingResult



main :: IO ()
main = do
    headingResult <- headingScraper
    -- print headingResult
    case headingResult of
        Just x -> do
            let formatted = Prelude.unlines ( Prelude.map (\a -> "\n========================================\n" ++ a ++ "\n========================================\n") x)
            -- formatted is now one string with the formatted output
            -- now we need to put this into a docx file
            Prelude.writeFile "output_files/headings.md" formatted
            mydoc <- runIO $ readMarkdown def ( convertText(formatted :: String ) :: Text )
            print mydoc




        Nothing -> print "Could not find the required elements"











-- main :: IO ()
-- main = do 
--     headingResult <- headingScraper
--     case headingResult of
--         Just x -> do
--             result <- runIO $ do
--                 let textToConvert = Prelude.unlines ( Prelude.map (\a -> "\n========================================\n" ++ a ++ "\n========================================\n") x)
--                 Prelude.writeFile "output_files/headings.md" (Prelude.unlines (Prelude.map trim x) )
--                 doc <- readMarkdown def (Prelude.readFile "output_files/headings.md")
--                 docxFile <- writeDocx def doc
--                 Data.Text.IO.writeFile "output_files/headings.docx" (decodeLatin1 (toStrict docxFile) )
--                 writeRST def doc
--             rst <- handleError result
--             TIO.putStrLn rst
            
--         Nothing -> print "Could not find the required elements"











{-    
  headingResult <- headingScraper
  case headingResult of
    Just x  -> do
        let textToConvert = Prelude.unlines ( Prelude.map (\a -> "\n========================================\n" ++ a ++ "\n========================================\n") x)
        writeFile "output_files/headings.md" textToConvert
        let doc = runIO (readMarkdown def "output_files/headings.md") :: IO (Either PandocError Pandoc)
        -- let result = writePlain def doc
        -- let docx = writeDocx def markdown
        -- let txt = writePlain def markdown
        -- T.writeFile "output_files/headings.docx" (writeLazyByteString docx)
        -- T.writeFile "output_files/headings.txt" (writeLazyByteString txt)
        -- writeFile "output_file.txt" mydoc
        writeFile "output_files/headings.md" textToConvert

        

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
-}
