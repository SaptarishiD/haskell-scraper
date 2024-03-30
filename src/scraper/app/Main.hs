{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text
import Data.Text.IO
import Data.Text.Encoding
import Data.Text.Conversions
import Control.Applicative

import Data.Char (isSpace)
import Data.ByteString     
import qualified Data.ByteString.Lazy as BytL
import Text.HTML.Scalpel
import Text.Pandoc
import Text.Pandoc.Sources
import Text.Pandoc.Class (runIO)
import Text.Pandoc.Writers.Docx
import qualified Data.Text as T
import qualified Data.Text.IO as TIO



-- trims the whitespace and newlines from a string. Possibly won't use this so that the formatting in the output is nice cause of the spaces
-- also write this better
trim :: String -> String
trim = f . f
  where f = Prelude.reverse . Prelude.dropWhile isSpace


-- heading1Scraper :: IO (Maybe [String])
-- heading1Scraper = scrapeURL "https://eli.thegreenplace.net/2018/type-erasure-and-reification/" heading1

--   where 

--     heading1 :: Scraper String String
--     heading1 = Text.HTML.Scalpel.text "h1"




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


-- headingToDocx:: Maybe [String] -> IO


-- headingParaScraper ::  IO (Maybe [String])
-- headingParaScraper = scrapeURL "https://eli.thegreenplace.net/2018/type-erasure-and-reification/" headparas
--   where
--     headparas = chroot "article" $ inSerial $ do 
--         title <- seekNext $ text "h1"
--         firstpara <- seekNext $ text "p"
--         sections <- many $ do
--             section <- seekNext $ text "h2"
--             ps <- untilNext (matches "h2") (many $ seekNext $ text "p")
--             return (section, ps)
--         return (title, firstpara, sections)



fetchHTML :: IO (Maybe String)
fetchHTML = scrapeURL "https://eli.thegreenplace.net/2018/type-erasure-and-reification/" htmlbody

  where 
    -- htmlbodies :: Scraper String [String] 
    -- htmlbodies = chroots "h1" htmlbody
    
    htmlbody :: Scraper String String
    htmlbody = innerHTML "body"

-- headparaScraper :: Scraper 

-- headparaScraper is a scraper that takes string as input and gives string as output
headparaScraper :: Scraper String (String, String, [(String, [String])])
headparaScraper = inSerial $ do
    title <- seekNext $ text "h1"
    firstpara <- seekNext $ text "p"
    sections <- many $ do
        section <- seekNext $ text "h2"
        ps <- untilNext (matches "h2") (many $ seekNext $ text "p")
        return (section, ps)
    return (title, firstpara, sections)    


------------------------------------------------------------------------------------------------------------------------
-- have the array of Maybe Strings from headingScraper. Need to bind the result of the IO operation to the headingResult

main :: IO ()
main = do
    result <- scrapeURL "https://eli.thegreenplace.net/2018/type-erasure-and-reification/" (chroot "body" headparaScraper)
    case result of
        Just x -> do
            print x
        Nothing -> print "Error"










{-



    headingResult <- headingScraper
    paraResult <- paraScraper
    codeResult <- codeScraper

    case headingResult of
        Just x -> do
            case paraResult of
                Just y -> do
                    let headingStart = "\n\nHeadings Start Here\n\n"
                    let formatted1 = Prelude.unlines ( Prelude.map (\a -> "\n====================\n" ++ a ++ "\n====================\n") x)

                    let paraStart = "\n\nParagraphs Start Here\n\n"
                    let formatted2 = Prelude.unlines ( Prelude.map (\a -> "\n====================\n" ++ a ++ "\n====================\n") y)

                    let formatted = headingStart ++ formatted1 ++ paraStart ++ formatted2

                    -- formatted is now one string with the formatted output
                    -- now we need to put this into a docx file
                    -- readMarkdown was giving error that no instance of pandocmonad IO, so need to run IO actions using runIO where $ is function application. def is default options. and need to convert String to Text. 
                    -- pandocResult is now a pandoc
                    Prelude.writeFile "output_files/head_para.md" formatted
                    pandocResult <- runIO $ readMarkdown def ( convertText(formatted :: String ) :: Text )
                    case pandocResult of
                        Right mypandoc -> do
                            -- write pandoc to docx
                            byte_docx <- runIO $ writeDocx def mypandoc
                            
                            case byte_docx of
                                Right mypandoc1 -> do
                                    BytL.writeFile "output_files/head_para.docx" mypandoc1
                                    
                                Left err -> Prelude.putStrLn $ "Error parsing pandoc: " ++ show err
                                
                        Left err -> Prelude.putStrLn $ "Error parsing Markdown: " ++ show err
                Nothing -> print "Paragraph Scraping Error" 
        Nothing -> print "Headings Scraping Error"
    
    case codeResult of
        Just x  -> Prelude.writeFile "output_files/code_snippets.txt" (Prelude.unlines ( Prelude.map (\a -> "\n====================\n" ++ a ++ "\n====================\n") x))
        Nothing -> print "Code Scraping Error"



-}    


