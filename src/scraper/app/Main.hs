{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Control.Applicative

import Text.HTML.Scalpel
import Text.HTML.TagSoup
import qualified Text.HTML.TagSoup.Match as TagMatch

import Text.Pandoc
import Text.Pandoc.Sources
import Text.Pandoc.Class (runIO)
import Text.Pandoc.Writers.Docx

import Data.List
import Data.Char (isSpace)
import Data.Text.Encoding
import Data.Text.Conversions
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as BytL
import qualified Data.ByteString.Lazy.Char8 as LBS


trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace


boolToString :: Bool -> String
boolToString True = "TRUE"
boolToString False = "FALSE"


fillTrue :: [Bool] -> [Bool]
fillTrue [] = []
fillTrue [a] = [a]
fillTrue (False:False:xs) = False:fillTrue(False:xs)
fillTrue (False:True:xs) = False:fillTrue(True:xs)
fillTrue (True:False:xs) = True:fillTrue(True:xs)
fillTrue (True:True:xs) = True:True:fillTrue(xs)


codeScraper :: IO (Maybe [String])
codeScraper = scrapeURL "https://eli.thegreenplace.net/2018/type-erasure-and-reification/" code_snippets

  where 
    code_snippets :: Scraper String [String] 
    code_snippets = chroots "pre" snippet
    
    snippet :: Scraper String String
    snippet = Text.HTML.Scalpel.text "pre"


-- write all the types of everything so that it's clear
-- type annotation

-- over here scraping twice which is bad cause two url requests. Can just use the reading of the html file now that we have it
main :: IO ()
main = do
    manager <- newTlsManager
    request <- parseRequest "GET https://eli.thegreenplace.net/2018/type-erasure-and-reification/"
    response <- httpLbs request manager
    let response_html = responseBody response

    -- writeFile "output_files/target.html" (LBS.unpack response_html)

    -- html_content <- readFile "output_files/target.html"
    let parsed_tags = parseTags (LBS.unpack response_html)

    -- get all the elements of the list that are not enclosed between an open pre tag and a close pre tag
    -- create a list of booleans whose elements are true if for the corresponding element in the above list, we have isTagOpenName = "pre" or isTagCloseName = "pre"
    -- use the above list to filter the elements of the first list

    -- so idea is get boolean list which has True corresponding to all pre tags and everything in between and remove those corresponding tags
    let bool_mapping_open = map (isTagOpenName "pre") parsed_tags 
    let bool_mapping_close = map (isTagCloseName "pre") parsed_tags 
    -- do element-vise or of the two lists
    let bool_mapping =  zipWith (||) bool_mapping_open bool_mapping_close 
    let filled_true = fillTrue bool_mapping
    -- zip parsed_tags list with the boolean list and filter elements of the first list based on the second list to create tuple list
    let combined = zip filled_true parsed_tags
    -- get tags of tuples from combined where the boolean is false
    let without_pre = map snd (filter (\(a,b) -> not a) combined)
    let pre_text = innerText (map snd (filter (\(a,b) -> a) combined))
    -- print without_pre
    let without_pre_text_new = filter (TagMatch.tagText (const True)) without_pre
    print without_pre_text_new

    {-
    let withoutpre_text = innerText without_pre
    -- i can trim this by using the lines function then mapping trim to each element but that would mess up the original formatting but that is fine
    -- also need to figure out out to demarcate that this is a heading and this is something else

    -- try to do something without converting to .md and back to docx
    -- see errors that can happen with the formatting here and try to clean up stuff
    writeFile "output_files/head_para.md" withoutpre_text
    pandocResult <- runIO $ readMarkdown def ( convertText(withoutpre_text :: String ) :: T.Text )
    case pandocResult of
        Right mypandoc -> do
            -- write pandoc to docx
            byte_docx <- runIO $ writeDocx def mypandoc
            case byte_docx of
                Right mypandoc1 -> do
                    BytL.writeFile "output_files/head_para.docx" mypandoc1
                    
                Left err -> Prelude.putStrLn $ "Error parsing pandoc: " ++ show err
                
        Left err -> Prelude.putStrLn $ "Error parsing the markdown: " ++ show err
    
    -- writeFile "output_files/new_code_snippets.txt" pre_text
    -- using scalpel it's better formatted with old method but can make it work with new method too in well formatted fashion
    codeResult <- codeScraper
    case codeResult of
        Just x  -> Prelude.writeFile "output_files/old_code_snippets.txt" (Prelude.unlines ( Prelude.map (\a -> "\n====================\n" ++ a ++ "\n====================\n") x))
        Nothing -> print "Code Scraping Error"


-}




