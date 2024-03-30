{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Control.Applicative

import Text.HTML.Scalpel
import Text.HTML.TagSoup
import Data.List

trim :: String -> String
trim = dropWhileEnd (== ' ') . dropWhile (== ' ')


-- extractVisibleText :: String -> [Tag String]
-- extractVisibleText html_content = do 
--     parsed_tags <- parseTags html_content
--     let text_tags = filter isTagText (parseTags html_content)
--     -- print text_tags
--     return parsed_tags

-- filterTags :: [Tag String] -> [Tag String]

boolToString :: Bool -> String
boolToString True = "TRUE"
boolToString False = "FALSE"

main :: IO ()
main = do
    manager <- newTlsManager
    request <- parseRequest "GET https://eli.thegreenplace.net/2018/type-erasure-and-reification/"
    response <- httpLbs request manager
    let htmlcontent = responseBody response
    -- LBS.putStrLn htmlcontent

    writeFile "output_files/target.html" (LBS.unpack htmlcontent)

    html_content <- readFile "output_files/target.html"
    -- print html_content
    -- let filtered_parsed = filter (\x -> isTagOpen x || isTagClose x || isTagText x) (parseTags html_content)
    let parsed_tags = parseTags html_content
    -- print (length parsed_tags)
    -- get all the elements of the list that are not enclosed between an open pre tag and a close pre tag
    -- create a list of booleans whose elements are true if for the corresponding element in the above list, we have isTagOpenName = "pre" or isTagCloseName = "pre"
    -- use the above list to filter the elements of the first list
    -- let boolean_filter = map (\x -> isTagOpenName "pre" || isTagCloseName "pre") filtered_parsed
    -- print boolean_filter
    let bool_mapping = map boolToString ( map (isTagOpenName "pre") parsed_tags ) 
    print bool_mapping
    

    -- writeFile "output_files/sitetext.txt" (innerText (filter isTagText (parseTags html_content) ) ) 
    -- print [fromTagText x | x <- (parseTags html_content), isTagText x]





