{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Control.Applicative

import Text.HTML.Scalpel
import Text.HTML.TagSoup
import Data.List
import Data.Char (isSpace)


trim :: String -> String
trim = f . f
where f = reverse . dropWhile isSpace


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


fillTrue :: [Bool] -> [Bool]
fillTrue [] = []
fillTrue [a] = [a]
fillTrue (False:False:xs) = False:fillTrue(False:xs)
fillTrue (False:True:xs) = False:fillTrue(True:xs)
fillTrue (True:False:xs) = True:fillTrue(True:xs)
fillTrue (True:True:xs) = True:True:fillTrue(xs)




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
    let bool_mapping_open = map (isTagOpenName "pre") parsed_tags 
    let bool_mapping_close = map (isTagCloseName "pre") parsed_tags 
    -- create another list which is the result of the element-wise or of the two lists above
    let bool_mapping =  zipWith (||) bool_mapping_open bool_mapping_close 
    let filled_true = fillTrue bool_mapping
    -- zip the parsed_tags list with the boolean list and filter the elements of the first list based on the second list
    let combined = zip filled_true parsed_tags
    -- get the tuples from combined where the boolean is false
    let without_pre = map snd (filter (\(a,b) -> not a) combined)
    let withoutpre_text = trim (innerText without_pre)
    writeFile "output_files/without_pre_tags.txt" withoutpre_text
    

    -- writeFile "output_files/sitetext.txt" (innerText (filter isTagText (parseTags html_content) ) ) 
    -- print [fromTagText x | x <- (parseTags html_content), isTagText x]





