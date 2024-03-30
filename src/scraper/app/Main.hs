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
    writeFile "output_files/sitetext.txt" (innerText (filter isTagText (parseTags html_content) ) ) 
    -- print [fromTagText x | x <- (parseTags html_content), isTagText x]





