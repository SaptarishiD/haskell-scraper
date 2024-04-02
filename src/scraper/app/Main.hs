{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Client
import Network.HTTP.Client.TLS


import Text.HTML.TagSoup
import qualified Text.HTML.TagSoup.Match as TagMatch

import Text.Pandoc

import Data.List
import Data.Char (isSpace)
import Data.Text.Lazy as TL
import Data.Text.Conversions
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC


trim :: String -> String
trim = f . f
  where f = Prelude.reverse . Prelude.dropWhile isSpace




fillTrue :: [Bool] -> [Bool]
fillTrue [] = []
fillTrue [a] = [a]
fillTrue (False:False:xs) = False:fillTrue (False:xs)
fillTrue (False:True:xs) = False:fillTrue (True:xs)
fillTrue (True:False:xs) = True:fillTrue (True:xs) -- main filling step where we fill in true for everything from the start of the pre tag
fillTrue (True:True:xs) = True:True:fillTrue (xs) -- if see 2 Trues in a row just skip both of them cause now pre tag has ended


-- inserts newlines before every instances of a particular element (tagclose pre) of the array
insertNewlines :: [Tag String] -> [Tag String]
insertNewlines [] = []
insertNewlines (x:xs) = if (isTagCloseName "pre" x)
    then TagText "\n\n========\n\n":x:insertNewlines (xs)
    else x:insertNewlines (xs)


-- write all the types of everything so that it's clear
-- type annotation

-- over here scraping twice which is bad cause two url requests. Can just use the reading of the html file now that we have it


-- know the types and stuff of everything
main :: IO ()
main = do
    
    mymanager <- newTlsManager
    myrequest <- parseRequest "GET https://eli.thegreenplace.net/2018/type-erasure-and-reification/"
    response <- httpLbs myrequest mymanager
    let response_html = responseBody response

    let parsed_tags = parseTags (LBSC.unpack response_html)

{-
    Idea: get all the elements of the list that are not enclosed between an open pre tag and a close pre tag
    create a list of booleans whose elements are true if for the corresponding element in the above list, we have isTagOpenName = "pre" or isTagCloseName = "pre"
    use the above list to Prelude.filter the elements of the first list

    so idea is get boolean list which has True corresponding to all pre tags and everything in between and remove those corresponding tags
    print parsed_tags
-}
    let bool_mapping_open = Prelude.map (isTagOpenName "pre") parsed_tags
    let bool_mapping_close = Prelude.map (isTagCloseName "pre") parsed_tags

    -- do element-vise or of the two lists
    let bool_mapping1 =  Prelude.zipWith (||) bool_mapping_open bool_mapping_close

    let filled_true_pre = fillTrue bool_mapping1
    let combined_pre = Prelude.zip filled_true_pre parsed_tags
    let with_pre = Prelude.map snd (Prelude.filter (\(a,b) -> a) combined_pre)

    -- idea can be that before every TagClose "pre" in this array of Tag Strs, you can insert a TagText delimiter with some string like "=====\n" which will delimit the code snippets

    let separated = insertNewlines with_pre
    -- print separated

    let html_pre = renderTags separated
    pand_pre_tags <- runIO $ readHtml def ( convertText (html_pre :: String ) :: T.Text )

    case pand_pre_tags of
        Right x -> do
            y <- runIO $ writePlain def x
            case y of
                Right direct_pan_pre -> do
                    TIO.writeFile "output_files/direct_sep_pre.txt" direct_pan_pre

                Left err -> Prelude.putStrLn $ "Error parsing pandoc: " ++ show err

        Left err -> print "Error direct html pre"

    let bool_mapping_img_open = Prelude.map (isTagOpenName "img") parsed_tags
    let bool_mapping_img_close = Prelude.map (isTagCloseName "img") parsed_tags
    let bool_mapping2 =  Prelude.zipWith (||) bool_mapping_img_open bool_mapping_img_close

    let bool_mapping =  Prelude.zipWith (||) bool_mapping1 bool_mapping2

    let filled_true = fillTrue bool_mapping
    -- zip parsed_tags list with the boolean list and Prelude.filter elements of the first list based on the second list to create tuple list
    let combined = Prelude.zip filled_true parsed_tags
    -- get tags of tuples from combined where the boolean is false
    let without_pre = Prelude.map snd (Prelude.filter (\(a,b) -> not a) combined)
    let pre_text = innerText (Prelude.map snd (Prelude.filter fst combined))
    -- print without_pre
    let without_pre_text_new = Prelude.filter (TagMatch.tagText (const True)) without_pre
    -- print without_pre_text_new
    let just_text_no_pre = Data.List.unlines (Prelude.map (trim . fromTagText) without_pre_text_new)

    let html_no_pre = renderTags without_pre
    -- print html_no_pre
    dir_pand_html <- runIO $ readHtml def ( convertText (html_no_pre :: String ) :: T.Text )
    -- print dir_pand_html

    case dir_pand_html of
        Right x -> do
            -- print x 
            y <- runIO $ writeDocx def x
            case y of
                Right direct_pan -> do
                    LBS.writeFile "output_files/direct_html.docx" direct_pan

                Left err -> Prelude.putStrLn $ "Error parsing pandoc: " ++ show err

        Left err -> print "Error direct html"

