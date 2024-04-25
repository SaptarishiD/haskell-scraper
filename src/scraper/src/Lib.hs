{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( writeFullSrc, getHTML, parseTheTags, separateTextCode, writeToTxt, writeToDocx
    ) where


import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as ClientTLS

import qualified Text.HTML.TagSoup as Soup
import Text.Pandoc

import qualified Data.Text.Conversions as TextConv
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC


writeFullSrc :: [Soup.Tag String] -> IO ()
writeFullSrc tagstrings = writeFile "output_files/full_text.txt" (Soup.innerText tagstrings)


getHTML :: String -> IO LBSC.ByteString
getHTML url = do
    mymanager <- ClientTLS.newTlsManager
    myrequest <- Client.parseRequest url
    response <- Client.httpLbs myrequest mymanager
    let response_html = Client.responseBody response
    return response_html


parseTheTags :: LBSC.ByteString -> [Soup.Tag String]
parseTheTags response_html = (Soup.parseTags :: String -> [Soup.Tag String]) (LBSC.unpack response_html)



fillTrue :: [Bool] -> [Bool]
fillTrue [] = []
fillTrue [a] = [a]
fillTrue (False:False:xs) = False:fillTrue (False:xs)
fillTrue (False:True:xs) = False:fillTrue (True:xs)

-- fill True for elements after a <pre> tag starts
fillTrue (True:False:xs) = True:fillTrue (True:xs) 

-- seeing 2 consecutive Trues denotes end of <pre> tag, so skip them
fillTrue (True:True:xs) = True:True:fillTrue (xs) 




-- inserts a delimiter before every closing <pre> tag for formatted output
insertNewlines :: [Soup.Tag String] -> [Soup.Tag String]
insertNewlines [] = []
insertNewlines (x:xs) = if (Soup.isTagCloseName "pre" x)
    then Soup.TagText "\n\n=======================\n\n":x:insertNewlines (xs)
    else x:insertNewlines (xs)





{-
    Idea: get all the elements of the list that are not enclosed between an open pre tag and a close pre tag
    create a list of booleans whose elements are true if for the corresponding element in the tag list, we have isTagOpenName = "pre" or isTagCloseName = "pre"
    use the above list to filter the elements of the tag list
-}

separateTextCode :: [Soup.Tag String] -> ([Soup.Tag String], [Soup.Tag String])
separateTextCode parsed_tags =
    let bool_mapping_open = Prelude.map (Soup.isTagOpenName "pre") parsed_tags
        bool_mapping_close = Prelude.map (Soup.isTagCloseName "pre") parsed_tags

        -- do element-vise or of the two lists
        bool_mapping1 =  Prelude.zipWith (||) bool_mapping_open bool_mapping_close

        filled_true_pre = fillTrue bool_mapping1
        combined_pre = Prelude.zip filled_true_pre parsed_tags
        preTags = Prelude.map snd (Prelude.filter (fst) combined_pre)

        bool_mapping_img_open = Prelude.map (Soup.isTagOpenName "img") parsed_tags
        bool_mapping_img_close = Prelude.map (Soup.isTagCloseName "img") parsed_tags
        bool_mapping2 =  Prelude.zipWith (||) bool_mapping_img_open bool_mapping_img_close

        bool_mapping =  Prelude.zipWith (||) bool_mapping1 bool_mapping2

        filled_true = fillTrue bool_mapping
        -- zip parsed_tags list with the boolean list and Prelude.filter elements of the first list based on the second list to create tuple list
        combined = Prelude.zip filled_true parsed_tags
        -- get tags of tuples from combined where the boolean is false
        nonPreTags = Prelude.map snd (Prelude.filter (\(a,_) -> not a) combined)

    in (preTags, nonPreTags)



-- write the text into .docx and code into .txt

writeToTxt :: [Soup.Tag String] -> String -> IO ()
writeToTxt preTags filepath = do
    let htmlPre = Soup.renderTags (insertNewlines preTags)
    pandocPre <- runIO $ readHtml def ( TextConv.convertText (htmlPre :: String ) :: T.Text )

    case pandocPre of
        Right x -> do
            y <- runIO $ writePlain def x
            case y of
                Right direct_pan_pre -> do
                    TIO.writeFile filepath direct_pan_pre

                Left err -> Prelude.putStrLn $ "Error with pandoc writePlain: " ++ show err

        Left err -> Prelude.putStrLn $ "Error parsing pandoc for pre tags: " ++ show err
    
    putStrLn "Completed writing to txt"


writeToDocx :: [Soup.Tag String] -> String -> IO ()
writeToDocx nonPreTags filepath = do
    let htmlNonPre = Soup.renderTags nonPreTags
    pandocNoPre <- runIO $ readHtml def ( TextConv.convertText (htmlNonPre :: String ) :: T.Text )

    case pandocNoPre of
        Right x -> do
            y <- runIO $ writeDocx def x
            case y of
                Right direct_pan -> do
                    LBS.writeFile filepath direct_pan

                Left err -> Prelude.putStrLn $ "Error with pandoc writeDocx: " ++ show err

        Left err -> Prelude.putStrLn $ "Error parsing pandoc for non pre tags: " ++ show err

    putStrLn "Completed writing to docx"

