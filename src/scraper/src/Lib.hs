{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( fillTrue, insertNewlines, separateTextCode, pandocProcessTags
    ) where

-- don't need to export insertnewlines?
-- also you should modularise further so that eliminate importing lbsc and tagsoup in main

import Text.HTML.TagSoup
import Text.Pandoc
import Data.Text.Conversions
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC


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
insertNewlines :: [Tag String] -> [Tag String]
insertNewlines [] = []
insertNewlines (x:xs) = if (isTagCloseName "pre" x)
    then TagText "\n\n=======================\n\n":x:insertNewlines (xs)
    else x:insertNewlines (xs)


{-
    Idea: get all the elements of the list that are not enclosed between an open pre tag and a close pre tag
    create a list of booleans whose elements are true if for the corresponding element in the tag list, we have isTagOpenName = "pre" or isTagCloseName = "pre"
    use the above list to filter the elements of the tag list
-}

separateTextCode :: [Tag String] -> ([Tag String], [Tag String])
separateTextCode parsed_tags =
    let bool_mapping_open = Prelude.map (isTagOpenName "pre") parsed_tags
        bool_mapping_close = Prelude.map (isTagCloseName "pre") parsed_tags

        -- do element-vise or of the two lists
        bool_mapping1 =  Prelude.zipWith (||) bool_mapping_open bool_mapping_close

        filled_true_pre = fillTrue bool_mapping1
        combined_pre = Prelude.zip filled_true_pre parsed_tags
        preTags = Prelude.map snd (Prelude.filter (fst) combined_pre)

        bool_mapping_img_open = Prelude.map (isTagOpenName "img") parsed_tags
        bool_mapping_img_close = Prelude.map (isTagCloseName "img") parsed_tags
        bool_mapping2 =  Prelude.zipWith (||) bool_mapping_img_open bool_mapping_img_close

        bool_mapping =  Prelude.zipWith (||) bool_mapping1 bool_mapping2

        filled_true = fillTrue bool_mapping
        -- zip parsed_tags list with the boolean list and Prelude.filter elements of the first list based on the second list to create tuple list
        combined = Prelude.zip filled_true parsed_tags
        -- get tags of tuples from combined where the boolean is false
        nonPreTags = Prelude.map snd (Prelude.filter (\(a,b) -> not a) combined)

    in (preTags, nonPreTags)


-- write the text into .docx and code into .txt
pandocProcessTags :: [Tag String] -> [Tag String] -> IO ()
pandocProcessTags preTags nonPreTags = do
    let htmlPre = renderTags (insertNewlines preTags)
    let htmlNonPre = renderTags nonPreTags

    pandocPre <- runIO $ readHtml def ( convertText (htmlPre :: String ) :: T.Text )

    case pandocPre of
        Right x -> do
            y <- runIO $ writePlain def x
            case y of
                Right direct_pan_pre -> do
                    TIO.writeFile "output_files/direct_sep_pre.txt" direct_pan_pre

                Left err -> Prelude.putStrLn $ "Error with pandoc writePlain: " ++ show err

        Left err -> Prelude.putStrLn $ "Error parsing pandoc for pre tags: " ++ show err


    pandocNoPre <- runIO $ readHtml def ( convertText (htmlNonPre :: String ) :: T.Text )

    case pandocNoPre of
        Right x -> do
            y <- runIO $ writeDocx def x
            case y of
                Right direct_pan -> do
                    LBS.writeFile "output_files/direct_html.docx" direct_pan

                Left err -> Prelude.putStrLn $ "Error with pandoc writeDocx: " ++ show err

        Left err -> Prelude.putStrLn $ "Error parsing pandoc for non pre tags: " ++ show err

