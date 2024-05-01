{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module Lib
    ( writeFullSrc, getHTML, parseTheTags, separateTextCode, writeToTxt, writeToDocx, getText, tokenize, getWords, regextest, tokenizer
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
-- import Text.Regex.Posix
import Text.Regex.TDFA
-- import Text.Regex.TDFA.Text ()




-- trim :: String -> String
-- trim = f . f
--   where f = Prelude.reverse . Prelude.dropWhile isSpace


-- define a function that takes a list of tag strings as input and returns a string that has as it's elements every single word of the textual content of the tag strings. Newlines should also be included here



-- for this can use regex later but even that won't capture everything then could use tokenize from library so will need to see how that works
-- need to also make more robust cause full stops and stuff so could make more granular and use NLP libraries
tokenize :: [Soup.Tag String] -> [[String]]
tokenize [] = []
tokenize (x:xs) = case x of
    Soup.TagText text -> case (words (T.unpack . T.toLower . T.pack $ text)) of
        [] -> ["NEWLINE"]:tokenize (xs)
        (y:ys) -> stringTokenize (y:ys) : tokenize (xs)

    _ -> tokenize (xs)

    where stringTokenize [] = []
          stringTokenize (y : ys)

            | y == " " = "SPACE" : stringTokenize (ys)
            | y == "\n" = "NEWLINE" : stringTokenize (ys)
            | y `elem` ["for", "if", "else", "while", "return", "int", "float", "double", "char", "void", "bool", "string", "struct", "class", "public"] = "KEYWORD" : stringTokenize (ys)
            | all (\c -> c `elem` ['0'..'9']) y = "NUMBER" : stringTokenize (ys)
            | all (\c -> c `elem` ['a'..'z'] || c `elem` ['A'..'Z']) y = "WORD" : stringTokenize (ys)
            | all (\c -> c `elem` ['_', 'a'..'z'] || c `elem` ['_', 'A'..'Z']) y = "UNDERSCORE_WORD" : stringTokenize (ys)
            | all (\c -> c `elem` ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y','z','\'']) y = "APOS_WORD" : stringTokenize (ys)
            | all (\c -> c `elem` ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y','z','!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '-', '+', '=', '{', '}', '[', ']', '|', '\\', ':', ';', '"', '\'', '<', '>', ',', '.', '?', '/', '`', '~'] || c `elem` ['!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '-', '+', '=', '{', '}', '[', ']', '|', '\\', ':', ';', '"', '\'', '<', '>', ',', '.', '?', '/', '`', '~', 'A','B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y','Z']) y = "SYMB_WORD" : stringTokenize (ys)
            | all (\c -> c `elem` ['!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '-', '+', '=', '{', '}', '[', ']', '|', '\\', ':', ';', '"', '\'', '<', '>', ',', '.', '?', '/', '`', '~']) y = y : stringTokenize (ys)
            | all (\c -> c `elem` [' ', '\t']) y = "SPACE" : stringTokenize (ys)
            | all (\c -> c `elem` ['0', '1', '2', '3', '4', '5', '6', '7', '8','9','a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x','y','z'] || c `elem` ['A'..'Z']) y = "NUMWORD" : stringTokenize (ys)
            | otherwise = y : stringTokenize (ys)
        



regextest :: String -> String -> Bool
regextest input regex = input =~ (regex:: String) :: Bool


-- \169 is copyright sign
-- boundary \\b needed to match whole word and not substring like int in intuition
-- can add more detail to the tokenizer regexes later for stuff like fun() and struct->pointer and system.out etc

tokenizer :: [String] -> [(String,String)]
tokenizer [] = []
tokenizer (x:xs)
    | x == "NEWLINE" = (x,x):tokenizer(xs)
    | regextest x "\\b(for|if|else|while|return|int|float|double|char|void|bool|string|struct|class|public)\\b" = (x,"KEYWORD"):tokenizer(xs)
    | regextest x "\\b[a-zA-Z]+\\b" = (x,"WORD"):tokenizer(xs)
    | regextest x "\\b[a-zA-Z_]+\\b" = (x,"UNDERSCORE_WORD"):tokenizer(xs)
    | regextest x "\\b[0-9]+\\b" = (x,"NUMBER"):tokenizer(xs)
    | otherwise = (x,x):tokenizer(xs)



getWords :: [String] -> [[String]]
getWords [] = []
getWords (x:xs)
    | words x == [] = ["NEWLINE"]:getWords(xs)
    | otherwise = (words (T.unpack . T.toLower . T.pack $ x)):getWords(xs)
    

getText :: [Soup.Tag String] -> [String]
getText tagstrings = map Soup.fromTagText (Prelude.filter Soup.isTagText tagstrings)

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

