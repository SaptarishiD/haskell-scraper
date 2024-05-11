{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

-- maybe can also build a progress bar thing for the matrix multiplications stuff so know how long it's taking
-- also need to do performance testing stuff and all

module Lib
    ( writeFullSrc, getHTML, parseTheTags, separateTextCode, writeToTxt, writeToDocx, getText, getWords, regextest, tokenizer, splitOnNewline, preProc, getUniqueWords, wordCounts, matrixRow, myVectorizer, sumCols, calcXGivenY, trainNaiveBayesSrc , trainNaiveBayesLang, classifyNaiveBayes
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
import qualified Data.List.Split as DLS
-- import Text.Regex.Posix
import Text.Regex.TDFA
-- import Text.Regex.TDFA.Text ()

import Data.Typeable
import Data.Map (fromListWith, toList)
import Data.Maybe (fromMaybe)
import qualified Data.Matrix as DM
import GHC.Float (int2Double)

type Document = String
type Vocabulary = [String]




classifyNaiveBayes :: String -> String -> DM.Matrix Int -> DM.Matrix Int -> Vocabulary -> Int -> Int -> [Int]
classifyNaiveBayes lang_test src_test trainedSrc trainedLang vocab src_len natural_len =
    let lang_test_data = lines lang_test
        src_test_data = lines src_test

        xTrain_src = trainedSrc
        xTrain_natural = trainedLang

        sourceCodeMatrix = xTrain_src
        naturalLanguageMatrix = xTrain_natural

        -- DM.Matrix Int -> [Int]
        sum_src_cols = sumCols sourceCodeMatrix
        sum_lang_cols = sumCols naturalLanguageMatrix


        xgivenY_src = calcXGivenY src_len sum_src_cols
        xgivenY_lang = calcXGivenY natural_len sum_lang_cols
        prob_src_prior = (int2Double src_len) / (int2Double src_len + fromIntegral natural_len)

        xTest_src = map (map int2Double) (DM.toLists (myVectorizer vocab (src_test_data)))
        xTest_lang = map (map int2Double) (DM.toLists (myVectorizer vocab (lang_test_data)))

        src_test_len = length (xTest_src)
        lang_test_len = length (xTest_lang)

        yTest = replicate src_test_len 0 ++ replicate lang_test_len 1

        xTest = xTest_src ++ xTest_lang

        y = DM.zero 1 (src_test_len + lang_test_len)

        -- [Double]
        log_src = map log xgivenY_src
        log_lang = map log xgivenY_lang

        log_matrix = DM.transpose (DM.fromLists [log_src, log_lang])

        -- [Double,Double]
        -- print (map typeOf (head (DM.toLists log_matrix)))

        prob1 = DM.multStrassen (DM.fromLists xTest) log_matrix

        -- [Double,Double]
        -- print (map typeOf (head (DM.toLists prob1)))

        logp = log prob_src_prior
        log_not_p = log (1 - prob_src_prior)

        prob1_trans = DM.toLists (DM.transpose prob1)

        prob2 = map (\x -> x + logp) (head prob1_trans)
        prob3 = map (\x -> x + log_not_p) (head (tail prob1_trans))

        combined = [prob2, prob3]
        combined_mat = DM.transpose (DM.fromLists combined)
        
        final_probs = map (\x -> if (head x) > (head (tail x)) then 0 else 1) (DM.toLists combined_mat)

    in final_probs




-- trainNaiveBayes :: [String] -> [String] -> ( ( [String] , Double ), ( [Double] , [Double] ))
trainNaiveBayesSrc :: [String] -> [String] -> DM.Matrix Int
trainNaiveBayesSrc natural_data source_data = 
    let source_words = concat (getWords source_data)
        natural_words = concat (getWords natural_data)
        unique_src_words = getUniqueWords source_words
        unique_natural_words = getUniqueWords natural_words
        vocab = unique_src_words ++ unique_natural_words
        xTrain_src = myVectorizer vocab (source_data)
    in xTrain_src
    

trainNaiveBayesLang :: [String] -> [String] -> DM.Matrix Int
trainNaiveBayesLang natural_data source_data = 
    let source_words = concat (getWords source_data)
        natural_words = concat (getWords natural_data)
        unique_src_words = getUniqueWords source_words
        unique_natural_words = getUniqueWords natural_words
        vocab = unique_src_words ++ unique_natural_words
        xTrain_lang = myVectorizer vocab (natural_data)
    in xTrain_lang
    

myVectorizer :: Vocabulary -> [Document] -> DM.Matrix Int
myVectorizer vocab docs = DM.fromLists [matrixRow vocab doc | doc <- docs]


matrixRow :: Vocabulary -> Document -> [Int]
matrixRow vocab doc = [fromMaybe 0 (lookup word counts) | word <- vocab]
  where counts = wordCounts doc

wordCounts :: Document -> [(String, Int)]
wordCounts doc = toList $ fromListWith (+) [(word, 1) | word <- words doc]





sumCols :: DM.Matrix Int -> [Int]
sumCols matrix = map sum (DM.toLists (DM.transpose matrix))


calcXGivenY :: Int -> [Int] -> [Double]
calcXGivenY mylen my_cols_len =  map (\x -> int2Double (x) + 0.001 / int2Double (mylen) + 0.9 ) my_cols_len


getUniqueWords :: [String] -> [String]
-- get all unique strings from list of strings
getUniqueWords = foldl (\seen x -> if x `elem` seen then seen else seen ++ [x]) []


-- you have a list of words as a vocabulary and a list of strings/documents. for each string, count the occurence of each word in that string. You should output XTrain which is a n × V dimensional matrix describing the n documents used for training your Naive Bayes classifier where V is the number of words in the vocabulary. The entry XTrain[i,j] is 1 if word j appears in the ith training document and 0 otherwise.















splitOnNewline :: [String] -> [[String]]
splitOnNewline = DLS.splitWhen (== "NEWLINE")


preProc :: [[String]] -> [String]
preProc splitted = filter (not . null) $ map unwords splitted

     

-- trim :: String -> String
-- trim = f . f
--   where f = Prelude.reverse . Prelude.dropWhile isSpace


-- define a function that takes a list of tag strings as input and returns a string that has as it's elements every single word of the textual content of the tag strings. Newlines should also be included here


regextest :: String -> String -> Bool
regextest input regex = input =~ (regex:: String) :: Bool

-- for this can use regex more but even that won't capture everything then could use tokenize from library so will need to see how that works
-- need to also make more robust cause full stops and stuff so could make more granular and use NLP libraries
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
    | '\n' `elem` x && length x == 1 = ["NEWLINE"]:getWords(xs)
    -- | words x == [] = x:getWords(xs) -- here need to check if newline to do newline, otherwise spaces also getting included which makes it bad 
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

