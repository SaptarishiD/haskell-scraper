{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant return" #-}

-- maybe can also build a progress bar thing for the matrix multiplications stuff so know how long it's taking
-- also need to do performance testing stuff and all

module Lib
    ( writeFullSrc, getHTML, parseTheTags, separateTextCode, writeToTxt, writeToDocx, getText, getWords, regextest, regexTokenizer, splitOnNewline, preProc, getUniqueWords, wordCounts, matrixRow, myVectorizer, sumCols, calcXGivenY, readTraining, trainNaiveBayes, classifyNaiveBayes , evaluateNaiveBayes, evalTests
    ) where

import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as ClientTLS
import Network.HTTP.Types.Status (statusCode)
import Control.Exception

import qualified Text.HTML.TagSoup as Soup
import Text.Pandoc

import Data.Typeable
import Data.Map (fromListWith, toList)
import Data.Maybe (fromMaybe)
import qualified Data.Matrix as DM
import GHC.Float (int2Double)
import qualified Data.Text.Conversions as TextConv
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Data.List.Split as DLS
-- import Text.Regex.Posix
import Text.Regex.TDFA
-- import Text.Regex.TDFA.Text ()

import Numeric.LinearAlgebra as NLA

data MyException = StatusCodeException
    deriving Show

instance Exception MyException

type Document = String
type Vocabulary = [String]
type Dummy  = String


-- can do case by case exceptions for the InvalidUrlException, HttpExceptionRequest and lastly the status code one

getHTML :: String -> IO (Either SomeException LBSC.ByteString)
getHTML url = do
    result <- try $ do
        mymanager <- ClientTLS.newTlsManager
        myrequest <- Client.parseRequest url
        response <- Client.httpLbs myrequest mymanager
        let status_code = statusCode (Client.responseStatus response)
        if status_code /= 200
            then do
                throwIO StatusCodeException
            else do
                let response_html = Client.responseBody response
                return response_html
    return result


parseTheTags :: LBSC.ByteString -> [Soup.Tag String]
parseTheTags response_html = (Soup.parseTags :: String -> [Soup.Tag String]) (LBSC.unpack response_html)

evalTests :: (( Double, ([Double] , [Double]) ), Vocabulary ) -> [String] -> IO [(Double, Double, Double, Double)]
evalTests _ [] = return []
evalTests trainedModel (x:y:xs) = do
    lang_test <- readFile ("cases/" ++ x)
    src_test <- readFile ("cases/" ++ y)
    let test_data = (lines src_test) ++ (lines lang_test)
    let final_probs = Lib.classifyNaiveBayes test_data trainedModel
    let mapping = zip test_data final_probs
    let src_test_len = length (lines src_test)
    let lang_test_len = length (lines lang_test)
    let yTest = replicate src_test_len 0 ++ replicate lang_test_len 1
    let test_accuracy_mapping = zip yTest final_probs
    -- precision_code, recall_code, precision_lang, recall_lang
    rest <- evalTests trainedModel xs
    return $ (Lib.evaluateNaiveBayes test_accuracy_mapping):rest

roundTo :: Int -> Double -> Double
roundTo n x = (fromInteger $ round $ x * (10^n)) / (10.0^^n)


-- 0 means code 1 means lang. left is truth right is predicted
evaluateNaiveBayes :: [(Int, Int)] -> (Double, Double, Double, Double)
evaluateNaiveBayes mydata = 
    let total_actual_lang = sum (map fst mydata)
        total_actual_code = (length mydata) - total_actual_lang
        code_correct = filter (== (0,0)) mydata
        lang_correct = filter (== (1,1)) mydata
        code_wrong  = filter (== (0,1)) mydata
        lang_wrong  = filter (== (1,0)) mydata

        num_code_correct = length code_correct
        num_lang_correct = length lang_correct
        num_code_wrong = length code_wrong
        num_lang_wrong = length lang_wrong

        -- need to keep in mind that paper has used different metrics
        

        precision_code = roundTo 2 (int2Double num_code_correct / (int2Double num_code_correct + int2Double num_lang_wrong))

        recall_code = roundTo 2 (int2Double num_code_correct / ((int2Double num_code_correct) + int2Double num_code_wrong))

        precision_lang = roundTo 2 (int2Double num_lang_correct / (int2Double num_lang_correct + int2Double num_code_wrong))

        recall_lang = roundTo 2 (int2Double num_lang_correct / ((int2Double num_lang_correct) + int2Double num_lang_wrong))

    in (precision_code, recall_code, precision_lang, recall_lang)





classifyNaiveBayes :: [String] -> (( Double, ([Double] , [Double]) ), Vocabulary ) -> [Int]
classifyNaiveBayes test_data trainedModel =
    let vocab = snd trainedModel
        
        -- lang_test_data = lines lang_test
        -- src_test_data = lines src_test


        -- test_data = src_test_data ++ lang_test_data
        xTest = (NLA.toLists (myVectorizer vocab (filter (not . null) test_data)))

        
        -- xTest_src = (NLA.toLists (myVectorizer vocab (src_test_data)))
        -- xTest_lang = (NLA.toLists (myVectorizer vocab (lang_test_data)))

        -- src_test_len = length (xTest_src)
        -- lang_test_len = length (xTest_lang)

        -- yTest = replicate src_test_len 0 ++ replicate lang_test_len 1

        -- xTest = xTest_src ++ xTest_lang

        test_len = length xTest


        y = NLA.fromLists [(replicate (test_len) (int2Double 0))]

        -- [Double]

        xgivenY_src = fst (snd (fst trainedModel))
        xgivenY_lang = snd (snd (fst trainedModel))
        prob_src_prior = fst (fst trainedModel)


        log_src = map log xgivenY_src
        log_lang = map log xgivenY_lang

        log_matrix = NLA.tr (NLA.fromLists [log_src, log_lang])

        -- [Double,Double]
        -- print (map typeOf (head (DM.toLists log_matrix)))

        -- mult
        prob1 = (NLA.fromLists xTest) NLA.<> (log_matrix)

        -- [Double,Double]
        -- print (map typeOf (head (DM.toLists prob1)))

        logp = log prob_src_prior
        log_not_p = log (1 - prob_src_prior)

        prob1_trans = NLA.toLists (NLA.tr prob1)

        prob2 = map (\x -> x + logp) (head prob1_trans)
        prob3 = map (\x -> x + log_not_p) (head (tail prob1_trans))

        combined = [prob2, prob3]
        combined_mat = NLA.tr (NLA.fromLists combined)
        
        final_probs = map (\x -> if (head x) > (head (tail x)) then 0 else 1) (NLA.toLists combined_mat)

    in final_probs

-- can try property based testing for the mathematical stuff like probabilities summing to 1 or smth. see properties of naive bayes



readTraining :: String -> String -> IO ([String], [String])
readTraining lang_file src_file = do
    natural <- readFile "input/lang_train.txt"
    src <- readFile "input/code_train.txt"
    return (lines natural, lines src)


trainNaiveBayes :: [String] -> [String] -> (( Double, ([Double] , [Double]) ), Vocabulary )
trainNaiveBayes natural_data source_data = 
    let source_words = concat (getWords source_data)
        natural_words = concat (getWords natural_data)
        unique_src_words = getUniqueWords source_words
        unique_natural_words = getUniqueWords natural_words
        vocab = unique_src_words ++ unique_natural_words

        -- source_data is an array of strings, where each string represents a new line in the file. So here each line here is a document here. 
        xTrain_src = myVectorizer vocab (source_data)
        xTrain_lang = myVectorizer vocab (natural_data)

        sourceCodeMatrix = xTrain_src
        naturalLanguageMatrix = xTrain_lang

        -- NLA.Matrix Double -> [Int]
        sum_src_cols = sumCols sourceCodeMatrix
        sum_lang_cols = sumCols naturalLanguageMatrix


        src_len = length source_data
        natural_len = length natural_data

        xgivenY_src = calcXGivenY src_len sum_src_cols
        xgivenY_lang = calcXGivenY natural_len sum_lang_cols

        prob_src_prior = (int2Double src_len) / (int2Double src_len + fromIntegral natural_len)

    in ((prob_src_prior, (xgivenY_src,xgivenY_lang)), vocab )
    




-- each row in matrix corresponds to a document and each column to a word in the vocabulary. So an entry (i,j) represents the word count of word j from the vocab in document i
myVectorizer :: Vocabulary -> [Document] -> NLA.Matrix Double
myVectorizer vocab docs
    | vocab == [] = NLA.fromLists [[int2Double 0]] -- sinceFromlists doesn't accept empty lists
    | docs == [] = NLA.fromLists [[int2Double 0]]
    | otherwise = NLA.fromLists [matrixRow vocab doc | doc <- docs]


-- takes a vocab i.e. list of strings. for each word in the vocab, look it up in the wordcount map of that document. If found then return the count, if not then 0 since the vocab word is not in this particular document. So this array of ints corresponds to one row in the vectorized matrix i.e. represents the word counts of all the words in the vocabulary in this document. 
matrixRow :: Vocabulary -> Document -> [Double]
matrixRow vocab doc = [fromMaybe (int2Double 0) (lookup vocab_word mywordcounts) | vocab_word <- vocab]
  where mywordcounts = wordCounts doc


-- for a document i.e. (string), create list of (string, int) pairs which maps each word with it's count in the document. create tuple for each word in the form of (word, 1) then map with the addition function in order to count
wordCounts :: Document -> [(String, Double)]
wordCounts doc = Data.Map.toList $ fromListWith (+) [(oneword, int2Double 1) | oneword <- words doc]



-- individual words
getWords :: [String] -> [[String]]
getWords [] = []
getWords (x:xs)
    | '\n' `elem` x && length x == 1 = ["NEWLINE"]:getWords(xs)
    -- | words x == [] = x:getWords(xs) -- here need to check if newline to do newline, otherwise spaces also getting included which makes it bad 
    | otherwise = (words (T.unpack . T.toLower . T.pack $ x)):getWords(xs)
    

getUniqueWords :: [String] -> [String]
-- get all unique strings from list of strings
getUniqueWords = foldl (\seen x -> if x `elem` seen then seen else seen ++ [x]) []

sumCols :: NLA.Matrix Double -> [Double]
sumCols matrix = map sum (NLA.toLists (NLA.tr matrix))

calcXGivenY :: Int -> [Double] -> [Double]
calcXGivenY mylen my_cols_sum =  map (\x -> x + 0.001 / int2Double (mylen) + 0.9 ) my_cols_sum



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
-- can add more detail to the regexTokenizer regexes later for stuff like fun() and struct->pointer and system.out etc

regexTokenizer :: [String] -> [(String,String)]
regexTokenizer [] = []
regexTokenizer (x:xs)
    | x == "NEWLINE" = (x,x):regexTokenizer(xs)
    | regextest x "\\b(for|if|else|while|return|int|float|double|char|void|bool|string|struct|class|public)\\b" = (x,"KEYWORD"):regexTokenizer(xs)
    | regextest x "\\b[a-zA-Z]+\\b" = (x,"WORD"):regexTokenizer(xs)
    | regextest x "\\b[a-zA-Z_]+\\b" = (x,"UNDERSCORE_WORD"):regexTokenizer(xs)
    | regextest x "\\b[0-9]+\\b" = (x,"NUMBER"):regexTokenizer(xs)
    | otherwise = (x,x):regexTokenizer(xs)



getText :: [Soup.Tag String] -> [String]
getText tagstrings = map Soup.fromTagText (Prelude.filter Soup.isTagText tagstrings)

writeFullSrc :: [Soup.Tag String] -> IO ()
writeFullSrc tagstrings = writeFile "output_files/full_text.txt" (Soup.innerText tagstrings)






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


writeToDocx :: String -> String -> IO ()
writeToDocx filepath lang_class  = do
    pandoc_lang <- runIO $ readHtml def ( TextConv.convertText ("<p>" ++ lang_class ++ "</p>" :: String ) :: T.Text )

    case pandoc_lang of
        Right x -> do
            y <- runIO $ writeDocx def x
            case y of
                Right direct_pan -> do
                    LBS.writeFile filepath direct_pan

                Left err -> Prelude.putStrLn $ "Error with pandoc writeDocx: " ++ show err

        Left err -> Prelude.putStrLn $ "Error parsing pandoc for natural language " ++ show err

    putStrLn "Completed writing to docx"

