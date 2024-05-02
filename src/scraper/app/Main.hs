module Main (main) where 

import Lib
import Data.List (elemIndex)
import Data.Map (fromListWith, toList)
import Data.Maybe (fromMaybe)
import qualified Data.Matrix as DM

type Document = String
type Vocabulary = [String]
type MyMatrix = [[Int]]

-- convert document to word list
wordsInDocument :: Document -> [String]
wordsInDocument = words

-- count word freq
wordCounts :: Document -> [(String, Int)]
wordCounts doc = toList $ fromListWith (+) [(word, 1) | word <- wordsInDocument doc]

-- one row for each doc
matrixRow :: Vocabulary -> Document -> [Int]
matrixRow vocab doc = [fromMaybe 0 (lookup word counts) | word <- vocab]
  where counts = wordCounts doc

-- doc word incidence matrix
vectorizer :: Vocabulary -> [Document] -> MyMatrix
vectorizer vocab docs = [matrixRow vocab doc | doc <- docs]



sumCols :: DM.Matrix Int -> [Int]
sumCols matrix = map sum (DM.toLists (DM.transpose matrix))


-- nbXGivenY :: DM.Matrix Int -> DM.Matrix Int -> [[Int]]
-- nbXGivenY src_mat lang_mat = do
--   let 









main :: IO ()
main = do
    -- let url = "https://eli.thegreenplace.net/2018/type-erasure-and-reification/"
    -- response_html <- getHTML url
    -- let parsed_tags = parseTheTags response_html

    -- -- print concat (getWords (getText parsed_tags))


    -- let splitted = preProc (splitOnNewline (concat (getWords (getText parsed_tags))))

    -- print splitted

    -- testDataReader <- readFile "output_files/newlines_words2.txt"
    -- let test_data = testDataReader

    natural <- readFile "input/natural_language_text.txt"
    let natural_data = lines natural
    source <- readFile "input/source_code.txt"
    let source_data = lines source

    let source_words = concat (getWords source_data)
    let natural_words = concat (getWords natural_data)

    let unique_src_words = getUniqueWords source_words
    let unique_natural_words = getUniqueWords natural_words

    -- print "Unique source code words: "
    -- print (length unique_src_words)
    -- print "Natural language words: "
    -- print (length unique_natural_words)

    let vocab = unique_src_words ++ unique_natural_words
    -- print "Total length of vocabulary: "
    -- print (length vocab)


    let src_len = length source_data
    let natural_len = length natural_data

    -- print "Number of source code docs: "
    -- print src_len
    -- print "Number of natural language docs: "
    -- print natural_len

    -- print "Total number of documents"
    -- print (src_len + natural_len)



    let xTrain_src = vectorizer vocab (source_data)
    let xTrain_natural = vectorizer vocab (natural_data)


    let yTrain = replicate src_len 0 ++ replicate natural_len 1
    -- get the number of elements in the matrix vectorizer and the number of elements in each element of vectorizer
    -- print "Number of training documennts: "
    -- print (length xTrain)
    -- print "Number of words in vocab: "
    -- print (length (head xTrain))

--     "Unique source code words: "
-- 1195
-- "Natural language words: "
-- 11223
-- "Total length of vocabulary: "
-- 12418
-- "Number of source code docs: "
-- 1189
-- "Number of natural language docs: "
-- 7440
-- "Total number of documents"
-- 8629
-- "Number of training documennts: "
-- 8629
-- "Number of words in vocab: "
-- 12418

  -- let data = [1195,11223,12418,1189,7440,8629]

    let sourceCodeMatrix = DM.fromLists (xTrain_src)
    let naturalLanguageMatrix = DM.fromLists (xTrain_natural)


    let sum_src_cols = (sumCols sourceCodeMatrix)
    let sum_lang_cols = (sumCols naturalLanguageMatrix)

    let xgivenY_src = map (\x -> fromIntegral (x) / fromIntegral(src_len) ) sum_src_cols


    let xgivenY_lang = map (\x -> fromIntegral (x) / fromIntegral(natural_len) ) sum_lang_cols



















    
    -- let newline_words = concat (getWords (getText parsed_tags))


    -- print (tokenizer ["intuition", "reification", "for", "glint", "01", "word10", "10word", "hyphen-word", "println"])

    -- print (tokenizer newline_words)


    -- print (map regextest ["word", "englush", "01"])



    -- print (concat (tokenize  parsed_tags))

    -- let parsed_tags = parseTheTags response_html
    -- let trimmed = trimText parsed_tags

    -- print trimmed
    -- writeFullSrc parsed_tags

    -- let separated_text_code = separateTextCode parsed_tags
    
    -- let preTags             = fst separated_text_code
    -- let nonPreTags          = snd separated_text_code

    -- writeToTxt preTags "output_files/final_text.txt"
    -- writeToDocx nonPreTags "output_files/final_text.docx"

