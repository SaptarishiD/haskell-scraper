module Main (main) where

import Lib
import Data.Typeable
import Data.Map (fromListWith, toList)
import Data.Maybe (fromMaybe)
import qualified Data.Matrix as DM
import GHC.Float (int2Double)

type Document = String
type Vocabulary = [String]
type MyMatrix = [[Int]]





main :: IO ()
main = do
    let url = "https://eli.thegreenplace.net/2018/type-erasure-and-reification/"
    response_html <- getHTML url
    let parsed_tags = parseTheTags response_html
    let splitted = preProc (splitOnNewline (concat (getWords (getText parsed_tags))))

    natural <- readFile "input/natural_language_text.txt"
    let natural_data = lines natural
    source <- readFile "input/source_code.txt"
    let source_data = lines source

    let source_words = concat (getWords source_data)
    let natural_words = concat (getWords natural_data)
    let unique_src_words = getUniqueWords source_words
    let unique_natural_words = getUniqueWords natural_words

    let vocab = unique_src_words ++ unique_natural_words

    let src_len = length source_data
    let natural_len = length natural_data

    let xTrain_src = myVectorizer vocab (source_data)
    let xTrain_natural = myVectorizer vocab (natural_data)
    let yTrain = replicate src_len 0 ++ replicate natural_len 1

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

    let sourceCodeMatrix = xTrain_src
    let naturalLanguageMatrix = xTrain_natural

    -- DM.Matrix Int -> [Int]
    let sum_src_cols = (sumCols sourceCodeMatrix)
    let sum_lang_cols = (sumCols naturalLanguageMatrix)


    -- maybe change these all to floats or smth explicitly

    let xgivenY_src = Lib.calcXGivenY src_len sum_src_cols
    let xgivenY_lang = Lib.calcXGivenY natural_len sum_lang_cols

    let prob_src_prior = (int2Double src_len) / (int2Double src_len + fromIntegral natural_len)


    -- myVectorizer :: Vocabulary -> [Document] -> Matrix
    -- xTest is of type [[Double]]
    let xTest = map (map int2Double) (DM.toLists (myVectorizer vocab (splitted)))

    -- print (length (head xTest))

    let num_samples = length (splitted)
    let y = DM.zero 1 num_samples -- here int could be a problem

    -- these are of type [Double]
    let log_src = map log xgivenY_src
    let log_lang = map log xgivenY_lang

    -- print (map typeOf log_src)

    -- put log_src and long_lang into a new list that has these two lists as it's two elements
    let log_matrix = DM.transpose (DM.fromLists [log_src, log_lang])

    -- [Double,Double]
    -- print (map typeOf (head (DM.toLists log_matrix)))

    let prob1 = DM.multStrassen (DM.fromLists xTest) log_matrix
    -- [Double,Double]
    -- print (map typeOf (head (DM.toLists prob1)))

    let logp = log prob_src_prior
    let log_not_p = log (1 - prob_src_prior)

    -- can also just store the matrices and their multiplied results ekbar
    -- presentation mein ofc can't show entire thing so just show results of the trained model on some tests

    -- print (DM.nrows prob1) -- 104
    -- print (DM.ncols prob1) -- 2

    let prob1_trans = DM.toLists (DM.transpose prob1)

    let prob2 = map (\x -> x + logp) (head prob1_trans)
    let prob3 = map (\x -> x + log_not_p) (head (tail prob1_trans))

    -- combine the two above lists into one list where the first element is the first list and the second element is the second list
    let combined = [prob2, prob3]
    let combined_mat = DM.transpose (DM.fromLists combined)

    -- print (DM.nrows combined_mat) -- 104
    -- print (DM.ncols combined_mat) -- 2

    let final_probs = map (\x -> if (head x) > (head (tail x)) then 0 else 1) (DM.toLists combined_mat)

    let mapping = zip splitted final_probs

    print (mapping)

    print "done" {-

    let prob2 = DM.mapCol (+ logp) 1 
    let prob3 = DM.mapCol (\x -> x + log_not_p) 2 prob2


    print (take 20 prob3)

-}

