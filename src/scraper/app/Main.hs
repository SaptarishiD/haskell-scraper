module Main (main) where

import Lib
import Data.Typeable
import Text.Read
import Data.Map (fromListWith, toList)
import qualified Data.List.Split as DLS
import Data.Maybe (fromMaybe)
import qualified Data.Matrix as DM
import GHC.Float (int2Double)

type Document = String
type Vocabulary = [String]
type MyMatrix = [[Int]]


-- also need to be careful abt hyperparameter tuning and stuff
-- also can have heuristics in the sense of full stop and colon at end of line meaning natural language and not code
-- need to compare performance with the test set
-- can also get details about the words and stuff
-- do cross-validation

main :: IO ()
main = do

    let url = "https://eli.thegreenplace.net/2018/type-erasure-and-reification/"
    response_html <- getHTML url
    let parsed_tags = parseTheTags response_html
    let splitted = preProc (splitOnNewline (concat (getWords (getText parsed_tags))))


  
  -- Training ==========================================================================================

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

    let sourceCodeMatrix = xTrain_src
    let naturalLanguageMatrix = xTrain_natural

    -- DM.Matrix Int -> [Int]
    let sum_src_cols = sumCols sourceCodeMatrix
    let sum_lang_cols = sumCols naturalLanguageMatrix

    -- maybe change these all to floats or smth explicitly

    let xgivenY_src = Lib.calcXGivenY src_len sum_src_cols
    let xgivenY_lang = Lib.calcXGivenY natural_len sum_lang_cols

    let prob_src_prior = (int2Double src_len) / (int2Double src_len + fromIntegral natural_len)

    -- myVectorizer :: Vocabulary -> [Document] -> Matrix
    -- xTest is of type [[Double]]

  -- Training Done ==========================================================================================


    lang_test <- readFile "input/lang_test.txt"
    let lang_test_data = lines lang_test
    src_test <- readFile "input/code_test.txt"
    let src_test_data = lines src_test


    let xTest_src = map (map int2Double) (DM.toLists (myVectorizer vocab (src_test_data)))

    let xTest_lang = map (map int2Double) (DM.toLists (myVectorizer vocab (lang_test_data)))


    let src_test_len = length (xTest_src)
    let lang_test_len = length (xTest_lang)

    let yTest = replicate src_test_len 0 ++ replicate lang_test_len 1

    print yTest
    print (sum yTest)
    print (lang_test_len)




    let xTest = xTest_src ++ xTest_lang
    -- print xTest

    let y = DM.zero 1 (src_test_len + lang_test_len)

    -- these are of type [Double]
    let log_src = map log xgivenY_src
    let log_lang = map log xgivenY_lang

    let log_matrix = DM.transpose (DM.fromLists [log_src, log_lang])
    -- print log_matrix


    -- writeFile "output_files/log_matrix.txt" (show log_matrix)
    -- [Double,Double]
    -- print (map typeOf (head (DM.toLists log_matrix)))

    let prob1 = DM.multStrassen (DM.fromLists xTest) log_matrix
    -- [Double,Double]
    -- print (map typeOf (head (DM.toLists prob1)))

    let logp = log prob_src_prior
    let log_not_p = log (1 - prob_src_prior)

    -- can also just store the matrices and their multiplied results once
    -- presentation mein ofc can't show entire thing so just show results of the trained model on some tests

    -- print (DM.nrows prob1) -- 104
    -- print (DM.ncols prob1) -- 2

    let prob1_trans = DM.toLists (DM.transpose prob1)

    let prob2 = map (\x -> x + logp) (head prob1_trans)
    let prob3 = map (\x -> x + log_not_p) (head (tail prob1_trans))

    let combined = [prob2, prob3]
    let combined_mat = DM.transpose (DM.fromLists combined)

    -- print combined_mat

    let final_probs = map (\x -> if (head x) > (head (tail x)) then 0 else 1) (DM.toLists combined_mat)

    let mapping = zip (src_test_data ++ lang_test_data) final_probs

    let test_accuracy_mapping = zip yTest final_probs

    print mapping

    print test_accuracy_mapping


    print "DONE" {-



    -- can have smth like every sequence of 0s or 1s is separated by a newline to demarcate separate segments or smth

    -- need error percentages and other stuff like recall precision and stuff so that can make nice tables and stuff. Also need to cross-validate stuff


    let code_class = [x | x <- mapping, snd x == 0]
    let lang_class = [x | x <- mapping, snd x == 1]

    writeFile "output_files/NB_lang_class.txt" (unlines (map fst lang_class))
    writeFile "output_files/NB_code_class.txt" (unlines (map fst code_class))


    -- print (mapping)


    let prob2 = DM.mapCol (+ logp) 1 
    let prob3 = DM.mapCol (\x -> x + log_not_p) 2 prob2


    print (take 20 prob3)

-}

