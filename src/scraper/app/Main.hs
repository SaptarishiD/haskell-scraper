{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Main (main) where

import Lib
import Numeric.LinearAlgebra as NLA
import GHC.Float (int2Double)


-- also need to be careful abt hyperparameter tuning and stuff
-- also can have heuristics in the sense of full stop and colon at end of line meaning natural language and not code
-- need to compare performance with the test set
-- can also get details about the words and stuff
-- do cross-validation
-- can loop through various tests and see results on each of them
-- can do cross validation later
-- can also just store the matrices and their multiplied results once
-- presentation mein ofc can't show entire thing so just show results of the trained model on some tests



-- maybe can fix the old not matching errors with this let in stuff instead of let and return cause same errors were happening

main :: IO ()
main = do

    -- let test_urls = ["https://www.baeldung.com/java-unit-test-private-methods", "https://eli.thegreenplace.net/2018/type-erasure-and-reification/", "https://docs.python.org/3/tutorial/datastructures.html", "https://www.google.com/teapot"]
    -- InvalidUrlException, http exception and some other exception




    

    

    let url = "https://eli.thegreenplace.net/2018/type-erasure-and-reification/"
    response_html <- getHTML url

    case response_html of
      Left e -> do 
        putStrLn $ "\nERROR! The following exception occured:\n\n" ++ head (words (show e)) ++ "\n"
      Right myhtml -> do
        let parsed_tags = parseTheTags myhtml
        let splitted = preProc (splitOnNewline (concat (getWords (getText parsed_tags))))

        let lang_train = "input/lang_train.txt"
        let code_train = "input/code_train.txt"

        mydata <- Lib.readTraining lang_train code_train
        let natural_data = fst mydata
        let source_data = snd mydata


        let trainedModel = Lib.trainNaiveBayes natural_data source_data



        -- let test = ["Hello world world world world", "World", "Test"]
        -- let dummy = map wordCounts test
        -- -- print dummy

        -- let source_words = concat (getWords source_data)
        -- let natural_words = concat (getWords natural_data)
        -- let unique_src_words = getUniqueWords source_words
        -- let unique_natural_words = getUniqueWords natural_words
        -- let vocab = unique_src_words ++ unique_natural_words

        -- -- source_data is an array of strings, where each string represents a new line in the file. So here each line here is a document here. 
        -- let xTrain_src = myVectorizer vocab (source_data)
        -- let xTrain_lang = myVectorizer vocab (natural_data)

        -- let sourceCodeMatrix = xTrain_src
        -- let naturalLanguageMatrix = xTrain_lang

        -- -- NLA.Matrix Double -> [Int]
        -- let sum_src_cols = sumCols sourceCodeMatrix
        -- let sum_lang_cols = sumCols naturalLanguageMatrix


        -- let src_len = length source_data
        -- let natural_len = length natural_data

        -- let xgivenY_src = calcXGivenY src_len sum_src_cols
        -- let xgivenY_lang = calcXGivenY natural_len sum_lang_cols

        -- -- print xgivenY_src






        -- let lang_test_data = lines lang_test
        -- let src_test_data = lines src_test

        -- let vocab = snd trainedModel
        
        -- let xTest_src = (NLA.toLists (myVectorizer vocab (src_test_data)))
        -- let xTest_lang = (NLA.toLists (myVectorizer vocab (lang_test_data)))

        -- let src_test_len = length (xTest_src)
        -- let lang_test_len = length (xTest_lang)

        -- let yTest = replicate src_test_len 0 ++ replicate lang_test_len 1

        -- let xTest = xTest_src ++ xTest_lang

        -- let y = NLA.fromLists [(replicate (src_test_len + lang_test_len) (int2Double 0))]

        -- let xgivenY_src = fst (snd (fst trainedModel))
        -- let xgivenY_lang = snd (snd (fst trainedModel))
        -- let prob_src_prior = fst (fst trainedModel)


        -- let log_src = map log xgivenY_src
        -- let log_lang = map log xgivenY_lang

        -- let log_matrix = NLA.tr (NLA.fromLists [log_src, log_lang])

        -- -- [Double,Double]
        -- -- print (map typeOf (head (DM.toLists log_matrix)))

        -- let prob1 = (NLA.fromLists xTest) NLA.<> (log_matrix)

        -- -- [Double,Double]
        -- -- print (map typeOf (head (DM.toLists prob1)))

        -- let logp = log prob_src_prior
        -- let log_not_p = log (1 - prob_src_prior)

        -- let prob1_trans = NLA.toLists (NLA.tr prob1)

        -- let prob2 = map (\x -> x + logp) (head prob1_trans)
        -- let prob3 = map (\x -> x + log_not_p) (head (tail prob1_trans))

        -- let combined = [prob2, prob3]
        -- let combined_mat = NLA.tr (NLA.fromLists combined)
        
        -- let final_probs = map (\x -> if (head x) > (head (tail x)) then 0 else 1) (NLA.toLists combined_mat)

        -- print final_probs


        -- print trained


        lang_test <- readFile "input/lang_test.txt"
        src_test <- readFile "input/code_test.txt"

        let final_probs = Lib.classifyNaiveBayes lang_test src_test trainedModel




        let mapping = zip (lines src_test ++ lines lang_test) final_probs
        let src_test_len = length (lines src_test)
        let lang_test_len = length (lines lang_test)
        let yTest = replicate src_test_len 0 ++ replicate lang_test_len 1
        let test_accuracy_mapping = zip yTest final_probs


        let evals = Lib.evaluateNaiveBayes test_accuracy_mapping

        print evals

    print "DONE" {-


    -- num_code_correct, num_lang_correct, num_code_wrong, num_lang_wrong, total_actual_code, total_actual_lang)
    --(91,22,0,28,91,50)
    -- (precision_code, recall_code, precision_lang, recall_lang)
    -- (0.7647058823529411,1.0,1.0,0.44)



    -- print mapping

    -- print test_accuracy_mapping



    -- can have smth like every sequence of 0s or 1s is separated by a newline to demarcate separate segments or smth

    -- can do the actual separation into files later cause that's just for the final thing. Before that need to evaluate. Cause don't wanna manually do stuff earlier. So first evaluate and test on easily testable stuff jiska code and natural language already separated since going line by line

    -- need error percentages and other stuff like recall precision and stuff so that can make nice tables and stuff. Also need to cross-validate stuff


    let code_class = [x | x <- mapping, snd x == 0]
    let lang_class = [x | x <- mapping, snd x == 1]
    writeFile "output_files/NB_lang_class.txt" (unlines (map fst lang_class))
    writeFile "output_files/NB_code_class.txt" (unlines (map fst code_class))

-}

