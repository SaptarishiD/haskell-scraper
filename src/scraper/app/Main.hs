{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Main (main) where

import Lib



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


        
        -- lang_test <- readFile "input/lang_test.txt"
        -- src_test <- readFile "input/code_test.txt"
        -- let test_data = (lines src_test) ++ (lines lang_test)
        -- print test_data
        -- let final_probs = Lib.classifyNaiveBayes test_data trainedModel
        -- let mapping = zip test_data final_probs
        -- let src_test_len = length (lines src_test)
        -- let lang_test_len = length (lines lang_test)
        -- let yTest = replicate src_test_len 0 ++ replicate lang_test_len 1
        -- let test_accuracy_mapping = zip yTest final_probs
        -- -- precision_code, recall_code, precision_lang, recall_lang
        -- let evals = Lib.evaluateNaiveBayes test_accuracy_mapping
        -- print evals
      

    -- num_code_correct, num_lang_correct, num_code_wrong, num_lang_wrong, total_actual_code, total_actual_lang)
    --(91,22,0,28,91,50)
    -- (precision_code, recall_code, precision_lang, recall_lang)
    -- (0.7647058823529411,1.0,1.0,0.44)


    -- can have smth like every sequence of 0s or 1s is separated by a newline to demarcate separate segments or smth

    -- can do the actual separation into files later cause that's just for the final thing. Before that need to evaluate. Cause don't wanna manually do stuff earlier. So first evaluate and test on easily testable stuff jiska code and natural language already separated since going line by line

    -- need error percentages and other stuff like recall precision and stuff so that can make nice tables and stuff. Also need to cross-validate stuff

        let final_probs = Lib.classifyNaiveBayes splitted trainedModel
        let mapping = zip splitted final_probs
        let code_class = [x | x <- mapping, snd x == 0]
        let lang_class = [x | x <- mapping, snd x == 1]
        writeFile "output_files/NB_lang_class.txt" (unlines (map fst lang_class))
        writeFile "output_files/NB_code_class.txt" (unlines (map fst code_class))

        print "DONE" {-

-}

