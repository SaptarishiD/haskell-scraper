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

    let url = "https://eli.thegreenplace.net/2018/type-erasure-and-reification/"
    response_html <- getHTML url
    let parsed_tags = parseTheTags response_html
    let splitted = preProc (splitOnNewline (concat (getWords (getText parsed_tags))))

    natural <- readFile "input/natural_language_text.txt"
    let natural_data = lines natural
    source <- readFile "input/source_code.txt"
    let source_data = lines source

    let trained = Lib.trainNaiveBayes natural_data source_data


    lang_test <- readFile "input/lang_test.txt"
    src_test <- readFile "input/code_test.txt"

    let final_probs = Lib.classifyNaiveBayes lang_test src_test trained


    let mapping = zip ((lines src_test) ++ (lines lang_test)) final_probs
    let src_test_len = length (lines src_test)
    let lang_test_len = length (lines lang_test)

    let yTest = replicate src_test_len 0 ++ replicate lang_test_len 1

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

