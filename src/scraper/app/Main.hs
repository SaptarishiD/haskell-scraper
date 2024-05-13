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
-- maybe mention that the training and testing data don't have any empty lines so everything has been put close together with newlines
-- so can add that in the preproc stage for any new thing that needs to go into the pipeline
main :: IO ()
main = do

      read_text_source <- readFile "input/eg.txt"
      let text_source = lines read_text_source

      let lang_train = "input/lang_train.txt"
      let code_train = "input/code_train.txt"
      mydata <- Lib.readTraining lang_train code_train
      let lang_data = fst mydata
      let code_data = snd mydata
      let trainedModel = Lib.trainNaiveBayes lang_data code_data

      -- let testFiles = ["lang_test1.txt", "code_test1.txt","lang_test5.txt", "code_test5.txt", "lang_test6.txt", "code_test6.txt", "lang_test7.txt", "code_test7.txt", "lang_test8.txt", "code_test8.txt", "lang_test9.txt", "code_test9.txt", "lang_test10.txt", "code_test10.txt"]
      -- let test_evals = Lib.evalTests trainedModel testFiles

      -- test_results <- test_evals
      -- print test_results
      


      -- -- lang_test <- readFile "input/lang_test.txt"
      -- -- src_test <- readFile "input/code_test.txt"

      -- src_test <- readFile "cases/code_test8.txt"
      -- lang_test <- readFile "cases/lang_test8.txt"

      -- -- print (filter (not . null) (lines src_test))

      

      -- let test_data = (lines src_test) ++ (lines lang_test)
      -- -- print test_data
      -- let final_probs = Lib.classifyNaiveBayes test_data trainedModel
      -- -- print (filter (not . null) test_data)
      -- let mapping = zip test_data final_probs
      -- let src_test_len = length (lines src_test)
      -- let lang_test_len = length (lines lang_test)
      -- let yTest = replicate src_test_len 0 ++ replicate lang_test_len 1
      -- let test_accuracy_mapping = zip yTest final_probs
      -- -- precision_code, recall_code, precision_lang, recall_lang
      -- let evals = Lib.evaluateNaiveBayes test_accuracy_mapping
      -- print evals

      -- print test_accuracy_mapping

      -- print "DONE" {-

    

  -- num_code_correct, num_lang_correct, num_code_wrong, num_lang_wrong, total_actual_code, total_actual_lang)
  --(91,22,0,28,91,50)
  -- (precision_code, recall_code, precision_lang, recall_lang)
  -- (0.7647058823529411,1.0,1.0,0.44)


  -- can have smth like every sequence of 0s or 1s is separated by a newline to demarcate separate segments or smth

  -- can do the actual separation into files later cause that's just for the final thing. Before that need to evaluate. Cause don't wanna manually do stuff earlier. So first evaluate and test on easily testable stuff jiska code and natural language already separated since going line by line

  -- need error percentages and other stuff like recall precision and stuff so that can make nice tables and stuff. Also need to cross-validate stuff

      let final_classes = Lib.classifyNaiveBayes text_source trainedModel
      let mapping = zip text_source final_classes
      let code_class = [x | x <- mapping, snd x == 0]
      let lang_class = [x | x <- mapping, snd x == 1]
      writeFile "output_files/NB_code_class.txt" (unlines (map fst code_class))
      writeToDocx "output_files/NB_lang_class.docx" (unlines (map fst lang_class))

      
      writeFile "output_files/NB_lang_class.txt" (unlines (map fst lang_class))
      




