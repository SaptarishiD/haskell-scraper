{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Main (main) where

import Lib


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

      let final_classes = Lib.classifyNaiveBayes text_source trainedModel
      let mapping = zip text_source final_classes
      let code_class = [x | x <- mapping, snd x == 0]
      let lang_class = [x | x <- mapping, snd x == 1]
      writeFile "output_files/NB_code_class.txt" (unlines (map fst code_class))
      writeToDocx "output_files/NB_lang_class.docx" (unlines (map fst lang_class))

      
      writeFile "output_files/NB_lang_class.txt" (unlines (map fst lang_class))


       -- let testFiles = ["lang_test1.txt", "code_test1.txt","lang_test2.txt", "code_test2.txt", "lang_test3.txt", "code_test3.txt", "lang_test4.txt", "code_test4.txt", "lang_test5.txt", "code_test5.txt", "lang_test6.txt", "code_test6.txt", "lang_test7.txt", "code_test7.txt"]
      -- let test_evals = Lib.evalTests trainedModel testFiles

      -- test_results <- test_evals
      -- print test_results
      


      -- lang_test <- readFile "input/lang_test.txt"
      -- src_test <- readFile "input/code_test.txt"

    --   src_test <- readFile "cases/code_test5.txt"
    --   lang_test <- readFile "cases/lang_test5.txt"

      -- print (filter (not . null) (lines src_test))

      

    --   let test_data = (lines src_test) ++ (lines lang_test)
    --   -- print test_data
    --   let final_probs = Lib.classifyNaiveBayes test_data trainedModel
    --   -- print (filter (not . null) test_data)
    --   let mapping = zip test_data final_probs
    --   let src_test_len = length (lines src_test)
    --   let lang_test_len = length (lines lang_test)
    --   let yTest = replicate src_test_len 0 ++ replicate lang_test_len 1
    --   let test_accuracy_mapping = zip yTest final_probs
    --   -- precision_code, recall_code, precision_lang, recall_lang
    --   let evals = Lib.evaluateNaiveBayes test_accuracy_mapping
    --   print evals

    --   print mapping

      -- print "DONE" {-


      




