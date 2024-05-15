{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Main (main) where

import Lib

main :: IO ()
main = do

      -- Main Code-Text Separation pipeline for a particular input given by input/eg.txt 
      -- ========================================================================
      read_text_source <- readFile "input/eg.txt"
      let text_source = lines read_text_source

      -- Training
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

      -- Main Code-Text Separation pipeline for a particular input given by input/eg.txt
      -- ========================================================================
    

      -- Evaluation Tests 
      -- ========================================================================

      let testFiles = ["lang_test1.txt", "code_test1.txt","lang_test2.txt", "code_test2.txt", "lang_test3.txt", "code_test3.txt", "lang_test4.txt", "code_test4.txt", "lang_test5.txt", "code_test5.txt", "lang_test6.txt", "code_test6.txt", "lang_test7.txt", "code_test7.txt"]
      let test_evals = Lib.evalTests trainedModel testFiles

      test_results <- test_evals
      print test_results -- precision and recall wrt code and natural language for each test case

      -- Evaluation Tests 
      -- ========================================================================


      




