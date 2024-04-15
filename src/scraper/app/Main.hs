module Main (main) where 

import Lib

main :: IO ()
main = do
    let url = "https://eli.thegreenplace.net/2018/type-erasure-and-reification/"
    response_html <- getHTML url

    let parsed_tags = parseTheTags response_html

    let separated_text_code = separateTextCode parsed_tags
    
    let preTags             = fst separated_text_code
    let nonPreTags          = snd separated_text_code

    writeToTxt preTags
    writeToDocx nonPreTags

