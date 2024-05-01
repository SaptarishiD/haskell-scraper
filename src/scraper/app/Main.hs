module Main (main) where 

import Lib


main :: IO ()
main = do
    let url = "https://eli.thegreenplace.net/2018/type-erasure-and-reification/"
    response_html <- getHTML url
    let parsed_tags = parseTheTags response_html

    let splitted = preProc (splitOnNewline (concat (getWords (getText parsed_tags))))
    print splitted
    
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

