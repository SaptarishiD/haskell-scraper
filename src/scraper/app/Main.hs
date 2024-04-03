module Main where 

import Lib
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Text.HTML.TagSoup
import qualified Data.ByteString.Lazy.Char8 as LBSC

-- write all the types of everything so that it's clear (type annotation)
-- know the types and stuff of everything
-- can modularise code further for testing purposes
main :: IO ()
main = do
    -- need to make http thing more robust for error handling, so can use status code for that
    mymanager <- newTlsManager
    let og_url = "https://eli.thegreenplace.net/2018/type-erasure-and-reification/"
    let url = "GET " ++ og_url
    myrequest <- parseRequest url
    response <- httpLbs myrequest mymanager
    let response_html = responseBody response

    let parsed_tags = parseTags (LBSC.unpack response_html)

    let separated_text_code = separateTextCode parsed_tags
    let preTags = fst separated_text_code
    let nonPreTags = snd separated_text_code

    pandocProcessTags preTags nonPreTags

