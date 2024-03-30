{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.Scalpel
import Control.Applicative


exampleHtml :: String
exampleHtml = "<article>\
\    <h1>title</h1>\
\    <h2>Section 1</h2>\
\    <p>Paragraph 1.1</p>\
\    <p>Paragraph 1.2</p>\
\    <h2>Section 2</h2>\
\    <p>Paragraph 2.1</p>\
\    <p>Paragraph 2.2</p>\
\</article>"



main :: IO ()
main = print $ scrapeStringLike exampleHtml (chroot "article" $ inSerial $ do
    title <- seekNext $ text "h1"
    sections <- many $ do
       section <- seekNext $ text "h2"
       ps <- untilNext (matches "h2") (many $ seekNext $ text "p")
       return (section, ps)
    return (title, sections) )