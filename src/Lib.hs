
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where


import Brick
import Control.Monad (void, forever)
import Text.Parsec

import TextSplitter
import TranslationEnv
import TransAPIs
import TransInterface


someFunc :: IO ()
someFunc = do
    s <- libreTranslate "ar" "Call me Ishmael. Some years ago..."
    putStrLn s

--    simpleMain translationUI
{-
    sampleTxt <- readFile "junk/testin.txt"
    let parsed = runParser parseSplitSource () "" sampleTxt
    case parsed of
        Left err -> putStrLn $ show err
        Right s -> do
            let content =  splitSourceContent s
            let format = splitSourceTemplate s
            writeFile "junk/testin.content" content
            writeFile "junk/testin.format" format
            let contentStrings = either (const []) id $ runParser parseContentFile () "" content
            let reassembled = either (const "") id $ runParser (reassembleSourceTemplate contentStrings) () "" format
            writeFile "junk/testin.reassembled" $ reassembled
-}
