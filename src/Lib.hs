
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
import TransState

someFunc :: IO ()
someFunc = do
    sampleTxt <- readFile "junk/testin.content"
    let parsed = runParser parseContentFile () "" sampleTxt
    case parsed of
        Left err -> putStrLn $ show err
        Right s -> do
            let ss = initTranslator s
            void $ customMainWithDefaultVty Nothing transApp ss 
    return ()

{-
    s <- libreTranslate "ar" "Call me Ishmael. Some years ago..."
    putStrLn s
-}
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
