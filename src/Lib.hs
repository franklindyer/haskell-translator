module Lib
    ( someFunc
    ) where

import Text.Parsec

import TextSplitter
import TranslationEnv

someFunc :: IO ()
someFunc = do
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
