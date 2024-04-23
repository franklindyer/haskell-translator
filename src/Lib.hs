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
            writeFile "junk/testin.content" $ splitSourceContent s
            writeFile "junk/testin.format" $ splitSourceTemplate s
