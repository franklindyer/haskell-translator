
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where


import Brick
import Control.Monad (void, forever)
import Control.Monad.Trans.Reader
import Text.Parsec

import TextSplitter
import TransEnv
import TransActions
import TransAPIs
import TransInterface
import TransState

someFunc :: IO ()
someFunc = do
    let fileName = "junk/testin.txt"
    let targetLang = "en"

    let transEnv = TransEnv {
        basePath = "junk/testin.txt",
        sourceLang = "en",
        targetLang = "es"
    }

    runReaderT preprocSource transEnv
    runReaderT translateSource transEnv
--    runReaderT postprocSource transEnv
 
{- 
    sampleTxt <- readFile ("junk/" ++ fileName ++ ".content")
    let (Right s) = runParser parseContentFile () "" sampleTxt

    let ss = initTranslator s
    translated <- fmap ((map snd) . allPassages . fst) $ customMainWithDefaultVty Nothing transApp ss
    contentOut <- writeFile ("junk/" ++ fileName ++ ".content." ++ targetLang) (stringListContent translated)

    return ()
-}
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
