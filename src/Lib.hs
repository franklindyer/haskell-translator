
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where


import Brick
import Brick.BChan
import Control.Monad (void, forever)
import Control.Monad.Trans.Reader
import Text.Parsec
import System.IO

import TextSplitter
import TransEnv
import TransActions
import TransAPIs
import TransInterface
import TransState

import TransApp

someFunc :: IO ()
someFunc = do
    let fileName = "junk/testin.txt"
    let targetLang = "en"

    chan <- newBChan 10

    putStr "File to translate: " >> hFlush stdout
    toTranslate <- getLine 
    putStr "Source language: " >> hFlush stdout
    srcLang <- getLine
    putStr "Target language: " >> hFlush stdout
    trgLang <- getLine

    let transEnv = TransEnv {
        basePath = toTranslate,
        sourceLang = srcLang,
        targetLang = trgLang,
        apiChan = chan,
        suggestors = [("LibreTranslate", libreTranslate)]
    }

    runReaderT preprocSource transEnv
    runReaderT translateSource transEnv
    runReaderT postprocTarget transEnv 
