module TransActions where

import Brick
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import System.Directory
import Text.Parsec

import TextSplitter
import TransAPIs
import TransInterface
import TransState
import TransEnv

preprocSource :: ReaderT TransEnv IO ()
preprocSource = do
    fullFileName <- askSourcePath
    contentFileName <- askSourceContentPath
    formatFileName <- askFormatPath 
    sourceText <- lift $ readFile fullFileName
    let (Right parsed) = runParser parseSplitSource () "" sourceText
    let content = splitSourceContent parsed
    let format = splitSourceTemplate parsed
    lift $ writeFile contentFileName content
    lift $ writeFile formatFileName format

translateSource :: ReaderT TransEnv IO ()
translateSource = do
    sourceContentFile <- askSourceContentPath
    targetContentFile <- askTargetContentPath
    sourceContent <- lift $ readFile sourceContentFile
    isStarted <- lift $ doesFileExist targetContentFile
    let (Right sourceStrings) = runParser parseContentFile () "" sourceContent
    let transStateInit = initTranslator sourceStrings
    transState <-
        if isStarted
        then do
            targetContent <- lift $ readFile targetContentFile
            let (Right targetStrings) = runParser parseContentFile () "" targetContent
            return $ loadPartialTranslation targetStrings transStateInit
        else return transStateInit
    res <- lift $ customMainWithDefaultVty Nothing transApp transState    
    let translatedStrings = stringListContent $ map snd $ allPassages $ fst $ res
    lift $ writeFile targetContentFile translatedStrings 

postprocSource :: ReaderT TransEnv IO ()
postprocSource = do
    contentFileName <- askSourceContentPath
    formatFileName <- askFormatPath
    outFileName <- askTranslationPath
    content <- lift $ readFile contentFileName
    format <- lift $ readFile formatFileName
    let (Right contentStrings) = runParser parseContentFile () "" content
    let (Right reassembled) = runParser (reassembleSourceTemplate contentStrings) () "" format
    lift $ writeFile outFileName reassembled
