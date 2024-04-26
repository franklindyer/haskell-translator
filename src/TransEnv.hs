module TransEnv where

import Brick.BChan
import Control.Monad.Trans.Reader
import System.FilePath

import TransState

type ApiTranslator = (String, String -> String -> IO String)

data TransEnv = TransEnv {
    sourceLang :: String,
    targetLang :: String,
    basePath :: FilePath,
    apiChan :: BChan TransEvent,
    suggestors :: [ApiTranslator]
}

askFormatPath :: Monad m => ReaderT TransEnv m FilePath
askFormatPath = do
    base <- fmap basePath ask
    return $ base ++ ".format"

askSourcePath :: Monad m => ReaderT TransEnv m FilePath
askSourcePath = do
    base <- fmap basePath ask
    lang <- fmap sourceLang ask
    return $ base ++ "." ++ lang

askSourceContentPath :: Monad m => ReaderT TransEnv m FilePath
askSourceContentPath = do
    base <- fmap basePath ask
    lang <- fmap sourceLang ask
    return $ base ++ ".content." ++ lang

askTargetContentPath :: Monad m => ReaderT TransEnv m FilePath
askTargetContentPath = do
    base <- fmap basePath ask
    lang <- fmap targetLang ask
    return $ base ++ ".content." ++ lang

askTranslationPath :: Monad m => ReaderT TransEnv m FilePath
askTranslationPath = do
    base <- fmap basePath ask
    lang <- fmap targetLang ask
    return $ base ++ ".translation." ++ lang

askApiChan :: Monad m => ReaderT TransEnv m (BChan TransEvent)
askApiChan = fmap apiChan ask
