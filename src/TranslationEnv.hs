module TranslationEnv where

import System.FilePath

data TranslationEnv = TranslationEnv {
    sourceLang :: String,
    targetLang :: String,
    sourcePath :: FilePath,
    targetPath :: FilePath
}
