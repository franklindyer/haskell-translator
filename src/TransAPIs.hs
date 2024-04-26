{-# LANGUAGE OverloadedStrings #-}

module TransAPIs where

import Brick
import Brick.BChan
import Brick.Types
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Aeson
import qualified Data.Aeson.Combinators.Decode as ACD
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Req
import Network.HTTP.Simple
import System.IO

import TransEnv
import TransState

decodeLibreTranslateResponse :: B.ByteString -> Maybe String
decodeLibreTranslateResponse bs = ACD.decode (ACD.at ["translatedText"] ACD.auto) bs

libreTranslate :: String -> String -> IO String
libreTranslate targetLang txt = runReq defaultHttpConfig $ do
    let payload =
            object [
                "source" .= ("auto" :: String),
                "target" .= (targetLang :: String),
                "q" .= (txt :: String)
            ]
    r <- req
            POST
            (http "localhost" /: "translate")
            (ReqBodyJson payload)
            lbsResponse
            (port 5001)
    let translatedString = decodeLibreTranslateResponse $ responseBody r
    return $ maybe "" id translatedString

translatorEventM :: ApiTranslator -> ReaderT TransEnv (EventM () TransState) ()
translatorEventM (transName, apiTrans) = do
    chan <- askApiChan
    toLang <- fmap targetLang ask
    lift $ do
        toTranslate <- fmap (fst . currentPassage) get
        void $ liftIO $ forkIO $ do
            suggested <- liftIO $ apiTrans toLang toTranslate
            writeBChan chan (TransSuggestionEvent (transName, suggested))

runTranslatorEvents :: ReaderT TransEnv (EventM () TransState) ()
runTranslatorEvents = do
    allTranslators <- fmap suggestors ask
    void $ sequence $ map translatorEventM allTranslators
