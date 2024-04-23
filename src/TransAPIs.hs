{-# LANGUAGE OverloadedStrings #-}

module TransAPIs where

import Network.HTTP.Simple

import Data.Aeson
import Network.HTTP.Req


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
            jsonResponse
            (port 5001)
    return $ show $ (responseBody r :: Value)
