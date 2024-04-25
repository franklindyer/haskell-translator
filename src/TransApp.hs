{-# LANGUAGE TemplateHaskell #-}

module TransApp where

import Brick
import Brick.BChan
import Brick.Widgets.Edit
import Control.Concurrent
import Control.Lens.TH
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Trans.Reader
import qualified Graphics.Vty as V
import Lens.Micro
import System.IO

import TransAPIs
import TransEnv
import TransState
import TransInterface

transAppEvent :: BrickEvent () TransEvent -> ReaderT TransEnv (EventM () TransState) ()
transAppEvent e = do
    chan <- askApiChan
    toLang <- fmap targetLang ask
    lift $ case e of
        VtyEvent (V.EvKey V.KDown []) -> do
            modify nextPassage
            toTranslate <- fmap (fst . currentPassage) get
            void $ liftIO $ forkIO $ do
                suggested <- liftIO $ libreTranslate toLang toTranslate
                writeBChan chan (TransSuggestion suggested)
        VtyEvent (V.EvKey V.KUp []) -> do
            modify prevPassage
        VtyEvent (V.EvKey V.KEsc []) -> state (\ts -> ((), savePassage ts)) >> halt
        AppEvent (TransSuggestion s) -> state (\ts -> ((), ts { suggestion = s }))
        _ -> zoom scratchLens $ handleEditorEvent e

transMakeApp :: TransEnv -> App TransState TransEvent ()
transMakeApp env = App {
    appDraw = transUI,
    appChooseCursor = showFirstCursor,
    appHandleEvent = \ev -> runReaderT (transAppEvent ev) env,
    appStartEvent = return (),
    appAttrMap = const $ attrMap V.defAttr []
}
