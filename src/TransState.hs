{-# LANGUAGE TemplateHaskell #-}

module TransState where

import Brick
import Brick.Widgets.Edit
import Control.Lens.TH
import Control.Monad.State.Strict
import qualified Graphics.Vty as V
import Lens.Micro

data TransState = TransState {
    currentIndex :: Int,
    currentPassage :: (String, String),
    prevPassages :: [(String, String)],
    nextPassages :: [(String, String)],
    scratch :: Editor String ()
}

data TransEvent = TransSuggestion String

makeLensesFor [("scratch", "scratchLens")] ''TransState

initTranslator :: [String] -> TransState
initTranslator ps = TransState {
    currentIndex = 0,
    currentPassage = (head ps, ""),
    prevPassages = [],
    nextPassages = zip (tail ps) (repeat ""),
    scratch = editor () Nothing ""
}

currentSourcePassage :: TransState -> String
currentSourcePassage = fst . currentPassage

currentTargetPassage :: TransState -> String
currentTargetPassage = snd . currentPassage

inferPassage :: TransState -> TransState
inferPassage ts
    = if currentTargetPassage ts == "" && nextPassages ts /= []
        then inferPassage (nextPassage ts)
        else ts

nextPassage :: TransState -> TransState
nextPassage ts = case (nextPassages ts) of
    [] -> ts
    (p:ps) -> ts {
        currentIndex = currentIndex ts + 1,
        currentPassage = p,
        prevPassages = savedPsg:(prevPassages ts),
        nextPassages = ps,
        scratch = editor () Nothing (snd p)
    }
    where
        savedPsg = (fst $ currentPassage ts, head $ getEditContents $ scratch ts)

prevPassage :: TransState -> TransState
prevPassage ts = case (prevPassages ts) of
    [] -> ts
    (p:ps) -> ts {
        currentIndex = currentIndex ts - 1,
        currentPassage = p,
        prevPassages = ps,
        nextPassages = savedPsg:(nextPassages ts),
        scratch = editor () Nothing (snd p)
    }
    where
        savedPsg = (fst $ currentPassage ts, head $ getEditContents $ scratch ts)

transAppEvent :: BrickEvent () TransEvent -> EventM () TransState ()
transAppEvent e
    = case e of
        VtyEvent (V.EvKey V.KDown []) -> state (\ts -> ((), nextPassage ts))
        VtyEvent (V.EvKey V.KUp []) -> state (\ts -> ((), prevPassage ts))
        VtyEvent (V.EvKey V.KEsc []) -> halt
        _ -> zoom scratchLens $ handleEditorEvent e

transMakeApp :: App TransState TransEvent ()
transMakeApp = App {
    appDraw = undefined,
    appChooseCursor = showFirstCursor,
    appHandleEvent = transAppEvent,
    appStartEvent = return (),
    appAttrMap = const $ attrMap V.defAttr []
}
