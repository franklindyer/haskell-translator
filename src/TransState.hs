module TransState where

import Brick
import Control.Monad.State.Strict
import qualified Graphics.Vty as V

data TransState = TransState {
    currentIndex :: Int,
    currentPassage :: (String, String),
    prevPassages :: [(String, String)],
    nextPassages :: [(String, String)]
} deriving (Eq, Show)

data TransEvent = TransSuggestion String

initTranslator :: [String] -> TransState
initTranslator ps = TransState {
    currentIndex = 0,
    currentPassage = (head ps, ""),
    prevPassages = [],
    nextPassages = zip (tail ps) (repeat "")
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
        prevPassages = (currentPassage ts):(prevPassages ts),
        nextPassages = ps
    }

prevPassage :: TransState -> TransState
prevPassage ts = case (prevPassages ts) of
    [] -> ts
    (p:ps) -> ts {
        currentIndex = currentIndex ts - 1,
        currentPassage = p,
        prevPassages = ps,
        nextPassages = (currentPassage ts):(nextPassages ts)
    }

transAppEvent :: BrickEvent () TransEvent -> EventM () TransState ()
transAppEvent e
    = case e of
        VtyEvent (V.EvKey V.KDown []) -> state (\ts -> ((), nextPassage ts))
        VtyEvent (V.EvKey V.KUp []) -> state (\ts -> ((), prevPassage ts))
        VtyEvent (V.EvKey V.KEsc []) -> halt
        VtyEvent _ -> state (\ts -> ((), ts))

transMakeApp :: App TransState TransEvent ()
transMakeApp = App {
    appDraw = undefined,
    appChooseCursor = showFirstCursor,
    appHandleEvent = transAppEvent,
    appStartEvent = return (),
    appAttrMap = const $ attrMap V.defAttr []
}
