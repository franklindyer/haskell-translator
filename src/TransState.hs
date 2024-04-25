{-# LANGUAGE TemplateHaskell #-}

module TransState where

import Brick
import Brick.BChan
import Brick.Widgets.Edit
import Control.Lens.TH
import Control.Monad.State.Strict
import Control.Monad.Trans.Reader
import qualified Graphics.Vty as V
import Lens.Micro

data TransState = TransState {
    currentIndex :: Int,
    currentPassage :: (String, String),
    suggestion :: String,
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
    suggestion = "",
    prevPassages = [],
    nextPassages = zip (tail ps) (repeat ""),
    scratch = editor () Nothing ""
}

loadPartialTranslation :: [String] -> TransState -> TransState
loadPartialTranslation ps ts = ts {
    prevPassages = prevLoaded,
    currentPassage = currentLoaded,
    nextPassages = nextLoaded,
    scratch = editor () Nothing (snd currentLoaded)
}
    where
        n = length $ prevPassages ts
        prevLoaded = zip (map fst $ prevPassages ts) (reverse $ take n ps)
        currentLoaded = (fst $ currentPassage ts, ps !! n)
        nextLoaded = zip (map fst $ nextPassages ts) (drop (n+1) ps)

currentSourcePassage :: TransState -> String
currentSourcePassage = fst . currentPassage

currentTargetPassage :: TransState -> String
currentTargetPassage = snd . currentPassage

allPassages :: TransState -> [(String, String)]
allPassages ts = reverse (prevPassages ts) ++ [currentPassage ts] ++ nextPassages ts

inferPassage :: TransState -> TransState
inferPassage ts
    = if currentTargetPassage ts == "" && nextPassages ts /= []
        then inferPassage (nextPassage ts)
        else ts

savePassage :: TransState -> TransState
savePassage ts = ts { currentPassage = savedPsg }
    where
        savedPsg = (fst $ currentPassage ts, head $ getEditContents $ scratch ts)

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

