module TransState where

import Brick
import Control.Monad.State.Strict
import qualified Graphics.Vty as V

data TransState = TransState {
    currentPassage :: Int,
    passages :: [(String, String)]
} deriving (Eq, Show)

data TransEvent = TransSuggestion String

initTranslator :: [String] -> TransState
initTranslator ps = TransState {
    currentPassage = 0, 
    passages = zip ps (repeat "")
}

inferPassage :: TransState -> TransState
inferPassage ts = ts { currentPassage = ind } 
    where
        ps = passages ts
        allDone = and $ map (not . null . snd) ps
        ind = snd $ head $ filter (null . snd . fst) $ zip ps [0..]

nextPassage :: TransState -> TransState
nextPassage ts = ts { currentPassage = min (1 + currentPassage ts) (length (passages ts) - 1) }

prevPassage :: TransState -> TransState
prevPassage ts = ts { currentPassage = max (-1 + currentPassage ts) 0 }

getPassage :: TransState -> String
getPassage ts = fst $ (passages ts) !! (currentPassage ts)

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
