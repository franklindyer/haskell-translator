{-# LANGUAGE TemplateHaskell #-}

module TransInterface where

import Brick
import Brick.Forms
import Brick.AttrMap
import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Border
import Brick.Widgets.Edit
import Control.Monad.State.Strict
import Data.Has
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Lens.Micro.TH.Internal

import Graphics.Vty (Event(..), Key(..), Modifier(..))

import TransState

transUI :: TransState -> [Widget ()]
transUI ts = [
        border $ (
            ((strWrap $ fst $ currentPassage ts)
                <=> hBorder
                <=> (strWrap $ suggestion ts))
            <+> vBorder
            <+> (strWrap $ unlines $ getEditContents $ scratch ts)
        )
    ] 
