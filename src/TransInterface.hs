module TransInterface where

import Brick
import Brick.Forms
import Brick.Widgets.Border
import Control.Monad.State.Strict
import qualified Graphics.Vty as V

import TransState

transUI :: TransState -> [Widget ()]
transUI ts = [
        border $
        (strWrap $ getPassage ts)
        <+> vBorder
        <+> (strWrap "Holamundo")
    ]

transApp :: App TransState TransEvent ()
transApp = App {
    appDraw = transUI,
    appChooseCursor = showFirstCursor,
    appHandleEvent = transAppEvent,
    appStartEvent = return (),
    appAttrMap = const $ attrMap V.defAttr []
}
