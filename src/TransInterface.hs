module TransInterface where

import Brick
import Brick.Forms
import Brick.Widgets.Border
import Brick.Widgets.Edit
import Control.Monad.State.Strict
import Data.Has
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Lens.Micro.TH.Internal

import TransState

--instance Has Text String where
--    getter = pack
--    modifier f s = unpack $ f (pack s)

transUI :: TransState -> [Widget ()]
transUI ts = [
        border $
        (strWrap $ fst $ currentPassage ts)
        <+> vBorder
        <+> (renderEditor (str . unlines) True $ scratch ts)
    ]

transApp :: App TransState TransEvent ()
transApp = App {
    appDraw = transUI,
    appChooseCursor = showFirstCursor,
    appHandleEvent = transAppEvent,
    appStartEvent = return (),
    appAttrMap = const $ attrMap V.defAttr []
}
