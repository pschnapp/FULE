module Container.Window
 ( Window
 , window
 , layout
 ) where

import Control.Arrow
import Data.Proxy

import Container
import Layout


type ControlGen k = GuideID -> GuideID -> k

data Window c k
  = Window
    { widthOf :: Int
    , heightOf :: Int
    , controlGenOf :: ControlGen k
    , contentsOf :: c
    }

window :: Int -> Int -> ControlGen k -> c -> Window c k
window width height gen = Window (max 0 width) (max 0 height) gen


layout :: (Container c k) => Window c k -> (Layout, [Component k])
layout = first build . runLayoutOp . makeLayoutOp

makeLayoutOp :: (Container c k) => Window c k -> LayoutOp k ()
makeLayoutOp (Window w h gen c) = do
  top <- addGuideToLayout $ Absolute 0
  left <- addGuideToLayout $ Absolute 0
  right <- addGuideToLayout $ Absolute (w - 1)
  bottom <- addGuideToLayout $ Absolute (h - 1)
  let bounds = Bounds top left right bottom
  let proxy = Proxy :: Proxy k
  addToLayout (gen right bottom) proxy bounds Nothing
  addToLayout c proxy bounds Nothing

