module Container.Window
 ( Window
 , window
 , layout
 ) where

import Control.Arrow

import Container
import Layout


data Window c
  = Window
    { widthOf :: Int
    , heightOf :: Int
    , contentsOf :: c
    }

window :: Int -> Int -> c -> Window c
window = Window


layout :: (Container c k) => Window c -> (Layout, [Component k])
layout = first build . runLayoutOp . makeLayoutOp

makeLayoutOp :: (Container c k) => Window c -> LayoutOp k ()
makeLayoutOp (Window w h c) = do
  top <- addGuideToLayout $ Absolute 0
  left <- addGuideToLayout $ Absolute 0
  right <- addGuideToLayout $ Absolute (w - 1)
  bottom <- addGuideToLayout $ Absolute (h - 1)
  let bounds = Bounds top left right bottom
  addToLayout c bounds Nothing

