module FULE.Container.Window
 ( Window
 , WindowControlGen
 , window
 , layout
 , layoutM
 ) where

import Control.Arrow
import Data.Functor.Identity
import Data.Proxy

import FULE.Container
import FULE.Layout


type WindowControlGen k = (GuideID, Int) -> (GuideID, Int) -> k

data Window c k
  = Window
    { widthOf :: Int
    , heightOf :: Int
    , controlGenOf :: WindowControlGen k
    , contentsOf :: c
    }

window :: Int -> Int -> WindowControlGen k -> c -> Window c k
window width height = Window (max 0 width) (max 0 height)


layout :: (Container c k) => Window c k -> (Layout, [Component k])
layout = first build . runIdentity . runLayoutOp . makeLayoutOp

layoutM :: (Container c k, Monad m) => Window c k -> m (Layout, [Component k])
layoutM = (first build <$>) . runLayoutOp . makeLayoutOp

makeLayoutOp :: (Container c k, Monad m) => Window c k -> LayoutOp k m ()
makeLayoutOp (Window w h gen c) = do
  top <- addGuideToLayout $ Absolute 0
  left <- addGuideToLayout $ Absolute 0
  right <- addGuideToLayout $ Absolute w
  bottom <- addGuideToLayout $ Absolute h
  let bounds = Bounds top left right bottom
  let proxy = Proxy :: Proxy k
  addToLayout (gen (right, w) (bottom, h)) proxy bounds Nothing
  addToLayout c proxy bounds Nothing

