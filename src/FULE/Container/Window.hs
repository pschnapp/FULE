{-# LANGUAGE FlexibleContexts #-}

module FULE.Container.Window
 ( Window
 , WindowAdjustorGen
 , window
 , layoutM
 , layout
 ) where

import Control.Arrow
import Data.Functor.Identity
import Data.Proxy

import FULE.Component
import FULE.Container
import FULE.Layout


-- | Type of a function to produce a 'FULE.Component.Component' to adjust
--   the 'FULE.Layout.Layout' in response to a change in the size of the window
--   in the encompassing GUI framework. The @Component@ should use the Guides
--   passed as arguments to this function to update the @Layout@.
type WindowAdjustorGen k
  =  GuideID
  -- ^ The Guide to use to adjust the /width/ of the 'FULE.Layout' in response
  --   to a change in the window size.
  -> GuideID
  -- ^ The Guide to use to adjust the /height/ of the 'FULE.Layout' in response
  --   to a change in the window size.
  -> k

-- | The base container of any (non-custom) 'FULE.Layout.Layout' representing
--   the window in the encompassing GUI framework. It is the only container that
--   can be used with the 'layout' and 'layoutM' functions to build a @Layout@.
data Window c k
  = Window
    { widthOf :: Int
    , heightOf :: Int
    , controlGenOf :: WindowAdjustorGen k
    , contentsOf :: c
    }

-- | Create a 'Window'.
window
  :: Int -- ^ The width of the window.
  -> Int -- ^ The height of the window.
  -> WindowAdjustorGen k
  -- ^ A function to construct a 'FULE.Component.Component' for reacting to
  --   changes in the size of the window in the encompassing GUI framework.
  -> c -- ^ The content of the window.
  -> Window c k
window width height = Window (max 0 width) (max 0 height)


-- | Build a layout for a 'Window' in the specified monad @m@.
layoutM :: (Container c k m) => Window c k -> m (Layout, [ComponentInfo k])
layoutM = (first build <$>) . runLayoutOp . makeLayoutOp

-- | Build a layout for a 'Window' in the 'Data.Functor.Identity.Identity' monad.
layout :: (Container c k Identity) => Window c k -> (Layout, [ComponentInfo k])
layout = first build . runIdentity . runLayoutOp . makeLayoutOp

makeLayoutOp :: (Container c k m) => Window c k -> LayoutOp k m ()
makeLayoutOp (Window w h gen c) = do
  top <- addGuideToLayout $ Absolute 0
  left <- addGuideToLayout $ Absolute 0
  right <- addGuideToLayout $ Absolute w
  bottom <- addGuideToLayout $ Absolute h
  let bounds = Bounds top left right bottom Nothing
  let proxy = Proxy :: Proxy k
  addToLayout (gen right bottom) proxy bounds Nothing
  addToLayout c proxy bounds Nothing

