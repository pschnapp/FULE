-- This module contains an example using `Layout` (and `LayoutDesign`).
--
-- `Layout` is a low-level construct of this library; you may wish to use one of
-- the higher-level constructs if you're not doing anything super custom, or you
-- can use the higher-level constructs then augment the result using `Layout`
-- afterward.
--
module Layout where

import Control.Arrow -- `second`
import Control.Monad.State
import FULE


makeLayout :: Int -> Int -> ([Bounds], Layout)
makeLayout width height =
  second build -- we have to `build` the `LayoutDesign`
  $ runState (designLayout width height) emptyLayoutDesign

-- make a layout for a divided window with a resize bar
designLayout :: Int -> Int -> State LayoutDesign [Bounds]
designLayout width height = do
  --
  -- window
  windowTop <- state . addGuide $ Absolute 0
  windowLeft <- state . addGuide $ Absolute 0
  windowRight <- state . addGuide $ Absolute width
  windowBottom <- state . addGuide $ Absolute height
  --
  -- resize bar
  resizeBarBottom <- state . addGuide $ Relative (-100) windowBottom Asymmetric
  resizeBarTop <- state . addGuide $ Relative (-10) resizeBarBottom Symmetric
  -- constraints keep the resize bar within the bounds of the window when moving
  modify . addGuideConstraint $ resizeBarBottom `LTE` windowBottom
  modify . addGuideConstraint $ resizeBarTop `GTE` windowTop
  --
  -- make the bounds for the various areas
  let window =
        Bounds windowTop windowLeft windowRight windowBottom Nothing
  let resizeBar =
        Bounds resizeBarTop windowLeft windowRight resizeBarBottom Nothing
  let unsizedContent = -- top portion of the screen
        Bounds windowTop windowLeft windowRight resizeBarTop Nothing
  let sizedContent = -- bottom portion of the screen
        Bounds resizeBarBottom windowLeft windowRight windowBottom Nothing
  return [window, unsizedContent, resizeBar, sizedContent]
