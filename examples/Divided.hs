-- This module contains two examples of Divided containers (using `sizedTop`):
--  - one statically divided
--  - one dynamically divided
--
-- Note the type-annotation on the `Dynamics` for each divided container:
-- this is required since the type-checker cannot disambiguate `Widget`
-- as the type-parameter to `Dynamics` by itself.
-- Using the qualifiers here requires the ScopedTypeVariables language-extension.
-- You could also declare these values outside of the function bodies and
-- not need to use the language-extension.
--
{-# LANGUAGE ScopedTypeVariables #-}

module Divided where

import FULE


data Widget
  = ResizeControl
    { widthOf :: Int
    , heightOf :: Int
    -- These guides should be used to resize the `Layout` in response to
    -- the window being resized, using a delta.
    -- Note that you will have to wire this to the GUI framework yourself.
    , widthGuideOf :: GuideID
    , heightGuideOf :: GuideID
    }
  | UnsizedContent
  | SizedContent
  | DividerBar
    -- A Horizontal Orientation would call for vertical movement,
    -- so should be paired with watching for changes in the y-axis;
    -- likewise with Vertical and the x-axis.
    { orientationOf :: Orientation
    , movementGuideOf :: GuideID
    }

getWindowSize :: m (Int, Int)
getWindowSize = undefined -- for you to implement

staticDivided :: (Monad m) => m (Layout, [ComponentInfo Widget])
staticDivided = do
  (width, height) <- getWindowSize
  let staticSize :: Dynamics Widget = static
  return $ layout
    (window width height (ResizeControl width height)
      (sizedTop sizedToContents staticSize
        SizedContent
        UnsizedContent
        ))

dynamicDivided :: (Monad m) => m (Layout, [ComponentInfo Widget])
dynamicDivided = do
  (width, height) <- getWindowSize
  let dynamicSize :: Dynamics Widget = dynamic DividerBar 10
  return $ layout
    (window width height (ResizeControl width height)
      (sizedTop (sizedTo 100) dynamicSize
        -- Depending on the content, you may wish to use `clipped` to prevent
        -- it from displaying out of bounds when the container is resized.
        (clipped SizedContent)
        (clipped UnsizedContent)
        ))
