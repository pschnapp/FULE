-- This module contains example usages of `Grid`, `GridM`, `Item`, and `ItemM`.
--
{-# LANGUAGE ScopedTypeVariables #-}

module Grid where

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
  | Content String


exampleGrid :: Int -> Int -> (Layout, [ComponentInfo Widget])
exampleGrid width height = layout
  (window width height (ResizeControl width height)
    (grid (2, 3)
      -- Annoyingly a type-annotation is required for the list of items which
      -- must be contained within an extra set of parentheses.
      ([item (Content "A")
      , item (Content "B")
      , item (Content "C")
      , item (Content "D")
      , item (Content "E")
      , item (Content "F")
      ]::[Item Widget])))


getWindowSize :: m (Int, Int)
getWindowSize = undefined -- for you to implement

-- When using `ItemM` you'll need to specify a `forall` for your monadic
-- type-variable in the type-signature and use the `ScopedTypeVariables`
-- language-extension to get the type-checker to use the same variable for the
-- type-signature and the type-annotation of your `ItemM` list in the body.
exampleGridM :: forall m . (Monad m) => m (Layout, [ComponentInfo Widget])
exampleGridM = do
  (width, height) <- getWindowSize
  layoutM
    (window width height (ResizeControl width height)
      (grid (2, 3)
        -- Annoyingly a type-annotation is required for the list of items which
        -- must be contained within an extra set of parentheses.
        ([item (Content "A")
        , item (Content "B")
        , item (Content "C")
        , item (Content "D")
        , item (Content "E")
        , item (Content "F")
        ]::[ItemM m Widget])))
