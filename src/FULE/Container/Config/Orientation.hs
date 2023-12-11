-- |
-- Module      : FULE.Container.Config.Orientation
-- Description : Visual orientation config.
-- Copyright   : (c) Paul Schnapp, 2023
-- License     : BSD3
-- Maintainer  : Paul Schnapp <paul.schnapp@gmail.com>
--
-- The @Orientation@ datatype is used to specify the visual orientation of an
-- element.
module FULE.Container.Config.Orientation where

import Control.DeepSeq


-- | Layout orientation, used in multiple 'FULE.Container.Container's.
data Orientation = Horizontal | Vertical

instance NFData Orientation where
  rnf o = seq o ()

