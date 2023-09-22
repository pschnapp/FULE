module FULE.Common.Orientation where

import Control.DeepSeq


-- | Layout orientation, used in multiple 'FULE.Container.Container's.
data Orientation = Horizontal | Vertical

instance NFData Orientation where
  rnf o = seq o ()

