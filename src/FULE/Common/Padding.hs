module FULE.Common.Padding where


-- | Visual padding around an element.
type Padding = (Int, Int)

-- | Padding to use; see parameters for details.
padding
  :: Int -- ^ Horizontal padding to use.
  -> Int -- ^ Vertical padding to use.
  -> Padding
padding horiz vert = (horiz, vert)

-- | Don't use any padding.
noPadding :: Padding
noPadding = (0, 0)

