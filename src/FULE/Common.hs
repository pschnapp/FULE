module FULE.Common
 ( Orientation(..)
 --
 , Padding
 , padding
 , noPadding
 --
 , SizedContentSize
 , sizedTo
 , sizedToContents
 ) where


--------------------------------
-- Orientation
--------------------------------

-- | Layout orientation, used in multiple 'FULE.Container.Container's.
data Orientation = Horizontal | Vertical


--------------------------------
-- Padding
--------------------------------

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


--------------------------------
-- Sized Content
--------------------------------

-- | The size that the sized portion of a container should have;
--   see the 'FULE.Container.Divided.Divided' container for an example use.
type SizedContentSize a = Maybe a

-- | Use a set size for the sized portion of a container.
sizedTo :: a -> SizedContentSize a
sizedTo = Just

-- | Use the inherent size of the content for the sized portion of a container.
sizedToContents :: SizedContentSize a
sizedToContents = Nothing

