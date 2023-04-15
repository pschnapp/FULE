module Container.Divided
 ( Divided
 , Dynamics
 , dynamic
 , static
 , Sizing
 , sizedTop
 , sizedLeft
 , sizedRight
 , sizedBottom
 ) where

import Container
import Internal.Util
import Layout


data Sizing = SizedTop | SizedLeft | SizedRight | SizedBottom


data Dynamics b
  = Dynamic
    { contentsGenOf :: GuideID -> b
    , barSizeOf :: Int
    }
  | Static

barSizeFor :: Dynamics b -> Maybe Int
barSizeFor (Dynamic _ s) = Just s
barSizeFor Static = Nothing

dynamic :: (GuideID -> b) -> Int -> Dynamics b
dynamic barGen size = Dynamic barGen (max 0 size)

static :: Dynamics b
static = Static


data Divided s b u
  = Divided
    { sizingOf :: Sizing
    , sizeOf :: Int
    , dynamicsOf :: Dynamics b
    , sizedOf :: s
    , unconstrainedOf :: u
    }

instance (Container s b, Container u b) => Container (Divided s b u) b where
  requiredWidth divided p =
    let sizedWidth = requiredWidth (sizedOf divided) p
    in case sizingOf divided of
      SizedLeft  -> makeSize [sizedWidth, barSizeFor (dynamicsOf divided)]
      SizedRight -> makeSize [sizedWidth, barSizeFor (dynamicsOf divided)]
      _          -> sizedWidth
  requiredHeight divided p =
    let sizedHeight = requiredHeight (sizedOf divided) p
    in case sizingOf divided of
      SizedTop    -> makeSize [sizedHeight, barSizeFor (dynamicsOf divided)]
      SizedBottom -> makeSize [sizedHeight, barSizeFor (dynamicsOf divided)]
      _           -> sizedHeight
  addToLayout divided bounds renderGroup =
    case sizingOf divided of
      SizedTop -> makeDivided divided bounds renderGroup
        DivisionConfig
        { setUnconInnerOf = \g b -> b { topOf = g }
        , setSizedInnerOf = \g b -> b { bottomOf = g }
        , setSizedOuterOf = topOf
        , multiplierOf = 1
        }
      SizedLeft -> makeDivided divided bounds renderGroup
        DivisionConfig
        { setUnconInnerOf = \g b -> b { leftOf = g }
        , setSizedInnerOf = \g b -> b { rightOf = g }
        , setSizedOuterOf = leftOf
        , multiplierOf = 1
        }
      SizedRight -> makeDivided divided bounds renderGroup
        DivisionConfig
        { setUnconInnerOf = \g b -> b { rightOf = g }
        , setSizedInnerOf = \g b -> b { leftOf = g }
        , setSizedOuterOf = rightOf
        , multiplierOf = -1
        }
      SizedBottom -> makeDivided divided bounds renderGroup
        DivisionConfig
        { setUnconInnerOf = \g b -> b { bottomOf = g }
        , setSizedInnerOf = \g b -> b { topOf = g }
        , setSizedOuterOf = bottomOf
        , multiplierOf = -1
        }

makeSize :: [Maybe Int] -> Maybe Int
makeSize = collapseTo sum


data DivisionConfig
  = DivisionConfig
    { setUnconInnerOf :: GuideID -> Bounds -> Bounds
    , setSizedInnerOf :: GuideID -> Bounds -> Bounds
    , setSizedOuterOf :: Bounds -> GuideID
    , multiplierOf :: Int
    }

makeDivided
 :: (Container s b, Container u b)
 => Divided s b u -> Bounds -> RenderGroup -> DivisionConfig -> LayoutOp b ()
makeDivided divided bounds renderGroup config = do
  -- sized
  sizedInner <- addGuideToLayout $ Relative (m*(size-1)) (getSizedOuter bounds) Asymmetric
  addToLayout sized (setSizedInner sizedInner bounds) renderGroup
  -- bar
  ref <- case dynamics of
    Dynamic barGen barSize -> do
      barSized <- addGuideToLayout $ Relative m sizedInner Symmetric
      barUncon <- addGuideToLayout $ Relative (m*(barSize-1)) barSized Symmetric
      -- yes the 'sized' and 'unconstrained' are supposed to be mixed here:
      let barBounds = setSizedInner barUncon . setUnconInner barSized $ bounds
      addComponent $ Component barBounds (barGen barSized) renderGroup
      return barUncon
    Static -> return sizedInner
  -- unconstrained
  unconstrainedInner <- addGuideToLayout $ Relative m ref Symmetric
  addToLayout unconstrained (setUnconInner unconstrainedInner bounds) renderGroup
  where
    Divided
      { sizeOf = size
      , dynamicsOf = dynamics
      , sizedOf = sized
      , unconstrainedOf = unconstrained
      } = divided
    DivisionConfig
      { setUnconInnerOf = setUnconInner
      , setSizedInnerOf = setSizedInner
      , setSizedOuterOf = getSizedOuter
      , multiplierOf = m
      } = config


sizedTop :: Int -> Dynamics b -> s -> u -> Divided s b u
sizedTop = Divided SizedTop . max 0

sizedLeft :: Int -> Dynamics b -> s -> u -> Divided s b u
sizedLeft = Divided SizedLeft . max 0

sizedRight :: Int -> Dynamics b -> s -> u -> Divided s b u
sizedRight = Divided SizedRight . max 0

sizedBottom :: Int -> Dynamics b -> s -> u -> Divided s b u
sizedBottom = Divided SizedBottom . max 0

