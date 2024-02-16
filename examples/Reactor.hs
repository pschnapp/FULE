-- This module contains a full example of a Presenter in a Model-View-Presenter
-- architecture utilizing `Reactor`s.
--
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reactor where

import Control.Monad
import Data.Maybe
import FULE


--------------------------------
-- VIEW
--
-- This section contains types and functions which represent the interaction
-- with the View of the MVP.
--------------------------------

data UIMessage
  = WindowSizeChanged
    { windowWidthOf :: Int
    , windowHeightOf :: Int
    }
  | MouseMoved
    { xPositionOf :: Int
    , yPositionOf :: Int
    , xDeltaOf :: Int
    , yDeltaOf :: Int
    }

data ForView = ForView

instance Semigroup ForView where
  a <> b = ForView

instance Monoid ForView where
  mempty = ForView

getUIMessage :: (Monad m) => m UIMessage
getUIMessage = undefined -- for you to implement

interactWithView :: (Monad m) => ForView -> m UIMessage
interactWithView = undefined -- for you to implement


--------------------------------
-- MODEL
--
-- This section contains types and functions which represent the interaction
-- with the Model of the MVP.
--------------------------------

data FromModel

data ForModel = ForModel

instance Semigroup ForModel where
  a <> b = ForModel

instance Monoid ForModel where
  mempty = ForModel

interactWithModel :: (Monad m) => ForModel -> m FromModel
interactWithModel = undefined -- for you to implement


--------------------------------
-- Reactor
--
-- This section contains types used to interact with a Reactor's contents
-- via the Reaction typeclass.
--
-- These types are generic so that the same set of data constructors could be
-- used with multiple MVP triads.
--------------------------------

data ReactorInput mi
  = LayoutInput Layout Bounds -- bounds for a component
  | ModelInput mi
  | NoInput
  | ViewInput UIMessage

-- GADT for selecting the `getProduct`'s output type (the `o` param)
data ReactorOutput mo po vo o where
  LayoutOutput :: ReactorOutput mo po vo (Layout -> Layout)
  ModelOutput :: ReactorOutput mo po vo mo
  NoOutput :: ReactorOutput mo po vo ()
  PresenterOutput :: ReactorOutput mo po vo po
  ViewOutput :: ReactorOutput mo po vo vo


--------------------------------
-- Window Control
--------------------------------

data WindowWidget
  = ResizeControl
    { currentWidthOf :: Int
    , currentHeightOf :: Int
    , deltaWidthOf :: Int
    , deltaHeightOf :: Int
    , widthGuideOf :: GuideID
    , heightGuideOf :: GuideID
    }

instance Reaction WindowWidget (ReactorInput mi) (ReactorOutput mo po vo) where
  addReactant (ViewInput (WindowSizeChanged w h)) _ ww =
    -- record the delta at a window size change
    ww
    { currentWidthOf = w
    , currentHeightOf = h
    , deltaWidthOf = w - currentWidthOf ww
    , deltaHeightOf = h - currentHeightOf ww
    }
  addReactant _ _ ww = -- catchall for other input cases
    -- reset the deltas
    ww { deltaWidthOf = 0, deltaHeightOf = 0 }
  getProduct _ LayoutOutput ww =
    -- react to a window size change if there has been one
    if dw == 0 && dh == 0 then Nothing else Just . reactToChanges
    . (if dw /= 0 then ((wg, dw):) else id)
    . (if dh /= 0 then ((hg, dh):) else id)
    $ []
    where
      ResizeControl
        { deltaWidthOf = dw
        , deltaHeightOf = dh
        , widthGuideOf = wg
        , heightGuideOf = hg
        } = ww
  getProduct _ _ _ = Nothing -- catchall for other output cases

makeWindowWidget
  :: (Monad m)
  => Int -> Int -> GuideID -> GuideID
  -> Reactor (ReactorInput mi) (ReactorOutput mo po vo) m
makeWindowWidget w h wg hg = reactor (ResizeControl w h 0 0 wg hg)


--------------------------------
-- UI Widgets
--------------------------------

data UIWidget = UIWidget

instance Reaction UIWidget (ReactorInput mi) (ReactorOutput mo po vo) where
  addReactant _ _ uw = uw
  getProduct _ _ _ = Nothing

makeUIWidget
  :: (Monad m)
  => Reactor (ReactorInput mi) (ReactorOutput mo po vo) m
makeUIWidget = reactor UIWidget


--------------------------------
-- Presenter
--------------------------------

data ForPresenter
  = NotQuit
  | Quit
  deriving (Eq)

hasQuit :: [ForPresenter] -> Bool
hasQuit = elem Quit


-- some convenience aliases
type RIN = ReactorInput FromModel
type ROUT = ReactorOutput ForModel ForPresenter ForView
type R = Reactor RIN ROUT
type I m = ItemM m (R m)
type CIRs m = [ComponentInfo (R m)]


getWindowSize :: (Monad m) => m (Int, Int)
getWindowSize = undefined -- for you to implement

-- When using `ItemM` you'll need to specify a `forall` for your monadic
-- type-variable in the type-signature and use the `ScopedTypeVariables`
-- language-extension to get the type-checker to use the same variable for the
-- type-signature and the type-annotation of your `ItemM` list in the body.
genLayout :: forall m . (Monad m) => m (Layout, [ComponentInfo (R m)])
genLayout = do
  (width, height) <- getWindowSize
  layoutM
    (window (width, height) (makeWindowWidget width height)
      (grid (2, 3)
        -- Annoyingly type-annotations are required for the list of items which
        -- must be contained within an extra set of parentheses.
        ([item (makeUIWidget :: R m)
        , item (makeUIWidget :: R m)
        , item (makeUIWidget :: R m)
        , item (makeUIWidget :: R m)
        , item (makeUIWidget :: R m)
        , item (makeUIWidget :: R m)
        ]::[I m])))


-- for whatever reason the compiler has trouble resolving the types of the
-- Reaction methods so we have to do it manually:

addReact :: (Monad m) => RIN -> ROUT o -> R m -> R m
addReact = addReactant

getProd :: (Monad m) => RIN -> ROUT o -> R m -> Maybe o
getProd = getProduct


addInput :: (Monad m) => RIN -> CIRs m -> CIRs m
addInput input = map (addReact input NoOutput <$>)

getOutputs :: (Monad m) => ROUT o -> CIRs m -> [o]
getOutputs outputType = mapMaybe (getProd NoInput outputType . componentOf)

adjustComponentLayouts
  :: (Monad m) => Layout -> CIRs m -> CIRs m
adjustComponentLayouts layout =
  map (\i -> addReact (LayoutInput layout (boundsOf i)) NoOutput <$> i)


presenterUI :: (Monad m) => m ()
presenterUI = do
  (layout, infos) <- genLayout
  viewIn <- getUIMessage
  -- prime each component with layout info
  let infos' = adjustComponentLayouts layout infos
  presenterUILoop layout infos' viewIn

presenterUILoop :: (Monad m) => Layout -> CIRs m -> UIMessage -> m ()
presenterUILoop layout infos0 viewIn = do
  -- VIEW (contd)
  let infos1 = addInput (ViewInput viewIn) infos0
  -- PRESENTER
  let presenterOut = getOutputs PresenterOutput infos1
  unless (hasQuit presenterOut) $ do
    -- MODEL
    let modelOut = mconcat $ getOutputs ModelOutput infos1
    modelIn <- interactWithModel modelOut
    let infos2 = addInput (ModelInput modelIn) infos1
    -- LAYOUT
    let adjustLayout = foldl (.) id $ getOutputs LayoutOutput infos2
    let layout' = adjustLayout layout
    let infos3 = adjustComponentLayouts layout' infos2
    -- VIEW (begin)
    let viewOut = mconcat $ getOutputs ViewOutput infos3
    viewIn' <- interactWithView viewOut
    -- LOOP
    presenterUILoop layout' infos3 viewIn'
    -- PSA: DON'T BIND LIKE THIS FOR THE RECURSION AS IT'S NOT TAIL-RECURSIVE:
    --presenterUILoop layout' infos3 =<< interactWithView viewOut

