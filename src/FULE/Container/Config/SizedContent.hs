-- |
-- Module      : FULE.Container.Config.SizedContent
-- Description : Content size config.
-- Copyright   : (c) Paul Schnapp, 2023
-- License     : BSD3
-- Maintainer  : Paul Schnapp <paul.schnapp@gmail.com>
--
-- Datatype and functions for specifying the size of content.
module FULE.Container.Config.SizedContent where


-- | The size that the sized portion of a container should have;
--   see the 'FULE.Container.Divided.Divided' container for an example use.
type SizedContentSize a = Maybe a

-- | Use a set size for the sized portion of a container.
sizedTo :: a -> SizedContentSize a
sizedTo = Just

-- | Use the inherent size of the content for the sized portion of a container.
sizedToContents :: SizedContentSize a
sizedToContents = Nothing

