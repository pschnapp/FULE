module FULE.Internal.Util where

import Data.Maybe


collapseTo :: ([a] -> b) -> [Maybe a] -> Maybe b
collapseTo f ms =
  case catMaybes ms of
    [] -> Nothing
    ls -> Just (f ls)

getMaxSize :: [Maybe Int] -> Maybe Int
getMaxSize = collapseTo maximum

