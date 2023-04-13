module Internal.Util where

import Data.Maybe


collapseTo :: ([a] -> a) -> [Maybe a] -> Maybe a
collapseTo f ms =
  case catMaybes ms of
    [] -> Nothing
    ls -> Just (f ls)

getMaxSize :: [Maybe Int] -> Maybe Int
getMaxSize = collapseTo maximum

