module Util where

import Data.Maybe (listToMaybe)

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads
