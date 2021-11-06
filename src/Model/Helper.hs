module Model.Helper where

import Data.Bool (bool)

toMaybeUnit :: Bool -> Maybe ()
toMaybeUnit = bool (Just ()) Nothing