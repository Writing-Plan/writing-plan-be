module Model.FamilyF where

-- | Map a Haskell type to its corresponding Sql type.
type family F a = b | b -> a
