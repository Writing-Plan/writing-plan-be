module Model.F.Family where

-- | Map a Haskell type to its corresponding Sql type.
--
-- The fundep is currently not used anywhere.
-- If you're uncomfortable with the fundep, feel free to delete it.
type family F a = b | b -> a
