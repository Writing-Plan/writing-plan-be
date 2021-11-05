{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.TH where

import           Data.Profunctor.Product.Default (Default (..))
import           Data.Serialize                  (Serialize)
import           Data.String                     (IsString)
import           Language.Haskell.TH

#ifdef BUILD
import           Opaleye                         (Field, ToFields)
#endif

genNewtypeT :: String -> Name -> Name -> Q [Dec]
genNewtypeT nm hsTy pgTy = do
  b <- isInstance ''IsString [ConT hsTy]

  let [name, nameT, nameF, maybeNameF, unName, a, f, x] =
        mkName <$> [nm, nm ++ "T", nm ++ "F", "Maybe" ++ nm ++ "F", "un" ++ nm, "a", "f", "x"]
      noBang = Bang NoSourceUnpackedness NoSourceStrictness
#ifdef BUILD
      [nmField, nmToFields] = [''Field, ''ToFields]
#else
      [nmField, nmToFields] = mkName <$> ["Opaleye.Field", "Opaleye.ToFields"]
#endif

      -- newtype 'nameT' a = 'name' { 'unName' :: a } deriving (Functor, Eq, Show, Serialize)
      newtyD = NewtypeD [] nameT [PlainTV a] Nothing (RecC name [(unName, noBang, VarT a)])
        [DerivClause Nothing (ConT <$> [''Functor, ''Eq, ''Show, ''Serialize])]
      -- type 'name' = 'nameT' 'hsTy'
      hsSyn = TySynD name [] $ ConT nameT `AppT` ConT hsTy
      -- type 'nameF' = 'nameT' (Field 'pgTy')
      pgSynF = TySynD nameF [] $ ConT nameT `AppT` (ConT nmField `AppT` ConT pgTy)
      -- type 'maybeNameF' = 'nameT' (Maybe (Field 'pgTy'))
      pgSynMaybe = TySynD maybeNameF [] $ ConT nameT `AppT` (ConT ''Maybe `AppT` (ConT nmField `AppT` ConT pgTy))

  insts <- [d|
    instance Applicative $(conT nameT) where
      pure = $(conE name)
      $(conP name [varP f]) <*> $(conP name [varP x]) = $(conE name) ($(varE f) $(varE x))

    instance $(conT maybeNameF) ~ fields => Default $(conT nmToFields) (Maybe $(conT name)) fields where
      def = toToFields toMaybeF
        where
          toMaybeF (Just n) = fmap (Just . toFields) n
          toMaybeF Nothing  = pure Nothing
    |]
  isStringInst <- if b
    then [d|
      deriving instance IsString $(conT name)
    |]
    else pure []

  pure $ [newtyD, hsSyn, pgSynF, pgSynMaybe] ++ insts ++ isStringInst
