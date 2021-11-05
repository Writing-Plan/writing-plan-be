{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.TH where

import           Control.Applicative             (Applicative (liftA2))
import           Data.Profunctor.Product.Default (Default (..))
import           Data.Serialize                  (Serialize)
import           Data.String                     (IsString)
import           Language.Haskell.TH
import           Model.FamilyF                   (F)

-- HLS will crash when importing 'Opaleye'
#ifdef BUILD
import           Opaleye                         (Field, ToFields)
#endif

genNewtypeT :: String -> Name -> Name -> Q [Dec]
genNewtypeT nm hsTy pgTy = do
  b <- isInstance ''IsString [ConT hsTy]

  let [name, nameT, unName, a, f, x] =
        mkName <$> [nm, nm ++ "T", "un" ++ nm, "a", "f", "x"]
      noBang = Bang NoSourceUnpackedness NoSourceStrictness

      -- It has been said that 'Field' will soon replace 'Column',
      -- and it is suggested to use the 'Field' type family for now.
      -- However, doing so will make the type family 'F' non-injective.
#ifdef BUILD
      [nmField, nmToFields] = [''Column, ''ToFields]
#else
      [nmField, nmToFields] = mkName <$> ["Opaleye.Column", "Opaleye.ToFields"]
#endif

      -- newtype 'nameT' a = 'name' { 'unName' :: a } deriving (Functor, Eq, Show, Serialize)
      newtyD = NewtypeD [] nameT [PlainTV a] Nothing (RecC name [(unName, noBang, VarT a)])
        [DerivClause Nothing (ConT <$> [''Functor, ''Eq, ''Show, ''Serialize])]
      -- type 'name' = 'nameT' 'hsTy'
      hsSyn = TySynD name [] $ ConT nameT `AppT` ConT hsTy

      -- -- type 'nameF' = 'nameT' (Field 'pgTy')
      -- pgSynF = TySynD nameF [] $ ConT nameT `AppT` (ConT nmField `AppT` ConT pgTy)
      -- -- type 'maybeNameF' = 'nameT' (Maybe (Field 'pgTy'))
      -- pgSynMaybe = TySynD maybeNameF [] $ ConT nameT `AppT` (ConT ''Maybe `AppT` (ConT nmField `AppT` ConT pgTy))

  insts <- [d|
    type instance F $(conT name) = $(conT nameT) ($(conT nmField) $(conT pgTy))

    type instance F (Maybe $(conT name)) = $(conT nameT) (Maybe ($(conT nmField) $(conT pgTy)))

    instance Applicative $(conT nameT) where
      pure = $(conE name)
      $(conP name [varP f]) <*> $(conP name [varP x]) = $(conE name) ($(varE f) $(varE x))

    instance F (Maybe $(conT name)) ~ fields => Default $(conT nmToFields) (Maybe $(conT name)) fields where
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

  pure $ [newtyD, hsSyn] ++ insts ++ isStringInst

makeTypeInstanceF :: Name -> Q [Dec]
makeTypeInstanceF tyName = do
  TyConI (TySynD _ _ ty) <- reify tyName
  let tyF = go ty
        where
          -- go :: TypeF Type -> Type
          go (a `AppT` b) = go a `AppT` (ConT ''F `AppT` b)
          go (ConT a)     = ConT a
          go _            = undefined
  [d|
    type instance F $(conT tyName) = $(pure tyF)|]

makeTypeInstanceFWR :: String -> Q [Dec]
makeTypeInstanceFWR = liftA2 (++) <$> make . (++ "W") <*> make . (++ "R")
  where
    make = makeTypeInstanceF . mkName
