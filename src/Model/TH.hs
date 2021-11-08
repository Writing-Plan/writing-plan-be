{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.TH where

import           Control.Algebra                 (Has, send)
import           Control.Applicative             (Applicative (liftA2))
import           Control.Monad                   (forM, replicateM)
import           Data.Aeson
import           Data.Char                       (toLower)
import           Data.Profunctor.Product.Default (Default (..))
import           Data.Serialize                  (Serialize)
import           Data.String                     (IsString)
import           Language.Haskell.TH
import           Model.F.Family                  (F)

-- HLS will crash when importing 'Opaleye'
#ifdef BUILD
import           Opaleye                         (Field, ToFields)
#endif

lowerHead :: String -> String
lowerHead (x:xs) = toLower x : xs
lowerHead []     = undefined

nmCntTy :: Con -> (Name, Int, Type)
nmCntTy = \case
  ForallC _ _ con       -> nmCntTy con
  GadtC [nm] bts ty     -> (nm, length bts, ty)
  RecGadtC [nm] vbts ty -> (nm, length vbts, ty)
  _                     -> undefined

-- | Generate the 'send' version of all constructors of a datatype.
sendAll :: Name -> Q [Dec]
sendAll tyName = do
  TyConI (DataD _cxt _name _bnds _mk cons _derivs) <- reify tyName
  fmap concat . forM cons $ \con -> do
    let (nm, cnt, _ `AppT` ty) = nmCntTy con
        name = mkName $ lowerHead $ nameBase nm

    nms <- replicateM cnt (newName "x")
        -- conName x1 x2 ... = send (ConName x1 x2 ...)
    def_ <- funD name
          [clause (varP <$> nms) (normalB (varE 'send `appE` foldl (\a b -> a `appE` varE b) (conE nm) nms)) []]

    if cnt /= 0
      then pure [def_]
      else do
        sig <- newName "sig"
        m <- newName "m"
        -- conName :: Has tyName sig m => m x
        sigDef <- sigD name $
          forallT
            [PlainTV sig, PlainTV m]
            (pure [ConT ''Has `AppT` ConT tyName `AppT` VarT sig `AppT` VarT m])
            (varT m `appT` pure ty)
        pure [sigDef, def_]

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
        [DerivClause Nothing (ConT <$> 
          [''Functor, ''Eq, ''Show, ''Serialize, ''Foldable, ''Traversable, ''FromJSON, ''ToJSON])]
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
