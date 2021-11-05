{-# LANGUAGE TemplateHaskell #-}

module Model.Effects.TH (sendAll) where

import           Control.Algebra     (Has, send)
import           Control.Monad
import           Data.Char           (toLower)
import           Language.Haskell.TH

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
    def <- funD name
          [clause (varP <$> nms) (normalB (varE 'send `appE` foldl (\a b -> a `appE` varE b) (conE nm) nms)) []]

    if cnt /= 0
      then pure [def]
      else do
        sig <- newName "sig"
        m <- newName "m"
        -- conName :: Has tyName sig m => m x
        sigDef <- sigD name $
          forallT
            [PlainTV sig, PlainTV m]
            (pure [ConT ''Has `AppT` ConT tyName `AppT` VarT sig `AppT` VarT m])
            (varT m `appT` pure ty)
        pure [sigDef, def]
