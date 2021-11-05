{-# LANGUAGE TemplateHaskell #-}

module Model.Effects.TH (sendAll) where

import           Control.Algebra
import           Control.Monad
import           Data.Maybe
import           GHC.Unicode         (toLower)
import           Language.Haskell.TH

lowerHead :: String -> String
lowerHead (x:xs) = toLower x : xs
lowerHead []     = undefined

nmAndCnt :: Con -> (Name, Int)
nmAndCnt = \case
  NormalC nm bts       -> (nm, length bts)
  RecC nm vbts         -> (nm, length vbts)
  ForallC _ _ con      -> nmAndCnt con
  GadtC [nm] bts _     -> (nm, length bts)
  RecGadtC [nm] vbts _ -> (nm, length vbts)
  _                    -> undefined

-- | Generate all then 'send' version of all constructors of a datatype
-- except those without any argument, since type inference in this case
-- won't work.
sendAll :: Name -> Q [Dec]
sendAll tyName = do
  TyConI (DataD _cxt _name _bnds _mk cons _derivs) <- reify tyName
  fmap catMaybes . forM cons $ \con -> do
    let (nm, cnt) = nmAndCnt con
    if cnt == 0
      then pure Nothing
      else do
        nms <- replicateM cnt (newName "x")
        -- conName x1 x2 ... = send (ConName x1 x2 ...)
        Just <$> funD (mkName $ lowerHead $ nameBase nm)
          [clause (varP <$> nms) (normalB (varE 'send `appE` foldl (\a b -> a `appE` varE b) (conE nm) nms)) []]
