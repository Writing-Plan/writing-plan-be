{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Model.Tag.Effect where

import           Control.Algebra
import           Control.Monad.IO.Class (MonadIO)
import           Data.Functor           (($>))
import           Data.Kind              (Type)
import           Model.Helper
import           Model.PG
import           Model.TH               (sendAll)
import           Model.Tag.Type
import           Opaleye


data Tag (m :: Type -> Type) k where
  InitTagTable :: Tag m Bool
  AddTag       :: TagText -> Tag m (Maybe ())

sendAll ''Tag

newtype TagC m a = TagC { runTagC :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

runTag :: TagC m a -> m a
runTag = runTagC

instance Has ConnectionPool sig m => Algebra (Tag :+: sig) (TagC m) where
  alg hdl sig ctx = case sig of
    R other       -> TagC (alg (runTagC . hdl) other ctx)
    L user -> (ctx $>) <$> case user of
      InitTagTable -> initTable "tag_table"
        "CREATE TABLE tag_table ( \
        \  tag_id bigserial PRIMARY KEY, \
        \  tag    text      NOT NULL \
        \);"
      AddTag tag -> toMaybeUnit . (==1) <$> insert Insert
        { iTable      = tagTable
        , iRows       = [toFields @TagW Tag{tagID = Nothing, ..}]
        , iReturning  = rCount
        , iOnConflict = Just DoNothing
        }
