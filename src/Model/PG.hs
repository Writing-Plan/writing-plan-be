{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Model.PG
  ( WithPool, withPool, WithPoolC, runWithPool
  , PG, initTable, select, insert, update, delete, PGC, runPG
  ) where

import           Control.Algebra
import           Control.Carrier.Reader
import           Control.Monad                   (unless, void)
import           Control.Monad.IO.Class          (MonadIO (..))
import           Data.Functor                    (($>))
import           Data.Kind                       (Type)
import           Data.Pool                       (Pool, withResource)
import           Data.Profunctor.Product.Default (Default)
import           Data.String                     (fromString)
import           Database.PostgreSQL.Simple      (Connection, execute_, query_)
import           Model.TH                        (sendAll)
import           Opaleye                         hiding (Delete, Insert, Update)
import qualified Opaleye                         as O

data WithPool a (m :: Type -> Type) k where
  -- May someone tell me how to use a more general type to replace 'IO' here!
  WithPool :: (a -> IO b) -> WithPool a m b

sendAll ''WithPool

newtype WithPoolC a m k = WithPoolC { runWithPoolC :: ReaderC (Pool a) m k }
  deriving (Functor, Applicative, Monad, MonadIO)

runWithPool :: Pool a -> WithPoolC a m b -> m b
runWithPool pool = runReader pool . runWithPoolC

instance (MonadIO m, Algebra sig m) => Algebra (WithPool a :+: sig) (WithPoolC a m) where
  alg hdl sig ctx = case sig of
    R other -> WithPoolC (alg (runWithPoolC . hdl) (R other) ctx)
    L (WithPool f) -> do
      pool <- WithPoolC ask
      a <- liftIO (pool `withResource` f)
      pure (ctx $> a)


data PG (m :: Type -> Type) k where
  InitTable :: String -> String -> PG m Bool
  Select    :: Default FromFields fields haskells => Select fields -> PG m [haskells]
  Insert    :: O.Insert haskells -> PG m haskells
  Update    :: O.Update haskells -> PG m haskells
  Delete    :: O.Delete haskells -> PG m haskells

sendAll ''PG

newtype PGC m a = PGC { runPGC :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

runPG :: PGC m a -> m a
runPG = runPGC

instance Has (WithPool Connection) sig m => Algebra (PG :+: sig) (PGC m) where
  alg hdl sig ctx = case sig of
    R other       -> PGC (alg (runPGC . hdl) other ctx)
    L pg -> (ctx $>) <$> case pg of
      InitTable nm s -> withPool $ \conn -> do
        [[has]] <- query_ conn $
          "SELECT EXISTS (SELECT FROM information_schema.tables WHERE table_name = '" <> fromString nm <> "')"
        unless has $ void $ execute_ conn $ fromString s
        pure (Prelude.not has)
      Select s       -> withPool (`runSelect` s)
      Insert i       -> withPool (`runInsert_` i)
      Update i       -> withPool (`runUpdate_` i)
      Delete i       -> withPool (`runDelete_` i)

-- Not currently used. Just put here.

-- data Redis (m :: Type -> Type) k where
--   Redis :: Redis.Redis a -> Redis m a

-- redis :: Has Redis sig m => Redis.Redis a -> m a
-- redis = send . Redis

-- newtype RedisC m a = RedisC { runRedisC :: m a}
--   deriving (Applicative, Functor, Monad, MonadIO)

-- instance (MonadIO m, Has (Reader Redis.Connection ) sig m) => Algebra (Redis :+: sig) (RedisC m) where
--   alg hdl sig ctx = case sig of
--     L (Redis r) -> do
--       conn <- ask
--       (ctx $>) <$> liftIO (Redis.runRedis conn r)
--     R other     -> RedisC (alg (runRedisC . hdl) other ctx)
