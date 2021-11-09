{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Model.PG
  ( WithPool, withPool, ConnectionPool, WithPoolC, runWithPool
  , PG, initTable', select', insert', update', delete', PGC, runPG, withPG
  , initTable, select, insert, update, delete
  ) where

import           Control.Algebra
import           Control.Carrier.Reader
import           Control.Effect.Lift
import           Control.Monad                   (unless, void)
import           Control.Monad.IO.Class          (MonadIO (..))
import           Data.Functor                    (($>))
import           Data.Kind                       (Type)
import           Data.Pool                       (Pool, withResource)
import           Data.Profunctor.Product.Default (Default)
import           Data.String                     (fromString)
import           Database.PostgreSQL.Simple      (Connection, execute_, query_)
import           Model.TH                        (sendAll)
import           Opaleye

data WithPool a (m :: Type -> Type) k where
  -- May someone tell me how to use a more general type to replace 'IO' here!
  WithPool :: (a -> IO b) -> WithPool a m b

sendAll ''WithPool

type ConnectionPool = WithPool Connection

newtype WithPoolC a m k = WithPoolC { runWithPoolC :: ReaderC (Pool a) m k }
  deriving (Functor, Applicative, Monad, MonadIO)

runWithPool :: Pool a -> WithPoolC a m b -> m b
runWithPool pool = runReader pool . runWithPoolC

instance Has (Lift IO) sig m => Algebra (WithPool a :+: sig) (WithPoolC a m) where
  alg hdl sig ctx = case sig of
    R other -> WithPoolC (alg (runWithPoolC . hdl) (R other) ctx)
    L (WithPool f) -> do
      pool <- WithPoolC ask
      a <- sendM (pool `withResource` f)
      pure (ctx $> a)


data PG (m :: Type -> Type) k where
  InitTable' :: String -> String -> PG m Bool
  Select'    :: Default FromFields fields haskells => Select fields -> PG m [haskells]
  Insert'    :: Insert haskells -> PG m haskells
  Update'    :: Update haskells -> PG m haskells
  Delete'    :: Delete haskells -> PG m haskells

sendAll ''PG

withPG :: (Has (WithPool Connection) sig m) => PGC IO a -> m a
withPG m = withPool (`runPG` m)

initTable :: Has (WithPool Connection) sig m => String -> String -> m Bool
initTable tableName tableSchema = withPG (initTable' tableName tableSchema)

select :: (Has (WithPool Connection) sig m, Default FromFields fields haskells) => Select fields -> m [haskells]
select = withPG . select'

insert :: Has (WithPool Connection) sig m => Insert haskells -> m haskells
insert = withPG . insert'

update :: Has (WithPool Connection) sig m => Update haskells -> m haskells
update = withPG . update'

delete :: Has (WithPool Connection) sig m => Delete haskells -> m haskells
delete = withPG . delete'

newtype PGC m a = PGC { runPGC :: ReaderC Connection m a }
  deriving (Functor, Applicative, Monad, MonadIO)

runPG :: Connection -> PGC m a -> m a
runPG conn = runReader conn . runPGC

instance Has (Lift IO) sig m => Algebra (PG :+: sig) (PGC m) where
  alg hdl sig ctx = case sig of
    R other       -> PGC (alg (runPGC . hdl) (R other) ctx)
    L pg -> fmap (ctx $>) $ PGC ask >>= \conn -> sendM $ case pg of
      (Select' s)     ->  runSelect conn s
      (Insert' i)     ->  runInsert_ conn i
      (Update' u)     ->  runUpdate_ conn u
      (Delete' d)     ->  runDelete_ conn d
      InitTable' nm s -> do
        [[has]] <- query_ conn $
          "SELECT EXISTS (SELECT FROM information_schema.tables WHERE table_name = '" <> fromString nm <> "')"
        unless has $ void $ execute_ conn $ fromString s
        pure (Prelude.not has)

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
