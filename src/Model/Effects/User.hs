{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Model.Effects.User (User, initUserTable, isEmailAvail, addUser, checkUser, UserC, runUser) where

import           Control.Algebra
import           Control.Monad.IO.Class (MonadIO)
import           Data.Functor           (($>))
import           Data.Kind              (Type)
import           Data.Maybe             (listToMaybe)
import           Model.Effects.PG
import           Model.Effects.TH       (sendAll)
import           Model.Type
import           Opaleye

data User (m :: Type -> Type) k where
  InitUserTable :: User m Bool
  IsEmailAvail  :: Email -> User m Bool
  AddUser       :: Email -> Username -> Password -> User m (Maybe UserID)
  CheckUser     :: Email -> Password -> User m (Maybe (UserID, Username))

-- initUserTable :: Has User sig m => m Bool
-- initUserTable = send InitUserTable

sendAll ''User

newtype UserC m a = UserC { runUserC :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

runUser :: UserC m a -> m a
runUser = runUserC

instance Has PG sig m => Algebra (User :+: sig) (UserC m) where
  alg hdl sig ctx = case sig of
    R other       -> UserC (alg (runUserC . hdl) other ctx)
    L user -> (ctx $>) <$> case user of
      InitUserTable -> initTable "user_table"
        "CREATE TABLE user_table ( \
        \   user_id   serial  PRIMARY KEY, \
        \   email     text    NOT NULL UNIQUE, \
        \   username  text    NOT NULL, \
        \   passwd    bytea   NOT NULL \
        \);"
      IsEmailAvail email' -> fmap Prelude.null . select $ do
        User{..} <- selectTable userTable
        where_ $ email .=== toFields email'
      AddUser email name passwd -> fmap listToMaybe . insert $ Insert
        { iTable      = userTable
        , iRows       = [toFields @UserW User{userID = Nothing, ..}]
        , iReturning  = rReturning userID
        , iOnConflict = Just DoNothing
        }
      CheckUser email' passwd' -> fmap listToMaybe . select $ do
        User{..} <- selectTable userTable
        where_ $ email .=== toFields email' .&& passwd .=== toFields passwd'
        pure (userID, name)
