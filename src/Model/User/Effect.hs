{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Model.User.Effect
  ( User, initUserTable, isEmailAvail, addUser, checkUser
  , UserC, runUser
  ) where

import           Control.Algebra
import           Control.Monad.IO.Class (MonadIO)
import           Data.Functor           (($>))
import           Data.Kind              (Type)
import           Data.Maybe             (listToMaybe)
import           Model.PG
import           Model.TH               (sendAll)
import           Model.User.Table
import           Opaleye

data User (m :: Type -> Type) k where
  InitUserTable :: User m Bool
  IsEmailAvail  :: Email -> User m Bool
  AddUser       :: Email -> Username -> Password -> User m (Maybe UserID)
  CheckUser     :: Email -> Password -> User m (Maybe (UserID, Username))

sendAll ''User

newtype UserC m a = UserC { runUserC :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

runUser :: UserC m a -> m a
runUser = runUserC

instance Has ConnectionPool sig m => Algebra (User :+: sig) (UserC m) where
  alg hdl sig ctx = case sig of
    R other       -> UserC (alg (runUserC . hdl) other ctx)
    L user -> (ctx $>) <$> case user of
      InitUserTable -> initTable "user_table"
        "CREATE TABLE user_table ( \
        \  user_id       bigserial PRIMARY KEY, \
        \  user_email    text      NOT NULL UNIQUE, \
        \  user_username text      NOT NULL, \
        \  user_passwd   bytea     NOT NULL \
        \);"
      IsEmailAvail userEmail' -> fmap Prelude.null . select $ do
        User{..} <- selectTable userTable
        where_ $ userEmail .=== toFields userEmail'
      AddUser userEmail userUsername userPasswd -> fmap listToMaybe . insert $ Insert
        { iTable      = userTable
        , iRows       = [toFields @UserW $ User {userID = Nothing, ..}]
        , iReturning  = rReturning userID
        , iOnConflict = Just DoNothing
        }
      CheckUser userEmail' userPasswd' -> fmap listToMaybe . select $ do
        User{..} <- selectTable userTable
        where_ $ userEmail .=== toFields userEmail' .&& userPasswd .=== toFields userPasswd'
        pure (userID, userUsername)
