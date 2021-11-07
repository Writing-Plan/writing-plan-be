{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Model.UserTag.Effect where

import           Control.Algebra
import           Control.Monad.IO.Class     (MonadIO)
import           Data.Functor               (($>))
import           Data.Kind                  (Type)
import           Database.PostgreSQL.Simple (Connection)
import           Model.Helper
import           Model.PG
import           Model.Post.Type
import           Model.TH                   (sendAll)
import           Model.User.Type
import           Model.UserTag.Type
import           Opaleye


data UserTag (m :: Type -> Type) k where
  InitUserTagTable :: UserTag m Bool
  AddUserTag       :: UserID -> PostID -> UserTagText -> UserTag m (Maybe ())

sendAll ''UserTag

newtype UserTagC m a = UserTagC { runUserTagC :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

runUserTag :: UserTagC m a -> m a
runUserTag = runUserTagC

instance Has (WithPool Connection) sig m => Algebra (UserTag :+: sig) (UserTagC m) where
  alg hdl sig ctx = case sig of
    R other       -> UserTagC (alg (runUserTagC . hdl) other ctx)
    L user -> (ctx $>) <$> case user of
      InitUserTagTable -> initTable "user_tag_table"
        "CREATE TABLE user_tag_table ( \
        \  user_tag_id       bigserial PRIMARY KEY, \
        \  user_tag_post_id  bigint    REFERENCES post_table(post_id), \
        \  user_tag_text     text      NOT NULL, \
        \  user_tag_adder_id bigint    REFERENCES user_table(user_id), \
        \  UNIQUE(user_tag_post_id, user_tag_text) \
        \);"
      AddUserTag userTagAdderID userTagPostID userTagText -> toMaybeUnit . (==1) <$> insert Insert
        { iTable      = userTagTable
        , iRows       = [toFields @UserTagW UserTag{userTagID = Nothing, ..}]
        , iReturning  = rCount
        , iOnConflict = Just DoNothing
        }
