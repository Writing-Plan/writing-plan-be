{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Model.UserTag.Effect where

import           Control.Algebra
import           Control.Monad.IO.Class     (MonadIO)
import           Data.Functor               (($>))
import           Data.Kind                  (Type)
import           Database.PostgreSQL.Simple (Connection)
import           Model.Article.Type         (ArticleID)
import           Model.Helper
import           Model.PG
import           Model.TH                   (sendAll)
import           Model.User.Type
import           Model.UserTag.Type
import           Opaleye


data UserTag (m :: Type -> Type) k where
  InitUserTagTable :: UserTag m Bool
  AddUserTag       :: UserID -> ArticleID -> UserTagText -> UserTag m (Maybe ())

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
        \  user_tag_id    bigserial PRIMARY KEY, \
        \  tag_article_id bigint    REFERENCES article_table(article_id), \
        \  user_tag       text      NOT NULL, \
        \  add_user_id    bigint    REFERENCES user_table(user_id), \
        \  UNIQUE(tag_article_id, user_tag) \
        \);"
      AddUserTag addUserID tagArticleID userTag -> toMaybeUnit . (==1) <$> insert Insert
        { iTable      = userTagTable
        , iRows       = [toFields @UserTagW UserTag{userTagID = Nothing, ..}]
        , iReturning  = rCount
        , iOnConflict = Just DoNothing
        }
