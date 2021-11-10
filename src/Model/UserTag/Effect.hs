{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Model.UserTag.Effect where

import           Control.Algebra
import           Control.Monad.IO.Class     (MonadIO)
import           Data.Kind                  (Type)
import           Database.PostgreSQL.Simple (Connection)
import           Model.Helper
import           Model.PG
import           Model.Post.Table
import           Model.TH                   (sendAll)
import           Model.User.Table
import           Model.UserTag.Table
import           Opaleye
import Control.Comonad

data UserTag (m :: Type -> Type) k where
  InitUserTagTable :: UserTag m Bool
  AddUserTag       :: UserID -> PostID -> UserTagText -> UserTag m (Maybe ())
  DelUserTag       :: UserID -> PostID -> UserTagText -> UserTag m (Maybe ())
  ReportUserTag    :: UserID -> PostID -> UserTagText -> UserTag m (Maybe Bool)

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
        \  user_tag_id           bigserial PRIMARY KEY, \
        \  user_tag_post_id      bigint    REFERENCES post_table(post_id), \
        \  user_tag_text         text      NOT NULL, \
        \  user_tag_adder_id     bigint    REFERENCES user_table(user_id), \
        \  user_tag_reporters    bigint[]  NOT NULL DEFAULT '{}', \
        \  UNIQUE(user_tag_post_id, user_tag_text) \
        \);"
      AddUserTag userTagAdderID userTagPostID userTagText -> toMaybeUnit . (==1) <$> insert Insert
        { iTable      = userTagTable
        , iRows       = [toFields @UserTagW UserTag{userTagID = Nothing, userTagReporters = [], ..}]
        , iReturning  = rCount
        , iOnConflict = Just DoNothing
        }
      DelUserTag userTagAdderID' userTagPostID' userTagText' -> toMaybeUnit . (==1) <$> delete Delete
        { dTable     = userTagTable
        , dWhere     = \UserTag{..} -> 
              (userTagAdderID, userTagPostID, userTagText) .=== toFields (userTagAdderID', userTagPostID', userTagText')
        , dReturning = rCount
        }
      ReportUserTag userID userTagPostID' userTagText' -> withPG $ do
        reporterss :: [[UserID]] <- update' $ Update
          { uTable = userTagTable
          , uWhere = \UserTag{..} -> 
                (userTagPostID, userTagText) .=== toFields (userTagPostID', userTagText') .&&
                userTagAdderID ./== toFields userID .&&
                Opaleye.not (extract $ sqlElem <$> toFields userID <*> userTagReporters)
          , uUpdateWith = updateEasy $ \UserTag{..} ->
                UserTag {userTagReporters = arrayPrepend <$> toFields userID <*> userTagReporters, ..}
          , uReturning = rReturning userTagReporters
          }
        case reporterss of
          [reporters] -> if length reporters < 10
            then pure (Just False)
            else do
              delete' $ Delete
                { dTable     = userTagTable
                , dWhere     = \UserTag{..} -> 
                      (userTagPostID, userTagText) .=== toFields (userTagPostID', userTagText')
                , dReturning = rCount
                }
              -- Returning 0 means already deleted.
              pure (Just True)
          _ -> pure Nothing

