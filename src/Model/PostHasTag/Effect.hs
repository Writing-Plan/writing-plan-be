{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Model.PostHasTag.Effect where

import           Control.Algebra
import           Control.Monad.IO.Class (MonadIO)
import           Data.Functor           (($>))
import           Data.Kind              (Type)
import           Model.Helper
import           Model.PG
import           Model.Post.Type
import           Model.PostHasTag.Type
import           Model.TH               (sendAll)
import           Model.Tag.Type
import           Model.User.Type
import           Opaleye

data PostHasTag (m :: Type -> Type) k where
  InitPostHasTagTable :: PostHasTag m Bool
  AddTagForPost       :: UserID -> PostID -> TagText -> PostHasTag m (Maybe ())
  DelTagForPost       :: UserID -> PostID -> TagText -> PostHasTag m (Maybe ())
  ReportTagForPost    :: UserID -> PostID -> TagText -> PostHasTag m (Maybe Bool)

sendAll ''PostHasTag

newtype PostHasTagC m a = PostHasTagC { runPostHasTagC :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

runPostHasTag :: PostHasTagC m a -> m a
runPostHasTag = runPostHasTagC

instance Has ConnectionPool sig m => Algebra (PostHasTag :+: sig) (PostHasTagC m) where
  alg hdl sig ctx = case sig of
    R other       -> PostHasTagC (alg (runPostHasTagC . hdl) other ctx)
    L user -> (ctx $>) <$> case user of
      InitPostHasTagTable -> initTable "post_has_tag_table"
        "CREATE TABLE post_has_tag_table ( \
        \  post_has_tag_id           bigserial PRIMARY KEY, \
        \  post_has_tag_post_id      bigint    REFERENCES post_table(post_id), \
        \  post_has_tag_tag_id       bigint    REFERENCES tag_table(tag_id), \
        \  post_has_tag_user_id      bigint    REFERENCES user_table(user_id), \
        \  post_has_tag_report_count int       NOT NULL \
        \);"
      AddTagForPost postHasTagAdderID postHasTagPostID postHasTagTagText -> toMaybeUnit . (==1) <$> insert Insert
        { iTable      = postHasTagTable
        , iRows       = [toFields @PostHasTagW PostHasTag{postHasTagID = Nothing, postHasTagReportCount = 0, ..}]
        , iReturning  = rCount
        , iOnConflict = Just DoNothing
        }
      DelTagForPost postHasTagAdderID' postHasTagPostID' postHasTagTagText' -> toMaybeUnit . (==1) <$> delete Delete
        { dTable     = postHasTagTable
        , dWhere     = \PostHasTag{..} ->
              (postHasTagAdderID, postHasTagPostID, postHasTagTagText) .=== 
                toFields (postHasTagAdderID', postHasTagPostID', postHasTagTagText')
        , dReturning = rCount
        }
      ReportTagForPost userID postHasTagPostID' postHasTagTagText' -> withPG $ do
          counts <- update' $ Update
            { uTable = postHasTagTable
            , uWhere = \PostHasTag{..} ->
                  (postHasTagPostID, postHasTagTagText) .=== toFields (postHasTagPostID', postHasTagTagText') .&& 
                    postHasTagAdderID ./== toFields userID
            , uUpdateWith = updateEasy $ \PostHasTag{..} -> 
                  PostHasTag {postHasTagReportCount = postHasTagReportCount + 1, ..}
            , uReturning = rReturning postHasTagReportCount
            }
          case counts of
            [cnt] -> if cnt < id @Int 10
              then pure (Just False)
              else do
                delete' $ Delete
                  { dTable     = postHasTagTable
                  , dWhere     = \PostHasTag{..} ->
                        (postHasTagPostID, postHasTagTagText) .=== toFields (postHasTagPostID', postHasTagTagText')
                  , dReturning = rCount
                  }
                -- Returning 0 means already deleted.
                pure (Just True)
            _ -> pure Nothing
