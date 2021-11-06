{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Model.ArticleHasTag.Effect where

import           Control.Algebra
import           Control.Monad.IO.Class   (MonadIO)
import           Data.Functor             (($>))
import           Data.Kind                (Type)
import           Model.Article.Type
import           Model.ArticleHasTag.Type
import           Model.Helper
import           Model.PG
import           Model.TH                 (sendAll)
import           Model.Tag.Type
import           Model.User.Type
import           Opaleye

data ArticleHasTag (m :: Type -> Type) k where
  InitArticleHasTagTable :: ArticleHasTag m Bool
  AddTagForArticle       :: UserID -> ArticleID -> TagID -> ArticleHasTag m (Maybe ())
  DelTagForArticle       :: UserID -> ArticleID -> TagID -> ArticleHasTag m (Maybe ())
  ReportTagForArticle    :: UserID -> ArticleID -> TagID -> ArticleHasTag m (Maybe Bool)

sendAll ''ArticleHasTag

newtype ArticleHasTagC m a = ArticleHasTagC { runArticleHasTagC :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

runArticleHasTag :: ArticleHasTagC m a -> m a
runArticleHasTag = runArticleHasTagC

instance Has PG sig m => Algebra (ArticleHasTag :+: sig) (ArticleHasTagC m) where
  alg hdl sig ctx = case sig of
    R other       -> ArticleHasTagC (alg (runArticleHasTagC . hdl) other ctx)
    L user -> (ctx $>) <$> case user of
      InitArticleHasTagTable -> initTable "article_has_tag_table"
        "CREATE TABLE article_has_tag_table ( \
        \  article_has_tag_id bigserial PRIMARY KEY, \
        \  has_article_id     bigint    REFERENCES article_table(article_id), \
        \  has_tag_id         bigint    REFERENCES tag_table(tag_id), \
        \  adder_id           bigint    REFERENCES user_table(user_id), \
        \  report_count       int       NOT NULL \
        \);"
      AddTagForArticle adderID hasArticleID hasTagID -> toMaybeUnit . (==1) <$> insert Insert
        { iTable      = articleHasTagTable
        , iRows       = [toFields @ArticleHasTagW ArticleHasTag{articleHasTagID = Nothing, reportCount = 0, ..}]
        , iReturning  = rCount
        , iOnConflict = Just DoNothing
        }
      DelTagForArticle adderID' hasArticleID' hasTagID' -> toMaybeUnit . (==1) <$> delete Delete
        { dTable     = articleHasTagTable
        , dWhere     = \ArticleHasTag{..} ->
              (adderID, hasArticleID, hasTagID) .=== toFields (adderID', hasArticleID', hasTagID')
        , dReturning = rCount
        }
      ReportTagForArticle userID hasArticleID' hasTagID' -> do
          -- How to use one connection in the two operations?
          counts <- update $ Update
            { uTable = articleHasTagTable
            , uWhere = \ArticleHasTag{..} ->
                  (hasArticleID, hasTagID) .=== toFields (hasArticleID', hasTagID') .&& adderID ./== toFields userID
            , uUpdateWith = updateEasy $ \ArticleHasTag{..} -> ArticleHasTag {reportCount = reportCount + 1, ..}
            , uReturning = rReturning reportCount
            }
          case counts of
            [cnt] -> if cnt < id @Int 10
              then pure (Just False)
              else do
                delete $ Delete
                  { dTable     = articleHasTagTable
                  , dWhere     = \ArticleHasTag{..} ->
                        (hasArticleID, hasTagID) .=== toFields (hasArticleID', hasTagID')
                  , dReturning = rCount
                  }
                -- Returning 0 means already deleted.
                pure (Just True)
            _ -> pure Nothing
