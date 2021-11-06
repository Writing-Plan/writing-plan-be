{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Model.Article.Effect where

import           Control.Algebra
import           Control.Monad.IO.Class (MonadIO)
import           Data.Functor           (($>))
import           Data.Kind              (Type)
import           Model.Article.Type
import           Model.Blogger.Type
import           Model.PG
import           Model.TH               (sendAll)
import           Opaleye


data Article (m :: Type -> Type) k where
  InitArticleTable :: Article m Bool
  AddArticle       :: BloggerID -> Title -> Content -> Article m Bool

sendAll ''Article

newtype ArticleC m a = ArticleC { runArticleC :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

runArticle :: ArticleC m a -> m a
runArticle = runArticleC

instance Has PG sig m => Algebra (Article :+: sig) (ArticleC m) where
  alg hdl sig ctx = case sig of
    R other       -> ArticleC (alg (runArticleC . hdl) other ctx)
    L user -> (ctx $>) <$> case user of
      InitArticleTable -> initTable "article_table"
        "CREATE TABLE article_table ( \
        \  article_id serial PRIMARY KEY, \
        \  author_id  int    REFERENCES blogger_table(blogger_id), \
        \  title      text   NOT NULL, \
        \  content    text   NOT NULL \
        \);"
      AddArticle authorID title content -> fmap (==1) . insert $ Insert
        { iTable      = articleTable
        , iRows       = [toFields @ArticleW $ Article {articleID = Nothing, ..}]
        , iReturning  = rCount
        , iOnConflict = Just DoNothing
        }
