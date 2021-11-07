{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Model.Article.Effect where

import           Control.Algebra
import           Control.Monad.IO.Class     (MonadIO)
import           Data.Functor               (($>))
import           Data.Kind                  (Type)
import           Data.Maybe                 (listToMaybe)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Model.Article.Type
import           Model.ArticleHasTag.Type
import           Model.Blogger.Type
import           Model.Helper
import           Model.PG
import           Model.TH                   (sendAll)
import           Model.Tag.Type
import           Model.User.Type
import           Model.UserTag.Type
import           Opaleye

data Article (m :: Type -> Type) k where
  InitArticleTable :: Article m Bool
  AddArticle       :: BloggerID -> Title -> Content -> Article m (Maybe ())
  GetArticle       :: ArticleID -> Article m (Maybe (Username, Title, Content, [TagText], [UserTagText]))

sendAll ''Article

newtype ArticleC m a = ArticleC { runArticleC :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

runArticle :: ArticleC m a -> m a
runArticle = runArticleC

makeAdaptorAndInstance "pTuple2" ''(,)

instance Has ConnectionPool sig m => Algebra (Article :+: sig) (ArticleC m) where
  alg hdl sig ctx = case sig of
    R other       -> ArticleC (alg (runArticleC . hdl) other ctx)
    L user -> (ctx $>) <$> case user of
      InitArticleTable -> initTable "article_table"
        "CREATE TABLE article_table ( \
        \  article_id bigserial PRIMARY KEY, \
        \  author_id  bigint    REFERENCES blogger_table(blogger_id), \
        \  title      text   NOT NULL, \
        \  content    text   NOT NULL \
        \);"
      AddArticle authorID title content -> toMaybeUnit . (==1) <$> insert Insert
        { iTable      = articleTable
        , iRows       = [toFields @ArticleW Article{articleID = Nothing, ..}]
        , iReturning  = rCount
        , iOnConflict = Just DoNothing
        }
      GetArticle articleID' -> fmap listToMaybe . select $ do
        let articleIDF = toFields articleID'

        Article{..} <- selectTable articleTable
        where_ (articleID .=== articleIDF)

        User{..} <- selectTable userTable
        where_ (userID .=== authorID)

        tags <- aggregate arrayAgg $ do
          ArticleHasTag{..} <- selectTable articleHasTagTable
          where_ (hasArticleID .=== articleIDF)
          Tag{..} <- selectTable tagTable
          where_ (tagID .=== hasTagID)
          pure tag

        userTags <- aggregate arrayAgg $ do
          UserTag{..} <- selectTable userTagTable
          where_ (tagArticleID .=== articleIDF)
          pure userTag

        pure (name, title, content, tags, userTags)

-- F a = Column b
-- F (T a) = T (Column b)
-- F (Maybe (T a)) = T (Maybe (Column b))

-- F [a] = Column (SqlArray b) -- Manually
-- F [T a] = T (Column (SqlArray b)) = T (F [a]) -- TH
-- F (Maybe [T a]) = T (Maybe (Column (SqlArray b))) = T (Maybe (F [a])) -- TH
