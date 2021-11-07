{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Model.Post.Effect where

import           Control.Algebra
import           Control.Monad.IO.Class  (MonadIO)
import           Data.Functor            (($>))
import           Data.Kind               (Type)
import           Data.Maybe              (listToMaybe)
import           Data.Profunctor.Product
import           Model.Blogger.Type
import           Model.Helper
import           Model.PG
import           Model.Post.Type
import           Model.PostHasTag.Type
import           Model.TH                (sendAll)
import           Model.Tag.Type
import           Model.User.Type
import           Model.UserTag.Type
import           Opaleye

data Post (m :: Type -> Type) k where
  InitPostTable :: Post m Bool
  AddPost       :: BloggerID -> Title -> Content -> Post m (Maybe ())
  GetPost       :: PostID -> Post m (Maybe (Username, Title, Content, [TagText], [UserTagText]))

sendAll ''Post

newtype PostC m a = PostC { runPostC :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

runPost :: PostC m a -> m a
runPost = runPostC

instance Has ConnectionPool sig m => Algebra (Post :+: sig) (PostC m) where
  alg hdl sig ctx = case sig of
    R other       -> PostC (alg (runPostC . hdl) other ctx)
    L user -> (ctx $>) <$> case user of
      InitPostTable -> initTable "post_table"
        "CREATE TABLE post_table ( \
        \  post_id        bigserial PRIMARY KEY, \
        \  post_author_id bigint    REFERENCES blogger_table(blogger_id), \
        \  post_title     text      NOT NULL, \
        \  post_content   text      NOT NULL \
        \);"
      AddPost postAuthorID postTitle postContent -> toMaybeUnit . (==1) <$> insert Insert
        { iTable      = postTable
        , iRows       = [toFields @PostW Post{postID = Nothing, ..}]
        , iReturning  = rCount
        , iOnConflict = Just DoNothing
        }
      GetPost postID' -> fmap (p5 (id, id, id, sequence, sequence)) . listToMaybe <$> select do
        let postIDF = toFields postID'

        Post{..} <- selectTable postTable
        where_ (postID .=== postIDF)

        User{..} <- selectTable userTable
        where_ (userID .=== postAuthorID)

        tags <- aggregate (pTagText $ TagText arrayAgg) $ do
          PostHasTag{..} <- selectTable postHasTagTable
          where_ (postHasTagPostID .=== postIDF)
          pure postHasTagTagText

        userTags <- aggregate (pUserTagText $ UserTagText arrayAgg) $ do
          UserTag{..} <- selectTable userTagTable
          where_ (userTagPostID .=== postIDF)
          pure userTagText

        pure (userUsername, postTitle, postContent, tags, userTags)

-- F a = Column b
-- F (T a) = T (Column b)
-- F (Maybe (T a)) = T (Maybe (Column b))

-- F [a] = Column (SqlArray b) -- Manually
-- F [T a] = T (Column (SqlArray b)) = T (F [a]) -- TH
-- F (Maybe [T a]) = T (Maybe (Column (SqlArray b))) = T (Maybe (F [a])) -- TH
