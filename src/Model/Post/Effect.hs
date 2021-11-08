{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Model.Post.Effect where

import           Control.Algebra
import           Control.Monad.IO.Class  (MonadIO)
import           Data.Functor            (($>))
import           Data.Kind               (Type)
import           Data.List               (foldl1')
import           Data.Maybe              (listToMaybe)
import           Data.Text               (Text)
import           Model.Blogger.Table
import           Model.Helper
import           Model.PG
import           Model.Post.Table
import           Model.Post.Type
import           Model.PostHasTag.Table
import           Model.TH                (sendAll)
import           Model.Tag.Table
import           Model.User.Table
import           Model.UserTag.Table
import           Opaleye

data Post (m :: Type -> Type) k where
  InitPostTable :: Post m Bool
  AddPost       :: BloggerID -> Title -> Content -> Post m (Maybe ())
  GetPost       :: PostID -> Post m (Maybe PostFull)
  SearchPost    :: Maybe Username -> [Text] -> Int -> Int -> Post m [PostBrief]

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

      GetPost postID' -> fmap (pPostFull $ PostFull id id id sequence sequence) . listToMaybe <$> select do
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

        pure $ PostFull
          { postFullAuthor  = userUsername
          , postFullTitle   = postTitle
          , postFullContent = postContent
          , postFullTag     = tags
          , postFullUserTag = userTags
          }

      SearchPost maybeAuthorName keywords skip lim -> fmap (fmap (pPostBrief $ PostBrief id id id sequence sequence)) $
        select $ limit lim . offset skip $ do
          Post{..} <- selectTable postTable

          User{..} <- selectTable userTable
          where_ (userID .=== postAuthorID)

          tags <- aggregate (pTagText $ TagText arrayAgg) $ do
            PostHasTag{..} <- selectTable postHasTagTable
            where_ (postHasTagPostID .=== postID)
            pure postHasTagTagText

          userTags <- aggregate (pUserTagText $ UserTagText arrayAgg) $ do
            UserTag{..} <- selectTable userTagTable
            where_ (userTagPostID .=== postID)
            pure userTagText

          let condAuthor = case maybeAuthorName of
                Nothing         -> []
                Just authorName -> [userUsername .=== toFields authorName]
              condKeyword = (\keyword -> let keywordF = toFields keyword in
                      keywordF `sqlElem` unTagText tags
                  .|| keywordF `sqlElem` unUserTagText userTags
                  -- Don't know if it could be injected
                  .|| postTitle `like` toFields ("_" <> keyword <> "_")
                ) <$> keywords

          where_ $ foldl1' (.&&) $ condAuthor ++ condKeyword

          pure $ PostBrief
            { postBriefID      = postID
            , postBriefAuthor  = userUsername
            , postBriefTitle   = postTitle
            , postBriefTag     = tags
            , postBriefUserTag = userTags
            }

-- F a = Column b
-- F (T a) = T (Column b)
-- F (Maybe (T a)) = T (Maybe (Column b))

-- F [a] = Column (SqlArray b) -- Manually
-- F [T a] = T (Column (SqlArray b)) = T (F [a]) -- TH
-- F (Maybe [T a]) = T (Maybe (Column (SqlArray b))) = T (Maybe (F [a])) -- TH
