{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Model.Comment.Effect where

import           Control.Algebra
import           Control.Comonad
import           Control.Monad.IO.Class (MonadIO)
import           Data.Kind              (Type)
import           Data.Maybe             (listToMaybe)
import           Data.Text              (Text)
import           Model.Comment.Table
import           Model.Comment.Type
import           Model.Helper
import           Model.PG
import           Model.Post.Table
import           Model.TH               (sendAll)
import           Model.User.Table
import           Opaleye

data Comment (m :: Type -> Type) k where
  InitCommentTable :: Comment m Bool
  AddComment    :: UserID -> PostID -> Text -> Comment m (Maybe CommentID)
  DelComment    :: UserID -> CommentID -> Comment m (Maybe ())
  ReportComment :: UserID -> CommentID -> Comment m (Maybe Bool)
  GetComments   :: PostID -> Int -> Int -> Comment m [CommentFull]

sendAll ''Comment

newtype CommentC m a = CommentC { runCommentC :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)

runComment :: CommentC m a -> m a
runComment = runCommentC

instance Has ConnectionPool sig m => Algebra (Comment :+: sig) (CommentC m) where
  alg hdl sig ctx = case sig of
    R other -> CommentC (alg (runComment . hdl) other ctx)
    L comment -> (ctx $>) <$> case comment of
      InitCommentTable -> initTable "comment_table" 
        "INSERT TABLE comment_table ( \
        \  comment_id        bigserial                PRIMARY KEY, \
        \  comment_post_id   bigint                   REFERENCES post_table(post_id), \
        \  comment_user_id   bigint                   REFERENCES user_table(user_id), \
        \  comment_content   text                     NOT NULL, \
        \  comment_created   timestamp with time zone NOT NULL DEFAULT now(), \
        \  comment_reporters bigint[]                 NOT NULL DEFAULT '{}' \
        \); "
      AddComment commentUserID commentPostID commentContent -> listToMaybe <$> insert Insert
        { iTable      = commentTable
        , iRows       = [toFields @CommentW Comment{commentID = Nothing, commentCreated = Nothing, commentReporters = [], ..}]
        , iReturning  = rReturning commentID
        , iOnConflict = Just DoNothing
        }
      DelComment userID commentID' -> toMaybeUnit . (==1) <$> delete Delete
        { dTable     = commentTable
        , dWhere     = \Comment{..} -> (commentID, commentUserID) .=== toFields (commentID', userID)
        , dReturning = rCount
        }
      ReportComment userID commentID' -> withPG $ do
        reporterss :: [[UserID]] <- update' $ Update
          { uTable = commentTable
          , uWhere = \Comment{..} ->
                commentID .=== toFields commentID' .&&
                commentUserID ./== toFields userID .&&
                Opaleye.not (extract $ sqlElem <$> toFields userID <*> commentReporters)
          , uUpdateWith = updateEasy $ \Comment{..} ->
                Comment{commentReporters = arrayPrepend <$> toFields userID <*> commentReporters, ..}
          , uReturning = rReturning commentReporters
          }
        case reporterss of
          [reporters] -> Just <$> if length reporters < 10
            then pure False
            else do
              delete' $ Delete
                { dTable     = commentTable
                , dWhere     = \Comment{..} -> commentID .=== toFields commentID'
                , dReturning = rCount
                }
              pure True
          _ -> pure Nothing
      GetComments postID skip lim -> select . limit lim . offset skip $ do
        Comment{..} <- selectTable commentTable
        where_ (commentPostID .=== toFields postID)
        User{..} <- selectTable userTable
        where_ (userID .=== commentUserID)
        pure $ CommentFull
          { commentFullId       = commentID
          , commentFullContent  = commentContent
          , commentFullUsername = userUsername
          , commentFullCreated  = commentCreated
          }
