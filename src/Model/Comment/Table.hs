{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Model.Comment.Table where

import           Data.Int                   (Int64)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Text                  (Text)
import           Data.Time
import           Model.F                    (F)
import           Model.Post.Table
import           Model.TH
import           Model.User.Table
import           Opaleye

genNewtypeT "CommentID" ''Int64 ''SqlInt8
makeAdaptorAndInstance "pCommentID" ''CommentIDT

data CommentT a b c d e f
  = Comment                 -- ^ @TABLE comment_table@
    { commentID        :: a -- ^ @comment_id bigserial PRIMARY KEY@
    , commentPostID    :: b -- ^ @comment_post_id bigint REFERENCES post_table(post_id)@
    , commentUserID    :: c -- ^ @comment_user_id bigint REFERENCES user_table(user_id)@
    , commentContent   :: d -- ^ @comment_content text NOT NULL@
    , commentCreated   :: e -- ^ @comment_created timestamp with time zone NOT NULL DEFAULT now()@
    , commentReporters :: f -- ^ @comment_reporters bigint[] NOT NULL DEFAULT '{}'@
    }

makeAdaptorAndInstance "pComment" ''CommentT

type CommentW = CommentT (Maybe CommentID) PostID UserID Text (Maybe UTCTime) [UserID]
type CommentR = CommentT CommentID         PostID UserID Text UTCTime         [UserID]
makeTypeInstanceFWR "Comment"

commentTable :: Table (F CommentW) (F CommentR)
commentTable = table "comment_table" $ pComment Comment
  { commentID          = pCommentID $ CommentID (tableField "comment_id")
  , commentPostID      = pPostID    $ PostID    (tableField "comment_post_id")
  , commentUserID      = pUserID    $ UserID    (tableField "comment_user_id")
  , commentContent     = tableField "comment_content"
  , commentCreated     = tableField "comment_created"
  , commentReporters   = pUserID    $ UserID    (tableField "comment_reporters")
  }

