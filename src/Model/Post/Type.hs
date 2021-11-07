{-# LANGUAGE TemplateHaskell #-}

module Model.Post.Type where

import           Data.Int                   (Int64)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Text                  (Text)
import           Model.F                    (F)
import           Model.TH                   (genNewtypeT, makeTypeInstanceFWR)
import           Model.User.Type
import           Opaleye

type Title   = Text
type Content = Text

genNewtypeT "PostID" ''Int64 ''SqlInt8
makeAdaptorAndInstance "pPostID" ''PostIDT

data PostT a b c d
  = Post                -- ^ @TABLE post_table@
    { postID       :: a -- ^ @post_id bigserial PRIMARY KEY@
    , postAuthorID :: b -- ^ @post_author_id bigint REFERENCES blogger_table(blogger_id)@
    , postTitle    :: c -- ^ @post_title text NOT NULL@
    , postContent  :: d -- ^ @post_content text NOT NULL@
    -- TODO: rating of the post
    }

makeAdaptorAndInstance "pPost" ''PostT

type PostW = PostT (Maybe PostID) UserID Title Content
type PostR = PostT PostID         UserID Title Content
makeTypeInstanceFWR "Post"

postTable :: Table (F PostW) (F PostR)
postTable = table "post_table" $ pPost Post
  { postID        = pPostID $ PostID (tableField "post_id")
  , postAuthorID  = pUserID $ UserID (tableField "post_author_id")
  , postTitle     = tableField "post_title"
  , postContent   = tableField "post_content"
  }
