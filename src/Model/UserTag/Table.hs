{-# LANGUAGE TemplateHaskell #-}

module Model.UserTag.Table where

import           Data.Int                   (Int64)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Text                  (Text)
import           Model.F                    (F)
import           Model.Post.Table
import           Model.TH                   (genNewtypeT, makeTypeInstanceFWR)
import           Model.User.Table
import           Opaleye

genNewtypeT "UserTagText" ''Text ''SqlText
makeAdaptorAndInstance "pUserTagText" ''UserTagTextT

genNewtypeT "UserTagID" ''Int64 ''SqlInt8
makeAdaptorAndInstance "pUserTagID" ''UserTagIDT

data UserTagT a b c d e
  = UserTag                 -- ^ @TABLE user_tag_table@
    { userTagID        :: a -- ^ @user_tag_id bigserial PRIMARY KEY@
    , userTagPostID    :: b -- ^ @user_tag_post_id bigint REFERENCES post_table(post_id), UNIQUE(user_tag_post_id, user_tag_text)@
    , userTagText      :: c -- ^ @user_tag_text text NOT NULL@
    , userTagAdderID   :: d -- ^ @user_tag_adder_id REFERENCES user_table(user_id), UNIQUE(user_tag_post_id, user_tag_text)@
    , userTagReporters :: e -- ^ @user_tag_reporters bigint[] NOT NULL DEFAULT '{}'@
    }

makeAdaptorAndInstance "pUserTag" ''UserTagT

type UserTagW = UserTagT (Maybe UserTagID) PostID UserTagText UserID [UserID]
type UserTagR = UserTagT UserTagID         PostID UserTagText UserID [UserID]
makeTypeInstanceFWR "UserTag"

userTagTable :: Table (F UserTagW) (F UserTagR)
userTagTable = table "tag_table" $ pUserTag UserTag
  { userTagID          = pUserTagID   $ UserTagID   (tableField "user_tag_id")
  , userTagPostID      = pPostID      $ PostID      (tableField "user_tag_post_id")
  , userTagText        = pUserTagText $ UserTagText (tableField "user_tag_text")
  , userTagAdderID     = pUserID      $ UserID      (tableField "user_tag_adder_id")
  , userTagReporters   = pUserID      $ UserID      (tableField "user_tag_reporters")
  }
