{-# LANGUAGE TemplateHaskell #-}

module Model.PostHasTag.Table where

import           Data.Int                   (Int64)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Model.F                    (F)
import           Model.Post.Table
import           Model.TH                   (genNewtypeT, makeTypeInstanceFWR)
import           Model.Tag.Table
import           Model.User.Table
import           Opaleye

genNewtypeT "PostHasTagID" ''Int64 ''SqlInt8
makeAdaptorAndInstance "pPostHasTagID" ''PostHasTagIDT

data PostHasTagT a b c d e
  = PostHasTag                   -- ^ @TABLE post_has_tag_table@
    { postHasTagID          :: a -- ^ @post_has_tag_id bigserial PRIMARY KEY@
    , postHasTagPostID      :: b -- ^ @post_has_tag_post_id bigint REFERENCES post_table(post_id)@
    , postHasTagTagText     :: c -- ^ @post_has_tag_tag_text bigint REFERENCES tag_table(tag_id)@
    , postHasTagAdderID     :: d -- ^ @post_has_tag_user_id bigint REFERENCES user_table(user_id)@
    , postHasTagReportCount :: e -- ^ @post_has_tag_report_count int NOT NULL@
    }

makeAdaptorAndInstance "pPostHasTag" ''PostHasTagT

type PostHasTagW = PostHasTagT (Maybe PostHasTagID) PostID TagText UserID Int
type PostHasTagR = PostHasTagT PostHasTagID         PostID TagText UserID Int
makeTypeInstanceFWR "PostHasTag"

postHasTagTable :: Table (F PostHasTagW) (F PostHasTagR)
postHasTagTable = table "post_table" $ pPostHasTag PostHasTag
  { postHasTagID          = pPostHasTagID $ PostHasTagID (tableField "post_has_tag_id")
  , postHasTagPostID      = pPostID       $ PostID       (tableField "post_has_tag_post_id")
  , postHasTagTagText     = pTagText      $ TagText      (tableField "post_has_tag_tag_text")
  , postHasTagAdderID     = pUserID       $ UserID       (tableField "post_has_tag_user_id")
  , postHasTagReportCount = tableField "post_has_tag_report_count"
  }
