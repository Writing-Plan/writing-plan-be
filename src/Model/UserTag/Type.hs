{-# LANGUAGE TemplateHaskell #-}

module Model.UserTag.Type where

import           Data.Int                   (Int64)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Text                  (Text)
import           Model.Article.Type
import           Model.F                    (F)
import           Model.TH                   (genNewtypeT, makeTypeInstanceFWR)
import           Model.User.Type
import           Opaleye

type UserTagText = Text

genNewtypeT "UserTagID" ''Int64 ''SqlInt8
makeAdaptorAndInstance "pUserTagID" ''UserTagIDT

data UserTagT a b c d
  = UserTag             -- ^ @TABLE user_tag_table@
    { userTagID    :: a -- ^ @user_tag_id bigserial PRIMARY KEY@
    , tagArticleID :: b -- ^ @tag_article_id bigint REFERENCES article_table(article_id)@
    , userTag      :: c -- ^ @user_tag text NOT NULL@
    , addUserID    :: d -- ^ @add_user_id REFERENCES user_table(user_id)@
    }

makeAdaptorAndInstance "pUserTag" ''UserTagT

type UserTagW = UserTagT (Maybe UserTagID) ArticleID UserTagText UserID
type UserTagR = UserTagT UserTagID         ArticleID UserTagText UserID
makeTypeInstanceFWR "UserTag"

userTagTable :: Table (F UserTagW) (F UserTagR)
userTagTable = table "tag_table" $ pUserTag UserTag
  { userTagID    = pUserTagID $ UserTagID (tableField "user_tag_id")
  , tagArticleID = pArticleID $ ArticleID (tableField "tag_article_id")
  , userTag      = tableField "user_tag"
  , addUserID    = pUserID    $ UserID    (tableField "tag_id")
  }
