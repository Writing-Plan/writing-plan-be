{-# LANGUAGE TemplateHaskell #-}

module Model.ArticleHasTag.Type where

import           Data.Int                   (Int64)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Model.Article.Type
import           Model.F                    (F)
import           Model.TH                   (genNewtypeT, makeTypeInstanceFWR)
import           Model.Tag.Type
import           Model.User.Type
import           Opaleye

genNewtypeT "ArticleHasTagID" ''Int64 ''SqlInt8
makeAdaptorAndInstance "pArticleHasTagID" ''ArticleHasTagIDT

data ArticleHasTagT a b c d e
  = ArticleHasTag          -- ^ @TABLE article_has_tag_table@
    { articleHasTagID :: a -- ^ @article_has_tag_id bigserial PRIMARY KEY@
    , hasArticleID    :: b -- ^ @has_article_id bigint REFERENCES article_table(article_id)@
    , hasTagID        :: c -- ^ @has_tag_id bigint REFERENCES tag_table(tag_id)@
    , hasUserID       :: d -- ^ @has_user_id bigint REFERENCES user_table(user_id)@
    , reportCount     :: e -- ^ @report_count int NOT NULL@
    }

makeAdaptorAndInstance "pArticleHasTag" ''ArticleHasTagT

type ArticleHasTagW = ArticleHasTagT (Maybe ArticleHasTagID) ArticleID TagID UserID Int
type ArticleHasTagR = ArticleHasTagT ArticleHasTagID         ArticleID TagID UserID Int
makeTypeInstanceFWR "ArticleHasTag"

articleHasTagTable :: Table (F ArticleHasTagW) (F ArticleHasTagR)
articleHasTagTable = table "article_table" $ pArticleHasTag ArticleHasTag
  { articleHasTagID = pArticleHasTagID $ ArticleHasTagID (tableField "article_has_tag_id")
  , hasArticleID    = pArticleID       $ ArticleID       (tableField "article_id")
  , hasTagID        = pTagID           $ TagID           (tableField "tag_id")
  , hasUserID       = pUserID          $ UserID          (tableField "adder_id")
  , reportCount     = tableField "content"
  }
