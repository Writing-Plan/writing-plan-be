{-# LANGUAGE TemplateHaskell #-}

module Model.Article.Type where

import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Text                  (Text)
import           Model.F                    (F)
import           Model.TH                   (genNewtypeT, makeTypeInstanceFWR)
import           Model.User.Type
import           Opaleye
import Data.Int (Int64)

type Title = Text
type Content = Text

genNewtypeT "ArticleID" ''Int64 ''SqlInt8
makeAdaptorAndInstance "pArticleID" ''ArticleIDT

data ArticleT a b c d
  = Article          -- ^ @TABLE article_table@
    { articleID :: a -- ^ @article_id bigserial PRIMARY KEY@
    , authorID  :: b -- ^ @author_id bigint REFERENCES blogger_table(blogger_id)@
    , title     :: c -- ^ @title text NOT NULL@
    , content   :: d -- ^ @content text NOT NULL@
    -- TODO: rating of the article
    }

makeAdaptorAndInstance "pArticle" ''ArticleT

type ArticleW = ArticleT (Maybe ArticleID) UserID Title Content
type ArticleR = ArticleT ArticleID         UserID Title Content
makeTypeInstanceFWR "Article"

articleTable :: Table (F ArticleW) (F ArticleR)
articleTable = table "article_table" $ pArticle Article
  { articleID = pArticleID $ ArticleID (tableField "article_id")
  , authorID  = pUserID    $ UserID    (tableField "author_id")
  , title     = tableField "title"
  , content   = tableField "content"
  }
