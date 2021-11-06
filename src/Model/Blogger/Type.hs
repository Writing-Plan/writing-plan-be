{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Model.Blogger.Type where

import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Text                  (Text)
import           Model.F                    (F)
import           Model.TH                   (makeTypeInstanceF)
import           Model.User.Type
import           Opaleye

type Url = Text

type BloggerID = UserID

data BloggerT a b c
  = Blogger              -- ^ @TABLE blogger_table@
    { bloggerID     :: a -- ^ @blogger_id bigint PRIMARY KEY REFERENCES user_table(user_id)@
    , blogUrl       :: b -- ^ @blogger_url text NOT NULL UNIQUE@
    , allowComments :: c -- ^ @allow_comments boolean NOT NULL@
    }

makeAdaptorAndInstance "pBlogger" ''BloggerT

type Blogger_ = BloggerT BloggerID Url Bool
makeTypeInstanceF ''Blogger_

bloggerTable :: Table (F Blogger_) (F Blogger_)
bloggerTable = table "blogger_table" $ pBlogger Blogger
  { bloggerID     = pUserID    $ UserID    (tableField "blogger_user_id")
  , blogUrl       = tableField "blog_url"
  , allowComments = tableField "allow_comments"
  }
