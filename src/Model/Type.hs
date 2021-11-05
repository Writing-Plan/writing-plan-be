{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Model.Type where

import           Data.ByteString            (ByteString)
import           Data.Int                   (Int64)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Text                  (Text)
import           Model.FamilyF              (F)
import           Model.TH
import           Opaleye

type Username = Text
type Url = Text

type instance F Bool = Column SqlBool
type instance F Text = Column SqlText

genNewtypeT "Email" ''Text ''SqlText
makeAdaptorAndInstance "pEmail" ''EmailT

genNewtypeT "Password" ''ByteString ''SqlBytea
makeAdaptorAndInstance "pPassword" ''PasswordT


genNewtypeT "UserID" ''Int64 ''SqlInt8
makeAdaptorAndInstance "pUserID" ''UserIDT

data UserT a b c d
  = User          -- ^ @TABLE user_table@
    { userID :: a -- ^ @user_id serial PRIMARY KEY@
    , email  :: b -- ^ @email text NOT NULL UNIQUE@
    , name   :: c -- ^ @username text NOT NULL@
    , passwd :: d -- ^ @passwd bytea NOT NULL@
    }
  deriving (Show, Eq)

makeAdaptorAndInstance "pUser" ''UserT

type UserW = UserT (Maybe UserID) Email Username Password
type UserR = UserT UserID         Email Username Password
makeTypeInstanceFWR "User"

userTable :: Table (F UserW) (F UserR)
userTable = table "user_table" $ pUser User
  { userID = pUserID   $ UserID   (tableField "user_id")
  , name   = tableField "username"
  , email  = pEmail    $ Email    (tableField "email")
  , passwd = pPassword $ Password (tableField "passwd")
  }


data BloggerT a b c
  = Blogger              -- ^ @TABLE blogger_table@
    { bloggerID     :: a -- ^ @blogger_id int PRIMARY KEY REFERENCES user_table(user_id)@
    , blogUrl       :: b -- ^ @blogger_url text NOT NULL UNIQUE@
    , allowComments :: c -- ^ @allow_comments boolean NOT NULL@
    }

makeAdaptorAndInstance "pBlogger" ''BloggerT

type Blogger_ = BloggerT UserID Url Bool
makeTypeInstanceF ''Blogger_

bloggerTable :: Table (F Blogger_) (F Blogger_)
bloggerTable = table "blogger_table" $ pBlogger Blogger
  { bloggerID     = pUserID    $ UserID    (tableField "blogger_user_id")
  , blogUrl       = tableField "blog_url"
  , allowComments = tableField "allow_comments"
  }
