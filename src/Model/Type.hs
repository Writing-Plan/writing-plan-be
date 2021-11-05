{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Model.Type where

import           Data.ByteString            (ByteString)
import           Data.Int                   (Int64)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Text                  (Text)
import           Model.FamilyF              (F)
import           Model.TH                   (genNewtypeT, makeTypeInstanceFWR)
import           Opaleye

type Username = Text

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


genNewtypeT "BloggerID" ''Int64 ''SqlInt8
makeAdaptorAndInstance "pBloggerID" ''BloggerIDT

data BloggerT a b c d
  = Blogger              -- ^ @TABLE blogger_table@
    { bloggerID     :: a -- ^ @blogger_id serial PRIMARY KEY@
    , bloggerUserID :: b -- ^ @blogger_user_id int REFERENCES user_table(user_id)@
    , blogUrl       :: c -- ^ @blogger_url text NOT NULL UNIQUE@
    , allowComments :: d -- ^ @allow_comments boolean NOT NULL@
    }

makeAdaptorAndInstance "pBlogger" ''BloggerT

type BloggerW = BloggerT (Maybe BloggerID) UserID Text Bool
type BloggerR = BloggerT BloggerID         UserID Text Bool
makeTypeInstanceFWR "Blogger"

bloggerTable :: Table (F BloggerW) (F BloggerR)
bloggerTable = table "blogger_table" $ pBlogger Blogger
  { bloggerID     = pBloggerID $ BloggerID (tableField "blogger_id")
  , bloggerUserID = pUserID    $ UserID    (tableField "blogger_user_id")
  , blogUrl       = tableField "blog_url"
  , allowComments = tableField "allow_comments"
  }
