{-# LANGUAGE TemplateHaskell #-}

module Model.User.Table where

import           Data.ByteString            (ByteString)
import           Data.Int                   (Int64)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Text                  (Text)
import           Model.F                    (F)
import           Model.TH                   (genNewtypeT, makeTypeInstanceFWR)
import           Opaleye

type Username = Text

genNewtypeT "Email" ''Text ''SqlText
makeAdaptorAndInstance "pEmail" ''EmailT

genNewtypeT "Password" ''ByteString ''SqlBytea
makeAdaptorAndInstance "pPassword" ''PasswordT


genNewtypeT "UserID" ''Int64 ''SqlInt8
makeAdaptorAndInstance "pUserID" ''UserIDT

data UserT a b c d
  = User              -- ^ @TABLE user_table@
    { userID       :: a -- ^ @user_id bigserial PRIMARY KEY@
    , userEmail    :: b -- ^ @user_email text NOT NULL UNIQUE@
    , userUsername :: c -- ^ @user_username text NOT NULL@
    , userPasswd   :: d -- ^ @user_passwd bytea NOT NULL@
    }
  deriving (Show, Eq)

makeAdaptorAndInstance "pUser" ''UserT

type UserW = UserT (Maybe UserID) Email Username Password
type UserR = UserT UserID         Email Username Password
makeTypeInstanceFWR "User"

userTable :: Table (F UserW) (F UserR)
userTable = table "user_table" $ pUser User
  { userID       = pUserID   $ UserID   (tableField "user_id")
  , userEmail    = pEmail    $ Email    (tableField "user_email")
  , userUsername = tableField "user_username"
  , userPasswd   = pPassword $ Password (tableField "user_passwd")
  }
