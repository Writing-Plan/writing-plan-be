{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Model.Type where

import           Data.ByteString            (ByteString)
import           Data.Int                   (Int64)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Text                  (Text)
import           Model.TH                   (genNewtypeT)
import           Opaleye

type Username = Text
type TextF = Field SqlText

genNewtypeT "Email" ''Text ''SqlText
makeAdaptorAndInstance "pEmail" ''EmailT

genNewtypeT "Password" ''ByteString ''SqlBytea
makeAdaptorAndInstance "pPassword" ''PasswordT


genNewtypeT "UserID" ''Int64 ''SqlInt8
makeAdaptorAndInstance "pUserID" ''UserIDT

data UserT a b c d
  = User
    { userID :: a
    , email  :: b
    , name   :: c
    , passwd :: d
    }
  deriving (Show, Eq)

type UserW  = UserT (Maybe UserID) Email  Username  Password
type UserR  = UserT UserID         Email  Username  Password
type UserFW = UserT MaybeUserIDF   EmailF TextF     PasswordF
type UserFR = UserT UserIDF        EmailF TextF     PasswordF

makeAdaptorAndInstance "pUser" ''UserT

userTable :: Table UserFW UserFR
userTable = table "user_table" $ pUser User
  { userID = pUserID   $ UserID   (tableField "userid")
  , name   = tableField "username"
  , email  = pEmail    $ Email    (tableField "email")
  , passwd = pPassword $ Password (tableField "passwd")
  }
