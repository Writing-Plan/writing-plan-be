{-# LANGUAGE TemplateHaskell #-}

module Model.Post.Type where

import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Model.Post.Table
import           Model.Tag.Table
import           Model.User.Table
import           Model.UserTag.Table

data PostBriefT a b c d e
  = PostBrief
    { postBriefId      :: a
    , postBriefAuthor  :: b
    , postBriefTitle   :: c
    , postBriefTag     :: d
    , postBriefUserTag :: e
    }

makeAdaptorAndInstance "pPostBrief" ''PostBriefT
deriveJSON (aesonDrop (length ("postBrief" :: String)) snakeCase) ''PostBriefT

type PostBrief = PostBriefT PostID Username Title [TagText] [UserTagText]

data PostFullT a b c d e
  = PostFull
    { postFullAuthor  :: a
    , postFullTitle   :: b
    , postFullContent :: c
    , postFullTag     :: d
    , postFullUserTag :: e
    }

makeAdaptorAndInstance "pPostFull" ''PostFullT
deriveJSON (aesonDrop (length ("postFull" :: String)) snakeCase) ''PostFullT

type PostFull = PostFullT Username Title Content [TagText] [UserTagText]
