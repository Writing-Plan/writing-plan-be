{-# LANGUAGE TemplateHaskell #-}

module Model.Comment.Type where

import           Data.Aeson.TH
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Text                  (Text)
import           Data.Time
import           Model.Comment.Table
import           Model.User.Table

data CommentFullT a b c d
  = CommentFull
    { commentFullId       :: a
    , commentFullUsername :: b
    , commentFullContent  :: c
    , commentFullCreated  :: d
    }

makeAdaptorAndInstance "pCommentFull" ''CommentFullT
deriveJSON defaultOptions ''CommentFullT

type CommentFull = CommentFullT CommentID Username Text UTCTime
