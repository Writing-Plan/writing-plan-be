{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Model.Effects.Blogger
  ( Blogger, initBloggerTable, addBlogger, setAllowComments
  , BloggerC, runBlogger
  ) where

import           Control.Algebra
import           Control.Monad.IO.Class (MonadIO)
import           Data.Functor           (($>))
import           Data.Kind              (Type)
import           Model.Effects.PG
import           Model.Effects.TH       (sendAll)
import           Model.Type
import           Opaleye

data Blogger (m :: Type -> Type) k where
  InitBloggerTable :: Blogger m Bool
  AddBlogger       :: UserID -> Url -> Blogger m Bool
  SetAllowComments :: UserID -> Bool -> Blogger m Bool

sendAll ''Blogger

newtype BloggerC m a = BloggerC { runBloggerC :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

runBlogger :: BloggerC m a -> m a
runBlogger = runBloggerC

instance Has PG sig m => Algebra (Blogger :+: sig) (BloggerC m) where
  alg hdl sig ctx = case sig of
    R other       -> BloggerC (alg (runBloggerC . hdl) other ctx)
    L user -> (ctx $>) <$> case user of
      InitBloggerTable -> initTable "blogger_table"
        "CREATE TABLE user_table ( \
        \  blogger_id      int     PRIMARY KEY UNIQUE REFERENCES user_table(user_id), \
        \  blogger_url     text    NOT NULL UNIQUE, \
        \  allow_comments  boolean NOT NULL \
        \);"
      AddBlogger bloggerID blogUrl -> fmap (==1) . insert $ Insert
        { iTable      = bloggerTable
        , iRows       = [toFields @Blogger_ $ Blogger {allowComments = True, ..}]
        , iReturning  = rCount
        , iOnConflict = Just DoNothing
        }
      SetAllowComments bloggerID' allowance -> fmap (==1) . update $ Update
        { uTable      = bloggerTable
        , uUpdateWith = \Blogger{..} -> Blogger{allowComments = toFields allowance, ..}
        , uWhere      = \Blogger{..} -> bloggerID .=== toFields bloggerID'
        , uReturning  = rCount
        }