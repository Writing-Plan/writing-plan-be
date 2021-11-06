{-# LANGUAGE TemplateHaskell #-}

module Model.Tag.Type where

import           Data.Int                   (Int64)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Text                  (Text)
import           Model.F                    (F)
import           Model.TH                   (genNewtypeT, makeTypeInstanceFWR)
import           Opaleye

type TagText = Text

genNewtypeT "TagID" ''Int64 ''SqlInt8
makeAdaptorAndInstance "pTagID" ''TagIDT

data TagT a b
  = Tag          -- ^ @TABLE tag_table@
    { tagID :: a -- ^ @tag_id bigserial PRIMARY KEY@
    , tag   :: b -- ^ @tag text NOT NULL UNIQUE@
    }

makeAdaptorAndInstance "pTag" ''TagT

type TagW = TagT (Maybe TagID) TagText
type TagR = TagT TagID         TagText
makeTypeInstanceFWR "Tag"

tagTable :: Table (F TagW) (F TagR)
tagTable = table "tag_table" $ pTag Tag
  { tagID = pTagID $ TagID (tableField "tag_id")
  , tag   = tableField "tag"
  }
