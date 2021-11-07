{-# LANGUAGE TemplateHaskell #-}

module Model.Tag.Type where

import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Text                  (Text)
import           Model.F                    (F)
import           Model.TH
import           Opaleye

-- type TagText = Text

genNewtypeT "TagText" ''Text ''SqlText
makeAdaptorAndInstance "pTagText" ''TagTextT

newtype TagT a
  = Tag            -- ^ @TABLE tag_table@
    { tagText :: a -- ^ @tagText text PRIMARY KEY@
    }

makeAdaptorAndInstance "pTag" ''TagT

type Tag_ = TagT TagText
makeTypeInstanceF ''Tag_

tagTable :: Table (F Tag_) (F Tag_)
tagTable = table "tag_table" $ pTag Tag
  { tagText = pTagText $ TagText (tableField "tagText")
  }
