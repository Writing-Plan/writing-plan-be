module Model.TH (module Export) where

import           Control.Comonad
import           Data.Profunctor
import           Data.Profunctor.Product.Default
import           Model.TH.Internal               as Export
import           Opaleye

-- If these instances below ever overlap, define a new typeclass for newtypes where
-- @
-- class NewType a b where
--   cons :: b -> a   -- replace 'pure'
--   unCons :: a -> b -- replace 'extract'
-- @
-- to replcace the 'Applicative' and 'Comonad' constraints.

-- Default ToFields (Maybe a) (F (Maybe a))
instance (Applicative m, Default ToFields a b) => Default ToFields (Maybe (m a)) (m (Maybe b)) where
  def = toToFields (maybe (pure Nothing) (fmap (Just . toFields)))

-- Default ToFields [a] (F [a])
instance (Applicative m, Comonad m, Default ToFields [a] b) => Default ToFields [m a] (m b) where
  def = dimap (map extract) pure def

-- Default FromFields (F [a]) [a]
instance (Applicative m, Comonad m, Default FromFields b [a]) => Default FromFields (m b) [m a] where
  def = dimap extract (map pure) def

-- Default ToFields (Maybe [a]) (F (Maybe [a]))
instance (Applicative m, Comonad m, Default ToFields [a] b) => Default ToFields (Maybe [m a]) (m (Maybe b)) where
  def = toToFields (pure . fmap (toFields . map extract))
