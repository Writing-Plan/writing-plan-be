module Model.F (F) where

import           Data.Text      (Text)
import           Model.F.Family (F)
import           Opaleye

type instance F Bool = Column SqlBool
type instance F Text = Column SqlText
