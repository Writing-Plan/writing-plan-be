module Model.F (F) where

import           Data.Int       (Int64)
import           Data.Text      (Text)
import           Model.F.Family (F)
import           Opaleye

type instance F Bool  = Column SqlBool
type instance F Text  = Column SqlText
type instance F Int   = Column SqlInt4
type instance F Int64 = Column SqlInt8
