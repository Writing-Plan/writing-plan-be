module Model.F (F) where

import           Data.Int       (Int64)
import           Data.Text      (Text)
import           Data.Time
import           Model.F.Family (F)
import           Opaleye

type instance F Bool  = Column SqlBool
type instance F Text  = Column SqlText
type instance F Int   = Column SqlInt4
type instance F Int64 = Column SqlInt8
type instance F ZonedTime = Column SqlTimestamptz
type instance F UTCTime = Column SqlTimestamptz

type instance F (Maybe UTCTime) = Maybe (Column SqlTimestamptz)

-- type instance F (Maybe Int) = Maybe (Column SqlInt4)
-- type instance F [Int] = Column (SqlArray SqlInt4)
