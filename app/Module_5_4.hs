module Module_5_4 where

import Prelude hiding (Maybe, Just, Nothing)

-- Maybe и [] - это тоже Монады
data Maybe a = Nothing | Just a
  deriving (Eq, Ord)