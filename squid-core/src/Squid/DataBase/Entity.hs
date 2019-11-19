{-# LANGUAGE StandaloneDeriving #-}
module Squid.DataBase.Entity where

import GHC.Generics
import Squid.DataBase.HasEntity

data Entity a = Entity
  { entityId  :: PrimaryKey a
  , entityVal :: a
  }

deriving instance (Show a, Show (PrimaryKey a)) => Show (Entity a)

deriving instance (Eq a, Eq (PrimaryKey a)) => Eq (Entity a)

deriving instance (Ord a, Ord (PrimaryKey a)) => Ord (Entity a)

deriving instance Generic (Entity a)
