{-# LANGUAGE StandaloneDeriving #-}
module DataBase.Internal.PersistEntity where

import DataBase.Internal.HasEntity
import GHC.Generics

data Entity a = Entity
  { entityVal :: a
  , entityId  :: PrimaryKey a
  }

-- | maybe derive Typeable
deriving instance (Show a, Show (PrimaryKey a)) => Show (Entity a)

deriving instance (Eq a, Eq (PrimaryKey a)) => Eq (Entity a)

deriving instance (Ord a, Ord (PrimaryKey a)) => Ord (Entity a)

deriving instance Generic (Entity a)
