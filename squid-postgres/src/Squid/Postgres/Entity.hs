{-# LANGUAGE StandaloneDeriving #-}
module Squid.Postgres.Entity where

import Data.Kind
import GHC.Generics

import Squid.Client

import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.FromRow   as PG

-- | Returned DB record/resource with primaryKey
data Entity a = Entity
  { entityId  :: PrimaryKey a
  , entityVal :: a
  }

deriving instance (Show a, Show (PrimaryKey a)) => Show (Entity a)

deriving instance (Eq a, Eq (PrimaryKey a)) => Eq (Entity a)

deriving instance (Ord a, Ord (PrimaryKey a)) => Ord (Entity a)

deriving instance Generic (Entity a)

-- | TODO: we may have to increase the number of tuple instances for FromRow class
-- maybe with template haskell or manually.
type family GetDbRowEntities (a :: Type) :: Type where
  GetDbRowEntities (a, b) = (Entity a, Entity b)
  GetDbRowEntities a = Entity a

instance (pid ~ PrimaryKey a, PG.FromRow a, PG.FromField pid)
  => PG.FromRow (Entity a) where
  fromRow = Entity <$> PG.field <*> PG.fromRow

-- | TODO: write more instances
instance {-# Overlapping #-}
  (ida ~ PrimaryKey a, idb ~ PrimaryKey b, PG.FromRow a, PG.FromRow b, PG.FromField ida, PG.FromField idb)
  => PG.FromRow (Entity a, Entity b) where
  fromRow = (,) <$> PG.fromRow <*> PG.fromRow
