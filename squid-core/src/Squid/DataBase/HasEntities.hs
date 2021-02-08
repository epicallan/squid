{-# LANGUAGE DefaultSignatures #-}

module Squid.DataBase.HasEntities where

import Squid.Prelude

import Squid.DataBase.HasEntity
import Squid.DataBase.Table

import qualified Data.List.NonEmpty as NE

class HasEntities operation where
  getEntities :: NonEmpty TableDefinition

-- | Treatment of a single SQL table statement operation
--
instance {-# Overlapping #-} HasEntity a
  => HasEntities a where
  getEntities = mkTableDefinition @a NE.:| []

-- Treatment of an Implicit Join SQL statement operation
instance {-# Overlapping #-} (HasEntity a, HasEntity b)
  => HasEntities (a, b) where
  getEntities = mkTableDefinition @a NE.:| [mkTableDefinition @b]

-- | Gives us an interpretation of tables in an SQL statement
--
type family OperationEntities (a :: Type) :: Type where
  OperationEntities (a, b) = (Table (TableFields a) , Table (TableFields b))
  OperationEntities a = Table (TableFields a)
