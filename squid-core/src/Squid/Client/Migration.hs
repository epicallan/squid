module Squid.Client.Migration where

import Data.Kind ( Type )

import Squid.Client.RunClient
  ( RunClient(..), PersistType(ExecuteVoid) )
import Squid.DataBase
    ( RawStatement, HasEntity(..), TableDefinition, raw_)

class HasMigration (ts :: [Type]) where
  migration :: [TableDefinition]

instance HasMigration '[] where
  migration = []

instance (HasEntity a, HasMigration ts)
  => HasMigration (a ': ts) where
   migration = mkTableDefinition @a : migration @ts

migrateAll_
  :: forall (ts :: [Type]) m. (RunClient ts 'ExecuteVoid m)
  => RawStatement
  -> m (SqlResults ts 'ExecuteVoid)
migrateAll_ stmt = runClient @ts @'ExecuteVoid $ raw_ stmt
