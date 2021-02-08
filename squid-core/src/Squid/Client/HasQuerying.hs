-- | A class containing query methods entity client should be able to implement.
-- Default implementation is provided but entity client can offer different implementation.
--
module Squid.Client.HasQuerying where

import Squid.Client.RunClient
import Squid.DataBase
import Data.Kind

-- | The HasQuerying  represents default implementations
-- of querying capabilities by an SQL client under different
-- interpretations of SQL operations such as simple select & implicit joins)
--
-- | 'entity' represents a table or tables under an sql select operation
--  'a' represents interpretation of the table as an operation
--
class HasQuerying (entity :: Type) a where
  -- 'a' an attempt to retrieving 'a' from entity over
  -- type-family does n't work well since the type-family
  -- instances would n't be injective. The fact that
  -- we have very general overlapping class instances
  -- also complicates matters.
  selectCmd :: (a -> Sql entity ()) -> Sql entity ()

instance {-# Overlapping #-} HasTable ts
  => HasQuerying entity (Table ts) where
  selectCmd
    :: (Table ts -> Sql entity ())
    -> Sql entity ()
  selectCmd f = do
    select_
    f (mkTable @ts)

-- | SQL Implicit join
instance {-# Overlapping #-} (HasTable ts, HasTable xs)
  => HasQuerying (a, b) (Table ts, Table xs) where

  selectCmd
    :: ((Table ts, Table xs) -> Sql entity ())
    -> Sql entity ()
  selectCmd f = do
    select_
    f (mkTable @ts, mkTable @xs)

select
  :: forall a m . (RunClient a 'Query m, HasQuerying a (OperationEntities a))
  => (OperationEntities a -> Sql a ())
  -> m (SqlResults a 'Query)
select f = runClient @a @'Query (selectCmd f)

from
  :: (b -> Sql a ())
  -> b
  -> Sql a ()
from f next = from_ >> f next
