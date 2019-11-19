module Squid.Client.Commands where

import Data.Proxy

import Squid.Client.RunClient (RunClient (..), SqlResults)
import Squid.DataBase

-- * Sql commands

select
  :: forall a m. (HasEntity a, RunClient a m)
  => (Table (TableFields a) -> Sql a ())
  -> m (SqlResults a)
select f = runClient (Proxy @a) $ do
  select_
  f entityTable
  where
    entityTable :: Table (TableFields a)
    entityTable = table (Proxy @a)

from
  :: forall a. (Table (TableFields a) -> Sql a ())
  -> Table (TableFields a)
  -> Sql a ()
from f x = do
  from_
  f x
