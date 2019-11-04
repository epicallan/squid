module DataBase.Squid.Internal.Sql where

import Data.Proxy

import DataBase.Internal
import DataBase.Squid.Internal.SqlPersist

select :: forall a m . (HasEntity a, MonadSql m a)
  => (Table (TableFields a) -> Sql a ()) -> m (SqlResults a)
select f = query $ do
  select_
  f entityTable
  where
    entityTable :: Table (TableFields a)
    entityTable = table (Proxy @a)


-- | TODO insert, getBy etc
