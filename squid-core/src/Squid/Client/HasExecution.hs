-- For Sql execution commands such as Insert, delete, update etc
-- currently only insert is implemented/supported
--
module Squid.Client.HasExecution where

import Squid.Client.RunClient
import Squid.DataBase

class HasExecution a where
  insertCmd :: a ->  Sql entity ()

  default insertCmd :: HasFieldValues a => a -> Sql entity ()
  insertCmd entity = insert_ (fieldValues entity)

instance (HasFieldValues a) => HasExecution a

insert
  :: forall a m . (RunClient a 'Execute m, HasExecution a)
  => a
  -> m (SqlResults a 'Execute)
insert = runClient @a @'Execute . insertCmd
