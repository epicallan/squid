{-# LANGUAGE TypeFamilyDependencies #-}
module Squid.Client.RunClient where

import Squid.Prelude

import Squid.DataBase

-- | Execute represents commands such as Insert
-- while Query represents commands such as select.
-- having a PersistType helps know the kind of result types we want to return
--
data PersistType = Execute | ExecuteVoid | Query

-- | DB clients such as postgres | mysql should provide an implementation
-- for this class
--
class Monad m => RunClient sqlEntity (persistType :: PersistType) m where
  type SqlResults sqlEntity persistType = (result :: Type) | result -> persistType

  runClient
    :: Sql sqlEntity ()
    -> m (SqlResults sqlEntity persistType)
