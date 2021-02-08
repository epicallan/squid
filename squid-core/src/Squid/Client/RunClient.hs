{-# LANGUAGE TypeFamilyDependencies #-}
module Squid.Client.RunClient where

import Squid.Prelude

import Squid.DataBase

-- | Execute represents commands such as Insert
-- while Query represents select.
--
data PersistType = Execute | ExecuteVoid | Query

class Monad m => RunClient sqlEntity (persistType :: PersistType) m where
  type SqlResults sqlEntity persistType = (result :: Type) | result -> persistType
  -- ^^ This seems to imply we can only have one result type for
  -- each PersistType  ??

  runClient
    :: Sql sqlEntity ()
    -> m (SqlResults sqlEntity persistType)
