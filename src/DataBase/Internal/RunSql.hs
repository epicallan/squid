{-# LANGUAGE UndecidableSuperClasses #-}
module DataBase.Internal.RunSql where

import Data.Proxy

import DataBase.Internal.HasEntity
import DataBase.Internal.PersistEntity
import DataBase.Internal.Sql


type Connection = Int

data SqlConfig = SqlConfig
  { connectionPool :: Connection
  , shouldLog      :: Bool
  -- time-out etc
  }

type SqlResults a = [Entity a]

-- m can be some database monad
class (Monad m, HasEntity a) => RunSql m a where
  runSql :: Proxy a -> SqlConfig -> SqlStatement -> m (SqlResults a)
