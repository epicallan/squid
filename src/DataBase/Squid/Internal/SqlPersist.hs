{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module DataBase.Squid.Internal.SqlPersist where

import Control.Monad.Reader
import Data.Functor.Identity
import Data.Proxy

import DataBase.Internal

-- | A function that embeds Sql a r into SqlPersistT such that we deal with the results of Sql a r
-- i.e we get SqlPersist m User
-- so this select is sort of auto run / derived and

data DataBaseType =
    Mock
  | Postgres
  | SQLite
  | Mysql

newtype SqlPersist (d :: DataBaseType) m a
  = SqlPersist { runDb' :: ReaderT SqlConfig m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader SqlConfig)


type SqlMock = SqlPersist 'Mock
type SqlMockPure  = SqlMock Identity
type SqlMockIO = SqlMock IO

class Monad m => MonadSql m a where
  query :: Sql a r -> m (SqlResults a)

instance
  ( HasEntity a
  , Monad m
  )
  => RunSql (SqlMock m) a where
  runSql _ _ _ = return []

instance (HasEntity a, Monad m) => MonadSql (SqlMock m) a where
  query x = do
    let sqlStatement = runSqlWriterPure x
    config <- ask
    runSql (Proxy @a) config sqlStatement

runDb :: SqlConfig -> SqlPersist d m a -> m a
runDb = undefined

runMockDbIO :: SqlConfig -> SqlMockIO a -> IO a
runMockDbIO = undefined

runMockPure :: SqlConfig -> SqlMockPure a -> Identity a
runMockPure = undefined

defaultMockConfig :: SqlConfig
defaultMockConfig = undefined
