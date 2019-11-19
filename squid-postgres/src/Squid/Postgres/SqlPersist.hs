module Squid.Postgres.SqlPersist where

import Control.Monad.Reader
import Data.Kind
import Data.Proxy
import Data.String (fromString)
import Data.Text (unpack)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow (FromRow (..))

import Squid.Client
import Squid.Postgres.Entity
import Squid.Postgres.SqlConfig
import Squid.Postgres.SqlWriter

newtype SqlPersistT m a
  = SqlPersist { runSqlPersist :: ReaderT SqlConfig m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader SqlConfig)

type SqlPersistIO = SqlPersistT IO

type SqlPersist m a = MonadIO m => SqlPersistT m a

instance
  ( MonadIO m
  , FromRow row
  , pid ~ PrimaryKey row
  , FromField pid
  )
  => RunClient row (SqlPersistT m) where
  runClient proxy sql = do
    config <- ask
    let statement = runSqlWriter proxy sql
    results <- runPostgresQuery (Proxy @pid) proxy config statement
    return $ toEntity <$> results


runDb :: SqlConfig -> SqlPersistIO a -> IO a
runDb config sqlPersist = runReaderT (runSqlPersist sqlPersist) config

type MonadDb m row =
  (  FromField (PrimaryKey row)
  ,  FromRow row
  ,  MonadIO m
  ,  RunClient row m
  )
-- * Helper functions

runPostgresQuery
  :: (MonadIO m, FromRow (pid ::: a))
  => Proxy pid -> Proxy a -> SqlConfig -> SqlStatement -> m [pid ::: a]
runPostgresQuery _ _ config statement = do
  let  (SqlQuery s xs) = sqlQuery statement
       values          = toSqlAction <$> xs
       qstatement      = fromString $ unpack $ unQueryStatement s
  case sqlConnection config of
    Nothing   -> error "Provide connection"
    Just conn -> do
      liftIO $ print qstatement
      liftIO $ query conn qstatement values


toEntity :: forall a (t :: Type). (t ~ PrimaryKey a) => t ::: a-> Entity a
toEntity (pid ::: val) = Entity pid val
