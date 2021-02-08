module Squid.Postgres.SqlPersist where

import Control.Monad.Reader
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy (Proxy(..))
import Data.String (fromString)

import Squid.Client
import Squid.Postgres.Entity
import Squid.Postgres.Field
import Squid.Postgres.SqlConfig

import qualified Data.Text as Text
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.ToField as PG

newtype SqlPersistT m a
  = SqlPersist { runSqlPersist :: ReaderT SqlConfig m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader SqlConfig)

type SqlPersistIO = SqlPersistT IO

type SqlPersist m a = MonadIO m => SqlPersistT m a

instance
  ( MonadIO m
  , HasEntities a
  , b ~ GetDbRowEntities a
  , PG.FromRow b
  )
  => RunClient a 'Query (SqlPersistT m) where
  type SqlResults a 'Query = [GetDbRowEntities a]

  runClient sqlCommand = do
    config <- ask
    let statement = getStatement sqlCommand
    postgresQuery @b (getEntities @a) config statement

instance
  ( MonadIO m
  , HasEntities a
  )
  => RunClient a 'Execute (SqlPersistT m) where
  type SqlResults a 'Execute = Int

  runClient sqlCommand = do
    config <- ask
    let statement = getStatement sqlCommand
    postgresExecute (getEntities @a) config statement

instance ( MonadIO m)
  => RunClient a 'ExecuteVoid (SqlPersistT m) where
  type SqlResults a 'ExecuteVoid = ()

  runClient sql = do
    config <- ask
    let statement = getRawStatement (Proxy @'RawQuery) sql
    postgresExecute_ config statement

-- | TODO: Should wrap everything in a transaction
runDb :: SqlConfig -> SqlPersistIO a -> IO a
runDb config sqlPersist = runReaderT (runSqlPersist sqlPersist) config

postgresQuery
  :: forall a m . (MonadIO m, PG.FromRow a)
  => NonEmpty TableDefinition
  -> SqlConfig
  -> SqlStatement
  -> m [a]
postgresQuery tables config statement = do
  let  (pgStatement, values) = buildQuery tables statement
  case sqlConnection config of -- TODO: get connection before hand so that we dont have a maybe
    Nothing   -> error "Provide connection" -- TODO: throw Error
    Just conn -> do
      liftIO $ print pgStatement
      liftIO $ PG.query conn pgStatement values

-- returns affected rows
postgresExecute
  :: forall m . (MonadIO m)
  => NonEmpty TableDefinition
  -> SqlConfig
  -> SqlStatement
  -> m Int
postgresExecute tables config statement = do
  let  (pgStatement, values) = buildQuery tables statement
  case sqlConnection config of -- TODO: get connection before hand so that we don't have a maybe
    Nothing   -> error "Provide connection" -- TODO: throw Error
    Just conn -> do
      liftIO $ print pgStatement
      rowCount <- liftIO $ PG.execute conn pgStatement values
      return $ fromIntegral @Int64 @Int rowCount

postgresExecute_
  :: MonadIO m
  => SqlConfig
  -> SqlStatement
  -> m ()
postgresExecute_  config statement = do
  let  pgStatement = buildQuery_  (Proxy @'RawQuery) statement
  case sqlConnection config of
    Just conn -> do
      liftIO $ print pgStatement
      liftIO $ void $ PG.execute_ conn pgStatement
    _         -> error "Provide connection"

buildQuery
  :: NonEmpty TableDefinition
  -> SqlStatement
  -> (PG.Query, [PG.Action])
buildQuery tables statement = (pgStatement, pgActions)
  where
    (TypedSqlQuery (TypedSqlStatement s) xs) = sqlWriter tables statement

    pgActions   = toSqlAction <$> xs
    pgStatement = fromString $ Text.unpack s

buildQuery_ :: Proxy 'RawQuery -> SqlStatement -> PG.Query
buildQuery_ proxy statement = fromString $ Text.unpack s
  where
    (TypedSqlQuery (TypedSqlStatement s) _ ) = rawWriter proxy statement
