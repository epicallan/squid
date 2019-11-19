module Squid.Postgres
  ( module Squid.Postgres.SqlPersist
  , module Squid.Client
  , module Squid.Postgres.SqlConfig
  , FromRow
  , connectPostgreSQL
  ) where

import Database.PostgreSQL.Simple (FromRow, connectPostgreSQL)
import Squid.Client
import Squid.Postgres.SqlConfig
import Squid.Postgres.SqlPersist
