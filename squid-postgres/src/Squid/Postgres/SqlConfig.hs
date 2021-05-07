module Squid.Postgres.SqlConfig where

import Control.Retry (RetryPolicy, retryPolicyDefault)
import Database.PostgreSQL.Simple

data SqlLogging = Enabled | Disabled
  deriving stock (Eq)

data SqlConfig = SqlConfig
  { sqlConnection  :: Connection
  , sqlRetryPolicy :: RetryPolicy
  , sqlLogging     :: SqlLogging
  }

defaultConfig :: Connection -> SqlConfig
defaultConfig conn = SqlConfig conn retryPolicyDefault Disabled
