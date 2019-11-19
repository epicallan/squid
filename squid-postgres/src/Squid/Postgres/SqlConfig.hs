module Squid.Postgres.SqlConfig where

import Control.Retry (RetryPolicy, retryPolicyDefault)
import Database.PostgreSQL.Simple

data SqlConfig = SqlConfig
  { sqlConnection  :: Maybe Connection
  , sqlRetryPolicy :: RetryPolicy
  -- logging options
  }

defaultConfig :: SqlConfig
defaultConfig = SqlConfig Nothing retryPolicyDefault
