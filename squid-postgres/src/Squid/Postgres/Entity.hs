module Squid.Postgres.Entity where

import Data.Kind
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField (Action, ToField (..))
import qualified Database.PostgreSQL.Simple.ToField as A

import Squid.Client

data (p :: Type) ::: (a :: Type) = p ::: a

instance (FromField t, FromRow a)
  => FromRow ( t ::: a) where
  fromRow = (:::) <$> field <*> fromRow

newtype SqlFieldWrapper = SqlFieldWrapper SqlField

instance ToField SqlFieldWrapper where
  toField (SqlFieldWrapper x) = case x of
    Plain b  -> A.Plain b
    Escape b -> A.Escape b

toSqlAction :: FieldValue -> Action
toSqlAction = \case
  FieldValue x -> toField . SqlFieldWrapper $ toSqlField x


-- | Migration
{-
 runMigrate :: All HasEntity ts => Proxy ts -> Proxy (TableFields ts)
 run 
-}