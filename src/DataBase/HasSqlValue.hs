module DataBase.HasSqlValue where

import Data.Kind

class Show a => HasSqlValue a where
  type SqlValue a :: Type
  toSqlValue :: a -> SqlValue a

instance HasSqlValue String where
  type SqlValue String = String

  toSqlValue = id

instance HasSqlValue Int where
  type SqlValue Int  = String

  toSqlValue = show
