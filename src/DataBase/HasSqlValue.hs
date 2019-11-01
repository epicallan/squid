module DataBase.HasSqlValue where

import Data.Kind

class HasSqlValue a where
  type SqlValue a :: Type
  toSqlValue :: a -> SqlValue a

instance HasSqlValue String where
  type SqlValue String = String

  toSqlValue = id

instance HasSqlValue Int where
  type SqlValue Int  = String

  toSqlValue = show
