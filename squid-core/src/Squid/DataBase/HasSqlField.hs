module Squid.DataBase.HasSqlField where

import Data.ByteString
import Data.ByteString.Builder (Builder, intDec)
import Data.String.Conversions (cs)

data SqlField =
    Plain Builder
  | Escape ByteString

-- | inspired by ToField from postgresql-simple
class HasSqlField a where
  toSqlField :: a -> SqlField

instance HasSqlField String where
  toSqlField  = Escape . cs

instance HasSqlField Int  where
  toSqlField  = Plain . intDec
