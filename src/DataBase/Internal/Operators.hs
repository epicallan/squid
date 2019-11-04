{-# LANGUAGE TypeApplications #-}
module DataBase.Internal.Operators where

import Data.Proxy
import GHC.TypeLits

import DataBase.Internal.HasSqlValue
import DataBase.Internal.Sql
import DataBase.Internal.Table

infixr 4 ==.

(==.)
  :: forall field a. (HasSqlValue a, KnownSymbol field)
  => Column '( field, a) -> a -> Relation
(==.) _ x = MkRelation QEq fieldName x
  where
    fieldName = symbolVal (Proxy @field)
