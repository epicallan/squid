{-# LANGUAGE TypeApplications #-}
module Squid.Client.Operators where

import Data.Proxy
import Data.String (fromString)
import GHC.TypeLits

import Squid.DataBase

createRelation
  :: forall field a. (KnownSymbol field, HasSqlField a)
  => RelationOp -> Column '( field, a) -> a -> RelationAction
createRelation op  _ x = RelationAction op fieldName (FieldValue x)
  where
    fieldName = fromString $ symbolVal (Proxy @field)

(==.)
  :: forall field a. (KnownSymbol field, HasSqlField a)
  => Column '( field, a) -> a -> RelationAction
(==.) = createRelation QEq

(>.)
  :: forall field a. (KnownSymbol field, HasSqlField a)
  => Column '( field, a) -> a -> RelationAction
(>.) = createRelation QGT


(<.)
  :: forall field a. (KnownSymbol field, HasSqlField a)
  => Column '( field, a) -> a -> RelationAction
(<.) = createRelation QLT
