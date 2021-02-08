module Squid.Client.Operators where

import Squid.Prelude

import Squid.DataBase
    ( HasSqlValue, FieldValue(FieldValue), TableField, RelationAction(..)
    , RelationOp(..)
    )

type HasOperators field a = (KnownSymbol field, HasSqlValue a)

createRelation
  :: forall field a . HasOperators field a
  => RelationOp
  -> TableField '(field, a)
  -> a
  -> RelationAction
createRelation op  _ x = RelationAction op fieldName (FieldValue x)
  where
    fieldName = symbolText @field

(==.) :: HasOperators field a
      => TableField '(field, a) -> a -> RelationAction
(==.) = createRelation QEq

(>.) :: HasOperators field a
     => TableField '(field, a) -> a -> RelationAction
(>.) = createRelation QGT

(<.) :: HasOperators field a
     => TableField '(field, a) -> a -> RelationAction
(<.) = createRelation QLT
