{-# LANGUAGE DefaultSignatures #-}
module Squid.DataBase.HasFieldValues where

import Data.ByteString
import Data.ByteString.Builder (Builder, intDec)
import Data.String.Conversions (cs)
import Squid.Prelude

import GHC.Generics

type (a :: k) := (b :: k1) = '(a, b)

data SqlValue =
    Plain Builder
  | Escape ByteString
  | Null

instance Eq SqlValue where
  _ == _ = False

data FieldValue where
  FieldValue :: forall a . HasSqlValue a => a -> FieldValue

-- | Only works for records
-- Note: that we don't need to submit a primary key, because by default
-- its set to auto-increment during migration. In cases where its not
-- and a user provides their own key, then we still have to do nothing
-- it will be submitted with the record.
--
-- This class is currently used for SQL insert values.
-- it wraps a column/field values of a provided Record into
-- an GADT with HasSqlValue context
--
class HasFieldValues a where
  fieldValues :: a -> [FieldValue]
  default fieldValues :: (Generic a, GHasFieldValues (Rep a)) => a -> [FieldValue]
  fieldValues x = gFieldValues (from x)

class GHasFieldValues (f :: Type -> Type) where
  gFieldValues :: f x -> [FieldValue]

instance (HasSqlValue a) => GHasFieldValues (K1 _i a) where
  gFieldValues (K1 a) = [FieldValue a]

instance (GHasFieldValues a, GHasFieldValues b) => GHasFieldValues (a :*: b) where
  gFieldValues (a :*: b) = gFieldValues a <> gFieldValues b

instance (GHasFieldValues a) => GHasFieldValues (M1 _x _y a) where
  gFieldValues (M1 a) = gFieldValues a

-- | inspired by ToField from postgresql-simple
--
class HasSqlValue a where
  toSqlValue :: a -> SqlValue

instance HasSqlValue String where
  toSqlValue  = Escape . cs

instance HasSqlValue Text where
  toSqlValue  = Escape . cs

instance HasSqlValue Int where
  toSqlValue  = Plain . intDec

instance HasSqlValue a => HasSqlValue (Maybe a) where
  toSqlValue = \case
    Just a -> toSqlValue a
    _      -> Null
