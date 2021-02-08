{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Squid.DataBase.HasSqlType where

import Data.Int (Int16, Int32, Int64, Int8)
import GHC.Generics

import Squid.Prelude

data SqlType =
    SqlText
  | SqlInt
  | SqlDate
  | SqlTimeStamp
  deriving (Show, Eq)

class HasSqlType (a :: Type) where
  getSqlType :: SqlType

  -- | default instance is for NewTypes inhabited by types with HasSqlType instances
  default getSqlType
    :: forall rep. (Generic a, rep ~ Rep a, GHasSqlType rep)
    => SqlType
  getSqlType = gSqlType (Proxy @rep)

-- | Derive HasSqlType instances for New-types with values
-- that have HasSqlType instances.
class GHasSqlType (f :: Type -> Type) where
  gSqlType :: Proxy f -> SqlType

instance (TypeError ('Text "Unit types are not supported for Sql Field types"))
  => GHasSqlType U1 where
  gSqlType _ = error "Not supported"

instance (TypeError ('Text "Void types are not supported for Sql Field types"))
  => GHasSqlType V1 where
  gSqlType _ = error "Not supported"

instance HasSqlType a => GHasSqlType (K1 _i a) where
  gSqlType _ = getSqlType @a

instance (TypeError ('Text "No default support for deriving Sql Field types for sum types"))
  => GHasSqlType (a :+: b) where
  gSqlType _ = error "Not supported"

instance (TypeError ('Text "No default support for deriving Sql Field types for product types"))
  => GHasSqlType (a :*: b) where
  gSqlType _ = error "Not supported"

instance (GHasSqlType f) => GHasSqlType (D1 ('MetaData _n _m _p 'True) f) where
    gSqlType _ = gSqlType (Proxy @f)

instance (GHasSqlType f) => GHasSqlType (C1 _m f) where
    gSqlType _ = gSqlType (Proxy @f)

instance (GHasSqlType f) => GHasSqlType (S1 _m f) where
    gSqlType _ = gSqlType (Proxy @f)

instance HasSqlType Text where
  getSqlType = SqlText

instance HasSqlType String where
  getSqlType =  SqlText

instance (HasSqlType a) => HasSqlType (Maybe a) where
  getSqlType = getSqlType @a

instance HasSqlType Int where getSqlType     = SqlInt
instance HasSqlType Int16 where getSqlType   = SqlInt
instance HasSqlType Int32 where getSqlType   = SqlInt
instance HasSqlType Int64 where getSqlType   = SqlInt
instance HasSqlType Int8 where getSqlType    = SqlInt
instance HasSqlType Natural where getSqlType = SqlInt

--  TODO: confirm this is correct
instance HasSqlType Float where getSqlType  = SqlInt
