{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE DefaultSignatures       #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module DataBase.Internal.HasEntity where

import Data.Kind
import Data.Proxy
import GHC.Generics
import GHC.TypeLits

import DataBase.Internal.Table
import DataBase.Internal.TypeLevel

class
  ( Generic a
  , UniqueConstraint (UniqueKeys a) (TableFields a)
  , ForeignKeyConstraint (ForeignKeys a) (TableFields a)
  ) => HasEntity a where
  type TableFields a :: [(Symbol, Type)]
  type TableFields a = PrimaryKeyField a ': GTableFields '[] (Rep a)

  type PrimaryKey a :: Type
  type PrimaryKey a = Int

  type PrimaryKeyField a :: (Symbol, Type)
  type PrimaryKeyField a = '("id", PrimaryKey a)

  type UniqueKeys a :: [Symbol]
  type UniqueKeys a = '[]

  type ForeignKeys a :: [(Symbol, Type)]
  type ForeignKeys a = '[]

  type TableName a :: Symbol
  type TableName a = HasTableName (Rep a)

  table :: Proxy a -> Table (TableFields a)

  getTableName :: Proxy a -> String
  default getTableName :: KnownSymbol (TableName a) => Proxy a -> String
  getTableName _ = symbolVal (Proxy @(TableName a))

  default table
    :: HasTable (TableFields a)
    => Proxy a -> Table (TableFields a)
  table _ = getTable (Proxy @(TableFields a))


type family GTableFields (ts :: [(Symbol, Type)]) (rep :: Type -> Type) :: [(Symbol, Type)] where
  GTableFields ts (S1 ('MetaSel ('Just name) _b _c _d ) (K1 _i a) ) = '( name, a ) ': ts

  GTableFields ts (a :*: b) = (GTableFields '[] a ++ GTableFields '[] b) ++ ts

  GTableFields ts (l :+: r) = TypeError ('Text "Sum types are not supported as table records")

  GTableFields ts (C1 _ a) = GTableFields ts a

  GTableFields ts (D1 _ a) = GTableFields ts a


-- | Make sure provided unique keys are with in the table
type family UniqueConstraint (ts :: [Symbol]) (table :: [(Symbol, Type)]) :: Constraint where
  UniqueConstraint _ _ = () -- TODO: implement me

-- | Make sure provided keys with in the table exists
type family ForeignKeyConstraint (ts :: [ (Symbol, Type) ]) (table :: [(Symbol, Type)]) :: Constraint where
  ForeignKeyConstraint _ _ = () -- TODO: implement me

type family HasTableName (rep :: k -> Type) :: Symbol where
  HasTableName (D1 ('MetaData name _ _ _) _) =  name
  HasTableName _  = TypeError ('Text "GHC error: Wrong provided data type")

type family IsTableField (field :: Symbol) (ts :: [(Symbol, Type)]) :: Symbol where
   IsTableField x '[] = TypeError ('Text "Error provided type is not in list")
   IsTableField x ( '(x, t) ': ts) = x
   IsTableField x ( '(s, t) ': ts ) = IsTableField x ts
