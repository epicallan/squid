{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE DefaultSignatures       #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module DataBase.HasEntity where

import Data.Kind
import Data.Proxy
import DataBase.Table
import DataBase.TypeLevel
import GHC.Generics
import GHC.TypeLits

-- | When we say some entity value a is unique
-- it means we can be able to make some SQL query where
-- we know the returned value will be a list containing one value
-- given it exists

class
  ( Generic a
  , GHasEntity (Rep a)
  , UniqueConstraint (UniqueKeys a) (EntityFields a)
  , ForeignKeyConstraint (ForeignKeys a) (EntityFields a)
  ) => HasEntity a where
  type EntityFields a ::  [ (Symbol, Type) ]
  type EntityFields a = GEntityFields (Rep a)

  type TableName a :: Symbol
  type TableName a =  HasTableName (Rep a)

  type PrimaryKey a :: (Symbol, Type)
  type PrimaryKey a = '("id", Int)

  type UniqueKeys a :: [Symbol]
  type UniqueKeys a = '[]

  type ForeignKeys a :: [(Symbol, Type)]
  type ForeignKeys a = '[]

  tableName :: String
  default tableName :: (KnownSymbol (TableName a)) => String
  tableName = symbolVal (Proxy @(TableName a))

  entity :: a -> Table (EntityFields a)
  default entity :: (GEntityFields (Rep a) ~  (EntityFields a)) => a -> Table (EntityFields a)
  entity x = gentity (from x)

class GHasEntity rep where
  type GEntityFields rep :: [ (Symbol, Type) ]

  gentity :: rep a -> Table (GEntityFields rep)

instance GHasEntity U1 where
  type GEntityFields U1 =  TypeError ('Text "Unit types are not supported")
  gentity _ = error "Provided unsupported table data type"

instance GHasEntity V1 where
  type GEntityFields V1 = TypeError ('Text "Void types are not supported")

  gentity _ = error "Provided unsupported table data"

instance GHasEntity (S1 ('MetaSel ('Just name) _b _c _d ) (K1 _i a) ) where
  type GEntityFields (S1 ('MetaSel ('Just name) _b _c _d ) (K1 _i a) ) = '[ '( name, a ) ]

  gentity _  = Column :. Nil

instance (GHasEntity a, GHasEntity b) => GHasEntity (a :*: b) where
  type GEntityFields (a :*: b) = GEntityFields a ++ GEntityFields b

  gentity (a :*: b) =  x `addTables` y
    where
      x :: Table (GEntityFields a)
      x = gentity a

      y :: Table (GEntityFields b)
      y = gentity b

instance GHasEntity (a :+: b) where
  type GEntityFields (a :+: b) = TypeError ('Text "Sum types are not supported as table records")

  gentity _ = error "Sum types are not supported as table records"

instance (GHasEntity a) => GHasEntity (C1 _y a) where
  type GEntityFields (C1 _y a) = GEntityFields a

  gentity (M1 a) = gentity a

instance (GHasEntity a) => GHasEntity (D1 _y a) where
  type GEntityFields (D1 _y a) = GEntityFields a

  gentity (M1 a) = gentity a

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
