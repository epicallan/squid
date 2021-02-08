{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Squid.DataBase.HasEntity where

import GHC.Generics

import Squid.Prelude


import Squid.DataBase.Table
import Squid.DataBase.HasNumericalType ( HasNumericalType(..) )
import Squid.DataBase.TypeLevel ( GetField, type (++) )
import Squid.DataBase.SqlColumn
import Squid.DataBase.HasSqlType ( HasSqlType(..) )
import qualified Data.List.NonEmpty as NE

data TableDefinition = TableDefinition
  { tableName    :: SqlTableName
  , tableColumns :: NonEmpty SqlColumn
  }

class
  ( Generic a
  , UniqueConstraints (UniqueKeys a) a
  , ForeignKeyConstraints (ForeignKeys a) a
  ) => HasEntity a where

  type TableFields a :: TableFieldsKind
  type TableFields a = PrimaryKeyField a ': GetTableFieldFromEntity '[] (Rep a)

  type PrimaryKey a :: Type
  type PrimaryKey a = Int

  type PrimaryKeyField a :: (Symbol, Type)
  type PrimaryKeyField a = '("id", PrimaryKey a)

  type UniqueKeys a :: [Symbol]
  type UniqueKeys a = '[]

  type ForeignKeys a :: TableFieldsKind
  type ForeignKeys a = '[]

  type TableName a :: Symbol
  type TableName a = HasTableName (Rep a)

  type TableDefaults a :: [Symbol]
  type TableDefaults a = '[]

  genTableEntity :: Table (TableFields a)

  default genTableEntity
    :: HasTable (TableFields a)
    => Table (TableFields a)
  genTableEntity = mkTable @(TableFields a)

  defaultKeys :: DefaultKeysRec (GetTableDefaultKeyTypes (TableDefaults a) (TableFields a))
  default defaultKeys
     :: (GetTableDefaultKeyTypes (TableDefaults a) (TableFields a) ~ '[])
     => DefaultKeysRec  (GetTableDefaultKeyTypes (TableDefaults a) (TableFields a))
  defaultKeys = Nil

  getSqlColumns :: NonEmpty SqlColumn
  default getSqlColumns :: forall uniqueKeys foreignKeys primaryField primaryType defaultKeys
   . ( Generic a
     , HasEntity a
     , uniqueKeys ~ UniqueKeys a
     , foreignKeys ~ GetForeignKeyValues (ForeignKeys a)
     , defaultKeys ~ GetTableDefaultKeyTypes (TableDefaults a) (TableFields a)
     , '(primaryField, primaryType) ~ PrimaryKeyField a
     , HasSqlType primaryType
     , HasNumericalType primaryType
     , KnownSymbol primaryField
     , GHasSqlColumns (Rep a) uniqueKeys foreignKeys defaultKeys
     )
   => NonEmpty SqlColumn
  getSqlColumns = primaryCol
    NE.:|
    gSqlColumns (Proxy @(Rep a)) (Proxy @uniqueKeys) (Proxy @foreignKeys) (defaultKeys @a)
    where
      primaryCol :: SqlColumn
      primaryCol = SqlColumn
        { colName  = symbolText @primaryField
        , colType  = getSqlType @primaryType
        , colAttrs = catMaybes [addAutoIncrement, Just Primary]
        }

      addAutoIncrement :: Maybe Attribute
      addAutoIncrement = if isNumerical (Proxy @primaryType)
        then Just AutoIncrement else Nothing

  getTableName :: Text
  default getTableName :: KnownSymbol (TableName a) => Text
  getTableName = symbolText @(TableName a)

  mkTableDefinition :: TableDefinition
  mkTableDefinition = TableDefinition (getTableName @a) (getSqlColumns @a)

type family HasPrimaryKey (pk :: (Symbol, Type)) :: Bool where
   HasPrimaryKey '(f, a) = 'True
   HasPrimaryKey _ = 'False

type family GetTableFieldFromEntity (ts :: [(Symbol, Type)]) (rep :: Type -> Type) :: [(Symbol, Type)] where
  GetTableFieldFromEntity ts (S1 ('MetaSel ('Just fieldName) _b _c _d ) (K1 _i fieldType) )
    = '(fieldName, fieldType) ': ts

  GetTableFieldFromEntity ts (a :*: b)
    = (GetTableFieldFromEntity '[] a ++ GetTableFieldFromEntity '[] b) ++ ts

  GetTableFieldFromEntity ts (l :+: r) = TypeError ('Text "Sum types are not supported as table fields")

  GetTableFieldFromEntity ts (C1 _ a) = GetTableFieldFromEntity ts a

  GetTableFieldFromEntity ts (D1 _ a) = GetTableFieldFromEntity ts a

-- | Make sure provided unique keys are with in the table
type family UniqueConstraints (ts :: [Symbol]) (a :: Type) :: Constraint where
  UniqueConstraints '[] _ = ()
  UniqueConstraints (x ': xs) a = (IsTableField x (TableFields a), UniqueConstraints xs a)

-- | Make sure provided keys exist with in current table and reference table
type family ForeignKeyConstraints (ts :: TableFieldsKind) (table :: Type) :: Constraint where
  ForeignKeyConstraints ( '( field, ForeignKeyReferencedTable refTable refCol ) ': ts ) a =
        ( HasEntity refTable
        , IsTableField field (TableFields a)
        , HasUniqueForeignParentField refCol (UniqueKeys refTable)
        )
  ForeignKeyConstraints '[] a = ()

type family HasUniqueForeignParentField (pfield :: Symbol) (parentUniques :: [Symbol]) :: Constraint where
  HasUniqueForeignParentField x ( x ': xs) = ()
  HasUniqueForeignParentField x ( y ': xs) = (HasUniqueForeignParentField x xs)
  HasUniqueForeignParentField x '[] = TypeError ('Text "Parent Foreign field should be Unique")

-- | Retrieve tableName from data type name
type family HasTableName (rep :: k -> Type) :: Symbol where
  HasTableName (D1 ('MetaData name _ _ _) _) =  name
  HasTableName _  = TypeError ('Text "GHC error: Wrong provided data type")

-- | Constraint to witness presence of a table field in a list of table fields
--
type family IsTableField (field :: Symbol) (ts :: [(Symbol, Type)]) :: Constraint where
   IsTableField x '[] = TypeError ('Text "Provided field Name is not part of the database table record")
   IsTableField x ( '(x, t) ': ts) = ()
   IsTableField x ( '(s, t) ': ts ) = IsTableField x ts

type family GetForeignKeyValues (a :: TableFieldsKind) :: [(Symbol, Symbol, Symbol)] where
  GetForeignKeyValues ( '(f, ForeignKeyReferencedTable a s) ': ts) = '(f, TableName a, s) ': GetForeignKeyValues ts
  GetForeignKeyValues '[] = '[]

-- | Get the corresponding type for each provided default symbol
type family GetTableDefaultKeyTypes (ts :: [Symbol]) (fs :: [(Symbol, Type)]) :: [(Symbol, Type)] where
  GetTableDefaultKeyTypes ( s ': ts ) fs = GetField fs s ': GetTableDefaultKeyTypes ts fs
  GetTableDefaultKeyTypes '[]  _  = '[]
