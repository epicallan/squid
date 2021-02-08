{-# LANGUAGE PatternSynonyms #-}
module Squid.DataBase.SqlColumn where

import Data.Singletons.Bool ( SBool(..), SBoolI(..) )
import Data.Vinyl hiding (ElField (..))
import GHC.Generics

import Squid.Prelude

import Squid.DataBase.HasSqlType ( HasSqlType(..), SqlType )
import Squid.DataBase.HasFieldValues ( HasSqlValue(..), SqlValue, type (:=) )
import Squid.DataBase.TypeLevel ( IsNotNull, IsMember )
import Squid.DataBase.Table

-- | Contains Table and Column being referenced in a Foreign Key constraint.
data ForeignKeyReferencedTable (table :: Type) (foreignColumn :: Symbol)

-- | Attributes for a column
--
-- Each attribute can have a uniq interpretation
-- depending on the SQL database in use
--
data Attribute =
    NotNull
  | ForeignKey Reference
  | Primary
  | Unique
  | AutoIncrement
  | Default SqlValue
  deriving (Eq)

type ColumnName = Text

-- | ForeignKey Reference values
--
data Reference = Reference
  { refTable  :: SqlTableName
  , refColumn :: ColumnName
  } deriving (Show, Eq)

attributeName :: Attribute -> Text
attributeName = \case
  NotNull       -> "NOT NULL"
  ForeignKey _  -> "FOREIGN KEY"
  Primary       -> "PRIMARY KEY"
  Unique        -> "UNIQUE"
  AutoIncrement -> "SERIAL"
  Default _     -> "DEFAULT"

data SqlColumn = SqlColumn
  { colName  :: ColumnName
  , colType  :: SqlType
  , colAttrs :: [Attribute]
  }

data Field (field :: TableFieldKind) where
  Field :: (KnownSymbol s, HasSqlValue t) => t -> Field '(s,t)

-- | We need a Rec (record) since we want to carry around the default
-- key type and value
type DefaultKeysRec  (ts :: TableFieldsKind) = Rec Field ts

pattern Nil :: Rec f '[]
pattern Nil = RNil

-- | For obtaining default values as specified within the 'HasEntity' class
--
class HasDefaultValue (a :: TableFieldKind) (ts :: TableFieldsKind) where
  getDefValue :: DefaultKeysRec  ts -> Maybe (Field a)

instance HasDefaultValue '(s, a) '[] where
  getDefValue _ = Nothing

instance  {-# Overlapping #-} HasDefaultValue '(s, a) ( s := a ': ts) where
  getDefValue (x :& _) = Just x

instance  {-# Overlapping #-} (HasDefaultValue '(s, a)  ts)
  => HasDefaultValue '(s, a) ( x := y ': ts) where
  getDefValue (_ :& ts) = getDefValue ts

-- | This class enables us to get Sql column values. Its used in
-- the HasEntity class
class GHasSqlColumns
  (columnRep   :: Type -> Type)
  (uniqueKeys  :: [Symbol])
  (foreignKeys :: [(Symbol, Symbol, Symbol)])
  (defaults    :: [(Symbol, Type)]) where
  gSqlColumns
    :: Proxy columnRep
    -> Proxy uniqueKeys
    -> Proxy foreignKeys
    -> DefaultKeysRec defaults
    -> [SqlColumn]

instance  {-# OVERLAPPABLE  #-}
  ( isUniq ~ IsMember name uniqKeys
  , '(isForeignKey, refCol, refTable) ~ IsForeignKey name foreignKeys
  , isNotNull ~ IsNotNull a
  , HasSqlType a
  , SBoolI isUniq
  , SBoolI isForeignKey
  , SBoolI isNotNull
  , KnownSymbol name
  , KnownSymbol refTable
  , HasDefaultValue '(name, a)  ts
  , HasSqlValue a
  , KnownSymbol refCol
  )
  => GHasSqlColumns (S1 ('MetaSel ('Just name) _b _c _d ) (K1 _i a)) uniqKeys foreignKeys ts where
  gSqlColumns _ _ _ defaultKeysRec = [SqlColumn fieldName sqlColType attrs]
    where
      sqlColType :: SqlType
      sqlColType = getSqlType @a

      -- | need to use case statements to bring HasSqlValue class into scope
      defValue :: Maybe Attribute
      defValue = case getDefValue @'(name, a) defaultKeysRec of
        Just (Field x) -> Just $ Default $ toSqlValue x
        Nothing        -> Nothing

      attrs :: [Attribute]
      attrs = catMaybes
        [ addAttr (sbool @isForeignKey) Unique
        -- foreign key field is also unique.
        , addAttr (sbool @isForeignKey) $ ForeignKey reference
        , addAttr (sbool @isUniq) Unique
        , addAttr (sbool @isNotNull) NotNull
        , defValue
        ]
      fieldName :: Text
      fieldName = pack $ symbolVal (Proxy @name)

      reference :: Reference
      reference = Reference
          (pack $ symbolVal (Proxy @refTable))
          (pack $ symbolVal (Proxy @refCol))

      addAttr :: SBool b -> Attribute -> Maybe Attribute
      addAttr sb attr = case sb of
        STrue  -> Just attr
        SFalse -> Nothing

instance (TypeError ('Text "Unit types are not supported as table records"))
  => GHasSqlColumns U1 uniqKeys foreignKeys ts where
  gSqlColumns _ _ _ _ = error "Unit types not supported"

instance (TypeError ('Text "Void types are not supported as table records"))
  => GHasSqlColumns V1 uniqKeys foreignKeys ts where
  gSqlColumns _ _ _ _ = error "Void types not supported"


instance (TypeError ('Text "Sum types are not supported as table records"))
  => GHasSqlColumns (a :+: b) uniqKeys foreignKeys ts where
  gSqlColumns _ _ _ _ = error "Sum types are not supported"

instance (GHasSqlColumns a uniqKeys foreignKeys ts, GHasSqlColumns b uniqKeys foreignKeys ts)
  => GHasSqlColumns (a :*: b) uniqKeys foreignKeys ts where
  gSqlColumns _ proxyUniqKeys proxyFKeys defaultKeysRec
    = gSqlColumns (Proxy @a) proxyUniqKeys proxyFKeys defaultKeysRec
    <> gSqlColumns (Proxy @b) proxyUniqKeys proxyFKeys defaultKeysRec

instance (GHasSqlColumns a uniqKeys foreignKeys ts)
  => GHasSqlColumns (C1 _b a) uniqKeys foreignKeys ts where
  gSqlColumns _ proxyUniqKeys proxyFKeys  defaultKeysRec = gSqlColumns (Proxy @a) proxyUniqKeys proxyFKeys defaultKeysRec

instance (GHasSqlColumns a uniqKeys foreignKeys ts)
  => GHasSqlColumns (D1 _b a) uniqKeys foreignKeys ts where
  gSqlColumns _ proxyUniqKeys proxyFKeys defaultKeysRec = gSqlColumns (Proxy @a) proxyUniqKeys proxyFKeys defaultKeysRec

type family IsForeignKey (key :: Symbol) (fs :: [(Symbol, Symbol, Symbol)]) :: (Bool, Symbol, Symbol) where
  IsForeignKey key ( '(key, refTable, refCol) ': ts  ) = '( 'True, refCol, refTable)
  IsForeignKey key ( '(x, f, t) ': ts) = IsForeignKey key ts
  IsForeignKey key '[] = '( 'False, "", "")
