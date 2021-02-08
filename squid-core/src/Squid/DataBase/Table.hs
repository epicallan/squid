module Squid.DataBase.Table where

import GHC.OverloadedLabels ( IsLabel(..) )

import Squid.Prelude
import Squid.DataBase.TypeLevel ( GetField, type (++) )

infixr 7 :.

type SqlTableName = Text -- Maybe newtype

type TableFieldKind = (Symbol, Type)

type TableFieldsKind = [TableFieldKind]

data TableField (a :: TableFieldKind) = TableField

data QueryFieldValue (a :: Type) = QFieldName Text | QValue a

-- | We are redefining a type similar to Rec from Vinyl, with a reason of avoiding having
-- orphan instances for the 'IsLabel' class.
--
data Table (a :: TableFieldsKind) where
  TNil :: Table '[]
  (:.) :: TableField x -> Table xs -> Table (x ': xs)

-- | For inductively constructing a table
--
class HasTable (ts :: [(Symbol, Type)]) where
  mkTable :: Table ts

instance HasTable '[] where
  mkTable = TNil

instance HasTable ts
  =>  HasTable ('(fieldName, fieldType) ': ts) where
  mkTable = TableField @'(fieldName, fieldType) :. mkTable @ts

-- | Add two tables
addTables :: Table ts -> Table xs -> Table (ts ++ xs)
addTables = \case
  TNil       -> id
  ( x :. xs) -> \ys -> x :. addTables xs ys

-- | Access Table fields using Overloaded labels
instance
  ( GetField fieldPairs fieldName ~ a
  )
  => IsLabel (fieldName :: Symbol) (Table fieldPairs -> TableField a) where

  fromLabel :: Table fieldPairs -> TableField a
  fromLabel _ = TableField
