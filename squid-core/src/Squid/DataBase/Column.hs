module Squid.DataBase.Column where

import Data.Kind (Type)
import Data.Text
import GHC.TypeLits

data Column (a :: (Symbol, Type)) = Column

type ColumnName = Text

data ColumnLabel (a :: Symbol) = ColumnLabel

type a  := b = '(a, b)

type family ColumnType (ts :: [(Symbol, Type)]) (s :: Symbol) :: (Symbol, Type) where
  ColumnType '[] _ = TypeError ('Text "Can't find value in list")
  ColumnType ( '(field, t) ': ts) field = '(field, t)
  ColumnType ( '(f, t) ': ts ) field = ColumnType ts field
