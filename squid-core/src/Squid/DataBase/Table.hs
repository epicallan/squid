{-# LANGUAGE InstanceSigs     #-}
{-# LANGUAGE TypeApplications #-}
module Squid.DataBase.Table where

import Data.Kind (Type)
import Data.Proxy
import GHC.OverloadedLabels
import GHC.TypeLits

import Squid.DataBase.TypeLevel
import Squid.DataBase.Column

infixr 7 :.

data Table (a :: [(Symbol, Type)]) where
  Nil  :: Table '[]
  (:.) :: Column x -> Table xs -> Table (x ': xs)

-- | inductively create a Table out of a list of types
class HasTable (ts :: [(Symbol, Type)]) where
  getTable :: Proxy ts -> Table ts

instance HasTable '[] where
  getTable _ = Nil

instance HasTable ts =>  HasTable ( '(s, t) ': ts) where
  getTable _ = col :. getTable (Proxy @ts)
    where
      col :: Column '(s, t)
      col = Column

-- | add to 2 tables
addTables :: Table ts -> Table xs -> Table (ts ++ xs)
addTables = \case
  Nil -> id
  ( x :. xs) -> \ ys -> x :. addTables xs ys

-- * Table Column accessors
infixr 7 ^.

-- | Access Table in lens like style
(^.) :: Table ts -> ColumnLabel a -> Column (ColumnType ts a)
_ ^.  _ = Column

-- | Access Table fields using Overloaded labels
instance (ColumnType xs s ~ ts)
  => IsLabel (s :: Symbol) (Table xs -> Column ts) where
  fromLabel :: Table xs -> Column ts
  fromLabel _ = Column
