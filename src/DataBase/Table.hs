-- | TODO: rename to Table
module DataBase.Table where

import Data.Kind (Type)
import GHC.TypeLits
import DataBase.TypeLevel

data Column (a :: (Symbol, Type)) = Column

type FieldName = String

infixr 7 :.

data Table (a :: [(Symbol, Type)]) where
  Nil  :: Table '[]
  (:.) :: Column x -> Table xs -> Table (x ': xs)

-- | add to 2 tables
addTables :: Table ts -> Table xs -> Table (ts ++ xs)
addTables = \case
  Nil -> id
  ( x :. xs) -> \ ys -> x :. addTables xs ys


-- | TODO: create a lens out of Table.

