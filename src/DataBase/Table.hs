module DataBase.Table where

import Data.Kind (Type)
import Data.Proxy
import DataBase.TypeLevel
import GHC.TypeLits

data Column (a :: (Symbol, Type)) = Column

type FieldName = String

data Field (a :: Symbol) = Field

type a  := b = '(a, b)

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

addColumn :: Column x -> Table xs -> Table ( x ': xs)
addColumn x xs = x :. xs

-- | TODO: create a lens out of Table.

type Lens s a = forall f. Functor f => (a -> f a) -> s -> f s

type family ColumnType (ts :: [(Symbol, Type)]) (s :: Symbol) :: (Symbol, Type) where
  ColumnType '[] _ = TypeError ('Text "Can't find value in list")
  ColumnType ( '(field, t) ': ts) field = '(field, t)
  ColumnType ( '(f, t) ': ts ) field = ColumnType ts field

getter :: Field a -> Table ts -> Column (ColumnType ts a)
getter _  _ = Column

-- | TODO: setter does nothing, so we don't need it
lens :: Field a -> Lens (Table ts) (Column (ColumnType ts a))
lens field f s =  s  <$ f (getter field s) -- | TODO: change to shorter version

infixr 7 ^.

(^.) :: Table ts -> Field a -> Column (ColumnType ts a)
_ ^.  _ = Column

-- from_ :: (Table ts -> a) -> Table ts -> a
-- from_  f = f


-- select :: a -> b
-- select = undefined

-- ex :: (Table ts -> a) -> a
-- ex = select $ from_ $ \ _x -> undefined
