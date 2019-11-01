-- | Co-records are sum types over an extensible set of options.
module DataBase.Corec where

import Data.Kind (Type)
import Data.Proxy
-- import DataBase.TypeLevel
import GHC.TypeLits

import DataBase.Column (TableField (..))

data Corec (ts :: [(Symbol, Type)]) :: Type where
  Stop :: KnownSymbol s => TableField '(s, a) -> Corec ('(s, a) ': ts)
  Skip :: KnownSymbol s => Corec ts -> Corec ('(s, a) ': ts)

class ElemOf ts (field :: Symbol) where
  inject :: Proxy field -> Corec ts

instance {-# OVERLAPPING #-} (a' ~ a, KnownSymbol s)
  => ElemOf ('(s, a) ': rs) s where
  inject _ = Stop (TableField @'(s, a))

instance (KnownSymbol s', ElemOf ts s)
  => ElemOf ( '(s', a') ': ts) s where
  inject = Skip . inject