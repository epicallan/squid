module Squid.DataBase.TypeLevel where

import Data.Kind
import GHC.TypeLits

type family (x :: [k]) ++ (y :: [k]) :: [k] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

type family IsMember (field :: Symbol) (list :: [Symbol]) :: Bool where
  IsMember x '[] = 'False
  IsMember x (x ': xs) = 'True
  IsMember x (y ': xs) = IsMember x xs

type family IsNotNull (a :: Type) :: Bool where
  IsNotNull (Maybe a) = 'False
  IsNotNull _ = 'True

type family GetField (ts :: [(Symbol, Type)]) (s :: Symbol) :: (Symbol, Type) where
  GetField '[] _ = TypeError ('Text "Can't find value in list")
  GetField ( '(field, t) ': ts) field = '(field, t)
  GetField ( '(f, t) ': ts ) field = GetField ts field
