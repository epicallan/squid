module DataBase.TypeLevel where

import Data.Kind
import GHC.Generics
import GHC.TypeLits

-- | got from lens Generics
type family Both (m1 :: Maybe a) (m2 :: Maybe a) :: Maybe a where
  Both ('Just a) ('Just a) = 'Just a

type family Alt (m1 :: Maybe a) (m2 :: Maybe a) :: Maybe a where
  Alt ('Just a) _ = 'Just a
  Alt _ b = b

type family HasTotalFieldP (field :: Symbol) f :: Maybe Type where -- We need this type family
  HasTotalFieldP field (S1 ('MetaSel ('Just field) _ _ _) (Rec0 t))
    = 'Just t
  HasTotalFieldP field (l :*: r)
    = Alt (HasTotalFieldP field l) (HasTotalFieldP field r)
  HasTotalFieldP field (l :+: r)
    = Both (HasTotalFieldP field l) (HasTotalFieldP field r)
  HasTotalFieldP field (S1 _ _)
    = 'Nothing
  HasTotalFieldP field (C1 _ f)
    = HasTotalFieldP field f
  HasTotalFieldP field (D1 _ f)
    = HasTotalFieldP field f
  HasTotalFieldP field (K1 _ _)
    = 'Nothing
  HasTotalFieldP field U1
    = 'Nothing
  HasTotalFieldP field V1
    = 'Nothing



type family (x :: [k]) ++ (y :: [k]) :: [k] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)
