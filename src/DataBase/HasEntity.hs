module DataBase.HasEntity where

import Data.Kind
import GHC.Generics
import GHC.TypeLits

-- | When we say some entity value a is unique
-- it means we can be able to make some SQL query where
-- we know the returned value will be a list containing one value
-- given it exists

data Table (a :: [(Symbol, Type)]) = Table

class (Generic a, HasEntity' (Rep a) ) => HasEntity a where

  type PrimaryKey a :: (Symbol, Type)
  type PrimaryKey a = '("id", Int)

  type EntityFields a :: [ (Symbol, Type) ]
  type EntityFields a = PrimaryKey a ': EntityFields' (Rep a)

  -- | all the below type families have default implementations
  -- this should make this class instances generically derivable
  -- a user can as well provide custom values
  type UniqueKeys a :: [Symbol] -- list of fields with unique keys
  type UniqueKeys a = '[]

  type ForeignRelations a :: [(Symbol, Type)]
  type ForeignRelations a = '[]


  entity :: a -> Table (EntityFields a)
  entity _  = Table


-- | Prototype a Generic version of HasEntity

class HasEntity' rep where
  type EntityFields' rep :: [ (Symbol, Type) ]

  entity' :: rep a -> Table (EntityFields' rep)
