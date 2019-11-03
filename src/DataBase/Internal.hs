{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE DeriveAnyClass          #-}
{-# LANGUAGE InstanceSigs            #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module DataBase.Internal where

-- import Data.Kind
-- import DataBase.Corec
import DataBase.HasEntity
import DataBase.HasSqlValue
import DataBase.Table
import GHC.Generics
-- import Control.Monad.Reader
-- import Data.Proxy
import DataBase.Table
import GHC.TypeLits

type family DataBase :: Symbol

type instance DataBase = "MyDB"

-- | for each data type used as a Model we obtain a type level list
-- containing its fields and related type. See HasEntity class

data RelationOps = QEq | QGT | QLT

data Relation where
  MkRelation
    :: forall a . HasSqlValue a
    => RelationOps -> FieldName -> a -> Relation

data Sql a =
    Select a
  | Insert a
  | Where Relation a

-- -----------------Example-----------------------***

data User = User
  { name :: String
  , age  :: Int
  , sex  :: String
  } deriving (Show, Eq, Generic, HasEntity)

-- Testing projection into some user table

userTable :: Table (EntityFields User)
userTable = undefined


{-
 -- we want to person to represent something similar to lenses
 -- ^. is like an accessor operator on a lens
 --
 field @"name" ^. person =. "Allan"

select $ from
       $ \ person -> do -- person is not exactly the generic original data type
            -- it can be a generically derived associated data type
            -- For our case its going to be an Hlist (Table ts)

            -- we can project values from person
            where_ (person ^. field "name" ==. "Allan") -- same monad
            -- where_ sets a value with in our monad. Lets call it Query
            -- where_ is a monadic command with in a Free Monad (Query)
            return person

with in the Query monad, we are building up a Select Query that we will eventually
run in the DB monad.

-- we can use free Monad for the Query and then have an interpreter to get us the result
-}

-- | projections into some type like person as in the select example above
-- gives us a corec value at which we can obtain a field symbol and its related type.
-- (^.) :: forall f fs sing . (ElemOf fs f) => Table fs -> sing (IsTableField f fs) -> Corec fs
-- (^.) _ _ = inject (Proxy @f)



-- (==.)
--   :: forall a s ts. (HasSqlValue a, KnownSymbol s)
--   => Corec ( '(s, a) ': ts)
--   -> a
--   -> Relation
-- (==.) corec x = case corec of
--   Stop Column -> MkRelation QEq fieldName x
--   Skip _          -> error "GHC error shouldn't happen"
--   where
--     fieldName :: String
--     fieldName = symbolVal (Proxy @s)
