{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE DeriveAnyClass          #-}
{-# LANGUAGE InstanceSigs            #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module DataBase.Internal where

import Data.Proxy
import DataBase.HasEntity
import DataBase.HasSqlValue
import DataBase.Sql
import DataBase.Table
import GHC.Generics hiding (from)
import GHC.TypeLits

infixr 4 ==.

(==.)
  :: forall field a. (HasSqlValue a, KnownSymbol field)
  => Column '( field, a) -> a -> Relation
(==.) _ x = MkRelation QEq fieldName x
  where
    fieldName = symbolVal (Proxy @field)


-----------------Example-----------------------***
{-

select $ from
       $ \ person -> do -- person is a table i.e Hlist (Table ts)

            -- we can project values from person
            where_ (person ^. field "name" ==. "Allan") -- same monad
            -- where_ sets a value with in our monad. Lets call it Query
            -- where_ is a monadic command with in a Free Monad (Query)
            return person

with in the Query monad, we are building up a Select Query that we will eventually
run in the DB monad.

-- we can use free Monad for the Query and then have an interpreter to get us the result
-}

data User = User
  { name :: String
  , age  :: Int
  , sex  :: String
  } deriving (Show, Eq, Generic, HasEntity)

-- Testing projection into some user table
userTable :: Table (TableFields User)
userTable = table (Proxy @User)

nameEx :: Column '( "name",  String )
nameEx = userTable ^. Field @"name"

equalityExample :: Table (TableFields User) -> Relation
equalityExample x = x ^. Field @"name" ==. "Allan"

exampleQuery :: Sql User ()
exampleQuery =
  select $ from $ \ p -> where_ ( p ^. Field @"name" ==. "Allan")

runExample :: SqlStatement
runExample = runPure exampleQuery
