{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE TypeApplications #-}
module DataBase.Internal.Sql where

import Control.Monad.Free
import Control.Monad.State
import Data.Kind
import Data.Proxy

import DataBase.Internal.HasEntity
import DataBase.Internal.HasSqlValue
import DataBase.Internal.Table

-- | for each data type used as a Model we obtain a type level list
-- containing its fields and related type. See HasEntity class

data RelationOps = QEq | QGT | QLT
  deriving Show

data SqlQueryType =
    SelectQuery
  | InsertQuery
  | UpdateQuery
  deriving (Show, Eq)

data SqlStatement = SqlStatement
  { sqlTable     :: String
  , sqlWhere     :: Maybe Relation -- assume we only have one relation per query
  , sqlQueryType :: Maybe SqlQueryType
  } deriving Show

data Relation where
  MkRelation
    :: forall a. (HasSqlValue a)
    => RelationOps -> FieldName -> a -> Relation

type SqlWriter = StateT SqlStatement

instance Show Relation where
  show (MkRelation r f a) =
    "Relation " <> show r <> " " <> f <> " " <> show a

data SqlF entity next =
    Select next
  | Insert next
  | Where Relation next
  | From next
  deriving (Functor, Show)

type Sql entity = Free (SqlF entity)


-- | the sqlF monad will be interepreted over
-- some state Monad such that we can build up queries

select_ :: Sql entity ()
select_ = liftF $ Select ()

from_ :: Sql entity ()
from_ = liftF $ From ()

insert_ :: Sql entity ()
insert_ = liftF $ Select ()

where_ :: Relation -> Sql entity ()
where_ x = liftF $ Where x ()

from
  :: (Table (TableFields a) -> Sql a r)
  -> Table (TableFields a)
  -> Sql a r
from f x = do
  from_
  f x

select'
  :: forall a. HasEntity a
  => ( Table (TableFields a) -> Sql a () )
  -> Sql a ()
select' f = do
  select_
  f entityTable
  where
    entityTable :: Table (TableFields a)
    entityTable = table (Proxy @a)

runSqlWriter :: forall (entity :: Type) r m. (Monad m, HasEntity entity) => Sql entity r -> SqlWriter m r
runSqlWriter (Pure r) = return r
runSqlWriter (Free (From r)) = do
  let tableName = getTableName (Proxy @entity)
  modify' ( \ s -> s {sqlTable = tableName } )
  runSqlWriter r

runSqlWriter (Free (Select r)) = do
  modify' (\s -> s { sqlQueryType = Just SelectQuery })
  runSqlWriter r

runSqlWriter (Free (Where relation r)) = do
  modify' (\s -> s { sqlWhere = Just relation  } )
  runSqlWriter r

runSqlWriter (Free (Insert r)) = do
  modify' (\s -> s { sqlQueryType = Just InsertQuery })
  runSqlWriter r

runSqlWriterPure
  :: forall (entity :: Type) r . HasEntity entity
  => Sql entity r -> SqlStatement
runSqlWriterPure x = execState (runSqlWriter x) initial
 where
   initial :: SqlStatement
   initial = SqlStatement mempty Nothing Nothing
