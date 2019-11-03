{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE StandaloneDeriving #-}
module DataBase.Sql where

import Control.Monad.Free
import Control.Monad.State
import Data.Kind
import Data.Proxy
import DataBase.HasEntity
import DataBase.HasSqlValue
import DataBase.Table
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

type SqlQuery = StateT SqlStatement

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

-- where_ :: Table (TableFields a) -> Sql entity ()
-- where_ x = do


-- | syntax sugar over commands
{-
 we want something like this
 exampleCmd :: Sql entity r
 exampleCmd = select $ from $ \ entity -> do-something

-}

from
  :: (Table (TableFields a) -> Sql a r)
  -> Table (TableFields a)
  -> Sql a r
from f = \ x -> do
  from_
  f x

select
  :: forall a . HasEntity a
  => ( Table (TableFields a) -> Sql a () )
  -> Sql a ()
select f = do
  select_
  f entityTable
  where
    entityTable :: Table (TableFields a)
    entityTable = table (Proxy @a)



runSql :: forall (entity :: Type) r m. (Monad m, HasEntity entity) => Sql entity r -> SqlQuery m r
runSql (Pure r) = return r -- run sql query
runSql (Free (From r)) = do
  let tableName = getTableName (Proxy @entity)
  modify' ( \ s -> s {sqlTable = tableName } )
  runSql r

runSql (Free (Select r)) = do
  modify' (\s -> s { sqlQueryType = Just SelectQuery })
  runSql r

runSql (Free (Where relation r)) = do
  modify' (\s -> s { sqlWhere = Just relation  } )
  runSql r

runSql (Free (Insert r)) = do
  modify' (\s -> s { sqlQueryType = Just InsertQuery })
  runSql r

runPure
  :: forall (entity :: Type) r . HasEntity entity
  => Sql entity r -> SqlStatement
runPure x = execState (runSql x)  initial
 where
   initial :: SqlStatement
   initial = SqlStatement mempty Nothing Nothing
