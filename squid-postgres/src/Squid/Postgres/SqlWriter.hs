module Squid.Postgres.SqlWriter where

import Control.Monad.Free
import Control.Monad.State (StateT, execState, modify')
import Data.Functor.Identity
import Data.Proxy
import Data.String (fromString)

import Squid.DataBase

type SqlWriter = StateT SqlStatement Identity

mkSqlWriter :: forall a r. HasEntity a => Sql a r -> SqlWriter r
mkSqlWriter (Pure r) = return r

mkSqlWriter (Free (From r)) = do
   let tableName = fromString $ getTableName (Proxy @a)
   modify' (\s -> s {  sqlTable =  tableName  } )
   mkSqlWriter r

mkSqlWriter (Free (Select r)) = do
  modify' (\s -> s { sqlQueryType = SelectQuery })
  mkSqlWriter r

mkSqlWriter (Free (Where relation r)) = do
  modify' (\s -> s { sqlWhere = relation : sqlWhere s  } )
  mkSqlWriter r

mkSqlWriter (Free (Insert x r)) = do
  modify' (\s -> s { sqlQueryType = InsertQuery, sqlInserts = x : sqlInserts s })
  mkSqlWriter r

runSqlWriter
  :: (HasEntity entity)
  => Proxy entity -> Sql entity () -> SqlStatement
runSqlWriter _ fsql = execState (mkSqlWriter fsql) defaultStatement


