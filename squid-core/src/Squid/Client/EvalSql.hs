{-# OPTIONS_GHC -Wno-unused-imports #-}
module Squid.Client.EvalSql where

import Control.Monad.Free
import Control.Monad.State (StateT, execState, modify')
import Data.Functor.Identity
import Data.Proxy

import Squid.DataBase

import qualified Data.List.NonEmpty as NE

-- | consider not using StateT,
-- by maybe returning the result into next
type EvalSql = StateT SqlStatement Identity

evalSql :: forall a r . Sql a r -> EvalSql r
evalSql (Pure r) = return r

evalSql (Free (From r)) = evalSql r

evalSql (Free (Select r)) = do
  modify' (\s -> s { sqlQueryType = SelectQuery })
  evalSql r

evalSql (Free (Where relationAction r)) = do
  modify' (\s -> s { sqlWhere = relationAction : sqlWhere s  } )
  evalSql r

evalSql (Free (Insert x r)) = do
  modify' (\s -> s { sqlQueryType = InsertQuery, sqlInserts = x })
  evalSql r

evalSql (Free (Raw x r)) = do
  modify' (\s -> s { sqlQueryType = RawQuery, sqlRaw = x })
  evalSql r

getStatement
  :: Sql a ()
  -> SqlStatement
getStatement sqlCommand
  = execState (evalSql sqlCommand) (mkDefStatement SelectQuery)

getRawStatement
  :: Proxy 'RawQuery
  -> Sql a ()
  -> SqlStatement
getRawStatement _ sqlCommand = case sqlCommand of
  Free (Raw rawSql _) -> defStmt { sqlRaw = rawSql, sqlQueryType = RawQuery }
  _                   -> error "GetRawStatement: should only be called with Raw"
  where
    defStmt = mkDefStatement RawQuery
