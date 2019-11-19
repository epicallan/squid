{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
module Squid.DataBase.SqlStatement where

import Data.String
import Data.Text
import qualified Data.Text as T

import Squid.DataBase.HasSqlField
import Squid.DataBase.Column

-- | for each data type used as a Model we obtain a type level list
-- containing its fields and related type. See HasEntity class
data RelationOp = QEq | QGT | QLT
  deriving (Eq)

instance Show RelationOp where
  show = \case
    QEq -> "="
    QGT -> ">"
    QLT -> "<"

data FieldValue where
  FieldValue :: forall a . HasSqlField a => a -> FieldValue

data RelationAction = RelationAction
  { raOp :: RelationOp
  , rafieldName :: ColumnName
  , raActionValue :: FieldValue
  }

data SqlQueryType =
    SelectQuery
  | InsertQuery
  | UpdateQuery
  deriving (Eq)

instance Show SqlQueryType where
  show = \case
    SelectQuery -> "select"
    InsertQuery -> "insert"
    UpdateQuery -> "update"

data SqlStatement = SqlStatement
  { sqlTable     :: Text
  , sqlWhere     :: [RelationAction]
  , sqlInserts   :: [FieldValue]
  , sqlQueryType :: SqlQueryType
  }

newtype QueryStatement = QueryStatement { unQueryStatement :: Text }
  deriving (Show, Eq)

newtype QueryAction = QueryAction { unQueryAction :: Text }
  deriving (Show, Eq)
  deriving Semigroup via Text
  deriving Monoid via Text

newtype SubQuery = SubQuery { unSubQuery :: Text }
  deriving (Show, Eq)
  deriving Semigroup via Text
  deriving Monoid via Text

data SqlQuery = SqlQuery
  { sqStatement :: QueryStatement
  , sqValues :: [FieldValue]
  }

defaultStatement :: SqlStatement
defaultStatement = SqlStatement mempty [] [] SelectQuery

-- | TODO: should be more general
getActionValues :: SqlStatement -> [FieldValue]
getActionValues SqlStatement {..} = raActionValue <$> sqlWhere

-- | TODO: should be more general
getAction :: SqlStatement -> QueryAction
getAction SqlStatement {..} = QueryAction $ " where " <> actions
  where
    actions =  T.intercalate " and " $ extractQuery <$> sqlWhere
    extractQuery (RelationAction op fname _) = fname <> " " <> fromString (show op) <> " ? "

-- | TODO: should take in a list of column names in preferred order
-- such that ordering is explicit
getSubQuery :: SqlStatement -> SubQuery
getSubQuery SqlStatement {..} = case sqlQueryType of
  SelectQuery -> SubQuery $ fromString (show sqlQueryType) <> " * from " <> sqlTable
  _           -> error "TODO"

mkQueryStatement :: SubQuery -> QueryAction -> QueryStatement
mkQueryStatement (SubQuery x) (QueryAction y) = QueryStatement $ x <> y

sqlQuery :: SqlStatement -> SqlQuery
sqlQuery x  =
  let subQuery = getSubQuery x
      action = getAction x
      actionValues = getActionValues x
  in  SqlQuery (mkQueryStatement subQuery action) actionValues
