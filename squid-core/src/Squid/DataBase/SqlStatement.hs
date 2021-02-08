module Squid.DataBase.SqlStatement where

import Squid.Prelude

import Squid.DataBase.HasFieldValues
import Squid.DataBase.SqlColumn
import Squid.DataBase.Table

data RelationOp = QEq | QGT | QLT
  deriving (Eq)

newtype CreateSql = CreateSql { unCreateSql :: Text }
  deriving (Monoid, Semigroup) via Text

newtype RawStatement = RawStatement { unRawStatement :: Text }
   deriving (Monoid) via Text

instance Semigroup RawStatement where
  (RawStatement a) <> (RawStatement b) = RawStatement (a <> " ; " <> b)

relationOpSymbol :: RelationOp -> Text
relationOpSymbol = \case
  QEq -> "="
  QGT -> ">"
  QLT -> "<"

type TableColumnPair = (SqlTableName,  ColumnName)

data RelationAction = RelationAction
  { raOp          :: RelationOp
  , raFieldName   :: ColumnName
  , raActionValue :: FieldValue
  }

data SqlQueryType =
    SelectQuery
  | InsertQuery
  | RawQuery
  deriving (Eq)

-- TODO: should use a conversion class
--
sqlQueryTypeText :: SqlQueryType -> Text
sqlQueryTypeText = \case
  SelectQuery -> "SELECT"
  InsertQuery -> "INSERT"
  RawQuery    -> mempty

data SqlStatement = SqlStatement
  { sqlWhere     :: [RelationAction]
  , sqlInserts   :: [FieldValue]
  , sqlCreate    :: CreateSql
  , sqlRaw       :: RawStatement
  , sqlQueryType :: SqlQueryType
  }

newtype TypedSqlStatement = TypedSqlStatement Text
  deriving (Show, Eq)

newtype TypedQueryParams = TypedQueryParams { unTypedQueryAction :: Text }
  deriving (Show, Eq)
  deriving Semigroup via Text
  deriving Monoid via Text

-- TODO: maybe rename
newtype TypedSubQuery = TypedSubQuery { unTypedSubQuery :: Text }
  deriving (Show, Eq)
  deriving Semigroup via Text
  deriving Monoid via Text

-- | The sql we finally send to the DB
data TypedSqlQuery = TypedSqlQuery
  { sqStatement :: TypedSqlStatement
  , sqValues    :: [FieldValue]
  }

mkDefStatement :: SqlQueryType -> SqlStatement
mkDefStatement queryType
  = SqlStatement mempty mempty mempty mempty queryType
