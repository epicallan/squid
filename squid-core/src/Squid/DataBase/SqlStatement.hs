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

fromRelationOp :: RelationOp -> Text
fromRelationOp = \case
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

data SqlStatement = SqlStatement
  { sqlWhere     :: [RelationAction]
  , sqlInserts   :: [FieldValue]
  , sqlCreate    :: CreateSql
  , sqlRaw       :: RawStatement
  , sqlQueryType :: SqlQueryType
  }

newtype TypedSqlStatement = TypedSqlStatement Text
  deriving (Eq)

newtype TypedQueryParams = TypedQueryParams Text
  deriving (Eq)
  deriving Semigroup via Text
  deriving Monoid via Text

-- TODO: maybe rename
newtype TypedSubQuery = TypedSubQuery Text
  deriving (Eq)
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
