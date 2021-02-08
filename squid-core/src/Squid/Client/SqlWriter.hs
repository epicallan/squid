-- | writes out literal textual sql thats eventually
-- run in the DB
--
module Squid.Client.SqlWriter where

import Squid.Prelude

import Squid.DataBase
import qualified Data.Text as Text
import qualified Data.List.NonEmpty as NE

sqlWriter
  :: NonEmpty TableDefinition
  -> SqlStatement
  -> TypedSqlQuery
sqlWriter tableDefinitions stmt
  = TypedSqlQuery queryStatement parameterValues
  where
    parameterValues = getParamValues stmt

    queryStatement = mkQueryStatement
      (getSubQuery tableDefinitions stmt)
      (getQueryParams tableDefinitions stmt)

-- | Note for select statements we are doing simple relations
-- whose values are carried within RelationAction
--
getParamValues :: SqlStatement -> [FieldValue]
getParamValues SqlStatement {..} = case sqlQueryType of
  InsertQuery -> sqlInserts
  _           -> []

rawWriter
  :: Proxy 'RawQuery
  -> SqlStatement
  -> TypedSqlQuery
rawWriter _ SqlStatement{..} = case sqlQueryType of
   RawQuery -> TypedSqlQuery (TypedSqlStatement (unRawStatement sqlRaw)) []
   _        -> error "Error: Tried to use rawWriter for non-raw sql"

getQueryParams
  :: NonEmpty TableDefinition
  -> SqlStatement
  -> TypedQueryParams
getQueryParams tables statement@SqlStatement{..} = case sqlQueryType of
  SelectQuery -> selectQueryParams statement
  --  TODO: throw when more than one table for insert
  InsertQuery -> insertValueParams (NE.head tables)
  RawQuery    -> TypedQueryParams mempty

insertValueParams
  :: TableDefinition
  -> TypedQueryParams
insertValueParams TableDefinition {..}
  = TypedQueryParams $ " VALUES (" <> insertAction <> ") "
  where
    insertAction = Text.concat $ "," <$ NE.toList tableColumns

selectQueryParams
  :: SqlStatement
  -> TypedQueryParams
selectQueryParams SqlStatement {..} = TypedQueryParams $ " WHERE " <> selectAction
  where
    selectAction :: Text
    selectAction = Text.intercalate " AND " $ extractParams <$> sqlWhere

    extractParams :: RelationAction -> Text
    extractParams (RelationAction op fieldName _fieldValue)
      = fieldName <> " " <> relationOpSymbol  op <> " ? "

getSubQuery
  :: NonEmpty TableDefinition
  -> SqlStatement
  -> TypedSubQuery
getSubQuery tables SqlStatement {..} = case sqlQueryType of
    SelectQuery -> selectSubQuery tables
    -- TODO: Throw error for when there is more than one table
    -- for an insert statement
    InsertQuery -> insertSubQuery (NE.head tables)
    RawQuery    -> TypedSubQuery $ unRawStatement sqlRaw

selectSubQuery
  :: NonEmpty TableDefinition
  -> TypedSubQuery
selectSubQuery tableDefinitions
  = TypedSubQuery $ "SELECT " <> tablesColumnString  <> " FROM " <> tables
  where
    tables
      = Text.intercalate ","
      . NE.toList
      $ tableName <$> tableDefinitions

    tablesColumnString :: Text
    tablesColumnString
      = Text.intercalate ","
      . NE.toList
      $ tableColumnString <$> tableDefinitions

insertSubQuery :: TableDefinition -> TypedSubQuery
insertSubQuery table@TableDefinition {..}
  = TypedSubQuery
  $ "INSERT INTO " <> tableName <> " (" <> tableColumnString table <> ")"

tableColumnString :: TableDefinition -> Text
tableColumnString TableDefinition{..} = Text.intercalate "," columnsWithTable
  where
    columnsWithTable :: [Text]
    columnsWithTable = NE.toList $ fmap (columnWithTable tableName) tableColumns

    columnWithTable :: SqlTableName -> SqlColumn -> Text
    columnWithTable tableName' column = tableName' <> "." <> colName column

mkQueryStatement
  :: TypedSubQuery
  -> TypedQueryParams
  -> TypedSqlStatement
mkQueryStatement (TypedSubQuery x) (TypedQueryParams y)
  = TypedSqlStatement $ x <> y
