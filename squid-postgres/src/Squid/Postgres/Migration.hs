module Squid.Postgres.Migration where

import Data.List (find)

import Squid.Prelude
import Squid.Client
import Squid.Postgres.Field

import qualified Data.List.NonEmpty as NE
import qualified Data.Text          as Text

migrateAll
  :: forall (ts :: [Type]) m. (RunClient ts 'ExecuteVoid m, HasMigration ts)
  => m (SqlResults ts 'ExecuteVoid)
migrateAll = migrateAll_ @ts $ migrationSql @ts

migrationSql :: forall ts . HasMigration ts => RawStatement
migrationSql = foldMap createSql (migration @ts)

createSql :: TableDefinition -> RawStatement
createSql TableDefinition {..} = RawStatement createTableSql
  where
    colsSql = Text.intercalate " , " $ NE.toList $ toColumnsSql <$> tableColumns

    toColumnsSql SqlColumn{..} = columnSql colName (fieldType colType) tableName colAttrs

    createTableSql :: Text
    createTableSql =
      "CREATE TABLE IF NOT EXISTS " <> tableName <> " ( " <> colsSql <> " ) ;"

-- | This can be from a type class in core.
constraintSql :: ColumnName -> SqlTableName -> Attribute -> Maybe Text
constraintSql field table attr =  case attr of
  Unique         -> Just sqlPrefix
  ForeignKey ref -> Just $ sqlPrefix <> foreignKeyRef ref
  _              -> Nothing
  where
    sqlPrefix = "CONSTRAINT " <> consName <> " " <> attrName <> " " <> fieldWrapper

    attrName = attributeName attr

    consName = attrPrefix <> "_" <> table <> "_" <> field

    attrPrefix =  fst $ Text.breakOn " " $ Text.toLower attrName

    fieldWrapper = "(" <> field <> ")"

    foreignKeyRef :: Reference -> Text
    foreignKeyRef Reference {..} =
        " REFERENCES " <> refTable <> " (" <> refColumn <> ") NOT VALID"

isPrimaryKey :: [Attribute] -> Bool
isPrimaryKey = elem Primary

primaryColSql :: ColumnName -> FieldType -> [Attribute] -> Text
primaryColSql fieldName fieldType' attrs = fieldName <> " " <> addFieldType <> attrStr
  where
    hasSerialAttr :: Bool
    hasSerialAttr = AutoIncrement `elem` attrs

    attrStr :: Text
    attrStr = (Text.append " " . attributeName) `foldMap` attrs

    addFieldType = if hasSerialAttr then mempty else fieldType'

regularColSql
  :: ColumnName
  -> FieldType
  -> SqlTableName
  -> [Attribute]
  -> Text
regularColSql fieldName fieldType' table attrs = mkFieldSql <> allConstraints
  where
    mkFieldSql = if NotNull `elem` attrs
      then fieldSql <> " NOT NULL " else fieldSql

    fieldSql = fieldName <> " " <> fieldType'

    allConstraints = addConstraints <> addDefConstraint

    addConstraints = if not (Text.null regularConstraints)
      then " , " <> regularConstraints else mempty

    addDefConstraint = maybe mempty defConstraint (getDefAttr attrs)

    regularConstraints = Text.intercalate " , "
      $ catMaybes
      $ constraintSql fieldName table <$> attrs

    getDefAttr :: [Attribute] -> Maybe Attribute
    getDefAttr = find (\x -> attributeName x == "DEFAULT")

    defConstraint :: Attribute -> Text
    defConstraint = \case
      Default defaultField -> " DEFAULT " <> wrapText (sqlFieldValueText defaultField)
      -- TODO: wrapText should n't be here
      _              -> mempty

columnSql
  :: ColumnName
  -> FieldType
  -> SqlTableName
  -> [Attribute]
  -> Text
columnSql fieldName fieldType' table attrs = if isPrimaryKey attrs
  then primaryColSql fieldName fieldType' attrs
  else regularColSql fieldName fieldType' table attrs
