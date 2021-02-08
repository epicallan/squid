module Squid.Postgres.Field where

import Data.ByteString.Builder ( toLazyByteString )
import Data.ByteString.Lazy (toStrict)
import Data.Text ( Text )
import Data.Text.Encoding ( decodeUtf8 )

import Squid.Client

import qualified Database.PostgreSQL.Simple.ToField as PG

newtype SqlFieldWrapper = SqlFieldWrapper SqlValue

type FieldType = Text -- TODO: use newtype

instance PG.ToField SqlFieldWrapper where
  toField (SqlFieldWrapper x) = case x of --  TODO: add more cases
    Plain b  -> PG.Plain b
    Escape b -> PG.Escape b
    Null     -> PG.Escape "Null"

toSqlAction :: FieldValue -> PG.Action
toSqlAction = \case
  FieldValue x -> PG.toField . SqlFieldWrapper $ toSqlValue x

-- | Note: We are not considering character sets for now
-- we are assuming UTF-8. This is primary used for parsing
-- provided default values in the 'HasEntity' class into textual values.
sqlFieldValueText :: SqlValue -> Text
sqlFieldValueText = \case
  Plain b  -> wrapText $ decodeUtf8 $ toStrict $ toLazyByteString b
  --  should have a wrapping function. WrapText is not working here
  Escape b -> decodeUtf8 b
  Null     -> "NULL"

wrapText :: Text -> Text
wrapText x = "'" <> x <> "'"

fieldType :: SqlType -> FieldType
fieldType = \case
  SqlText    -> "text"
  SqlInt     -> "integer"
  fieldType' -> error $ " field type to sqlType conversion missing for " <> show fieldType'
