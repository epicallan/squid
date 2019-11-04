-- | Translation between haskell types and dataBase types
module DataBase.Internal.PersistTypes where


data SqlTypes = SqlTypes -- TODO: do me

class ToPersistTypes (a :: Type) where
  toPersistTypes :: sing a -> PersistType

-- class FromSqlType a where
--   fromPersistTypes :: a ->
