-- | Translation between haskell types and data base types
module DataBase.PersistTypes where

data PersistTypes = PersistTypes -- TODO: do me

class ToPersistTypes (a :: Type) where
  toPersistTypes :: sing a -> PersistType


class FromPersistTypes (a :: PersistType) where
  fromPersistTypes :: forall (b :: Type). sing a -> b
