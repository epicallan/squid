module DataBase.RunSql where

data SqlEntity entityType primaryKey = SqlEntity
  { entityVal :: entityType
  , entityId  :: primaryKey
  }

class HasEntity entity => RunSql m entity where
  runSql :: SqlStatement -> m (SqlEntity entity (PrimaryKey entity))
