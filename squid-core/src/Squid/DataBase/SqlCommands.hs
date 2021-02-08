{-# LANGUAGE DeriveFunctor #-}
module Squid.DataBase.SqlCommands where

import Control.Monad.Free

import Squid.DataBase.HasFieldValues
import Squid.DataBase.SqlStatement

data SqlF entity next =
    Select next
  | Insert [FieldValue] next
  | Where RelationAction next
  | Raw RawStatement next
  -- | On JoinAction -- joinAction can aswell have the table names
  --  ( (TableName, TableField), (TableName, TableField) , RelationOp, JoinType)
  -- From has [tableNames] -- but we are already get tableNames. implicitely.
  | From next -- should have table names
  deriving (Functor)

type Sql entity = Free (SqlF entity)

select_ :: Sql entity ()
select_ = liftF $ Select ()

from_ :: Sql entity ()
from_ = liftF $ From ()

insert_ :: [FieldValue] -> Sql entity ()
insert_ x = liftF $ Insert x ()

raw_ :: RawStatement -> Sql entity ()
raw_ x = liftF $ Raw x ()

where_ ::  RelationAction -> Sql entity ()
where_ x = liftF $ Where x ()

-- | NOTE: Table creation / migration should set primary key
-- to auto increment when the default Primary Key is used.
-- When people use their own primary key we do nothing.
