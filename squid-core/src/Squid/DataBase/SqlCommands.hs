{-# LANGUAGE DeriveFunctor #-}
module Squid.DataBase.SqlCommands where

import Control.Monad.Free

import Squid.DataBase.SqlStatement

data SqlF a next =
    Select next
  | Insert FieldValue next
  | Where RelationAction next
  | From next
  deriving (Functor)

type Sql a = Free (SqlF a)

select_ :: Sql a ()
select_ = liftF $ Select ()

from_ :: Sql a ()
from_ = liftF $ From ()

insert_ :: FieldValue  -> Sql a ()
insert_ x = liftF $ Insert x ()

where_ :: RelationAction -> Sql a ()
where_ x = liftF $ Where x ()
