{-# LANGUAGE DeriveFunctor #-}
module Squid.DataBase.SqlCommands where

import Control.Monad.Free ( liftF, Free )

import Squid.DataBase.HasFieldValues ( FieldValue )
import Squid.DataBase.SqlStatement ( RelationAction, RawStatement )

data SqlF entity next =
    Select next
  | Insert [FieldValue] next
  | Where RelationAction next
  | Raw RawStatement next
  | From next
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
