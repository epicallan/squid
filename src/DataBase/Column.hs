module DataBase.Column where

import Data.Kind (Type)
import GHC.TypeLits

data TableField (a :: (Symbol, Type)) = TableField

type FieldName = String
