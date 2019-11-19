module Squid.Client.RunClient where

import Data.Proxy

import Squid.DataBase

type SqlResults a = [Entity a]

class Monad m => RunClient entity m where
  runClient
    :: HasEntity entity
    => Proxy entity -> Sql entity () -> m (SqlResults entity)
