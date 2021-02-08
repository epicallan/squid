# Squid

[![Hackage](https://img.shields.io/hackage/v/squid.svg?logo=haskell)](https://hackage.haskell.org/package/squid)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Build status](https://img.shields.io/travis/epicallan/squid.svg?logo=travis)](https://travis-ci.org/epicallan/squid)

## Intro

Squid is a type safe EDSL for SQL thats aims to replicate the Persistent-Esqueleto API without the
use of Template Haskell.

Squid makes use of Generic programming and type level programming to provide a similar sql query
API to that of Persistent-Esqueleto. In a way Squid is very similar to Selda as far as general approach is concerned.

Squid works with GHC 8.8 and above.

## Motivation

Squid is built for those who love Persistent-Esqueleto but wish they had the following:

- A non TH way of specifying table entities.
- A namespace that's not littered with new compile generated Template Haskell generated types.
- A minimal and flexible API

## Comparison between Persistent-Esqueleto and Squid

### Persistent-Esqueleto

```haskell ignore
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
  Person
    name String
    age Int Maybe
    deriving Eq Show
  BlogPost
    title String
    authorId PersonId
    deriving Eq Show
  Follow
    follower PersonId
    followed PersonId
    deriving Eq Show
|]

-- | Generates SELECT * FROM Person
getPersons :: SqlPersist m ()
getPersons = do
  people <- select $
              from $ \person -> do
                where_ (p ^. PersonAge >=. just (val 18))
                return person
  liftIO $ mapM_ (putStrLn . personName . entityVal) people
```

### Squid

```haskell
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

import Control.Monad.IO.Class
import GHC.Generics hiding (from)

import Squid.Postgres

data Person = Person
  { name :: String
  , age  :: Maybe Int
  } deriving (Show, Generic, HasEntity, FromRow)

getPersons :: SqlPersist m ()
getPersons =  do
  people <- select @Person
               $ from
               $ \person -> do
                     where_ (#age person  >=. Just 20 )
  liftIO $ mapM_ (putStrLn . name . entityVal) people

createConfig :: IO SqlConfig
createConfig = do
  conn  <- connectPostgreSQL "host=localhost port=5432 dbname=squid user=allan"
  return $ defaultConfig { sqlConnection = Just conn }

main' :: IO ()
main' = do
  config <- createConfig
  runDb config getPersons
```

## Documentation

This README is tested by `markdown-unlit` to make sure the code builds. To keep _that_ happy, we do need a `main` in this file, so ignore the following :)

```haskell
main :: IO ()
main = pure ()
```
