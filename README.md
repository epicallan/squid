# Squid

[![Hackage](https://img.shields.io/hackage/v/squid.svg?logo=haskell)](https://hackage.haskell.org/package/squid)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Build status](https://github.com/epicallan/squid/workflows/setup-haskell/badge.svg)
[![Build status](https://img.shields.io/travis/epicallan/squid.svg?logo=travis)](https://travis-ci.org/epicallan/squid)


## Intro

Squid is a type safe EDSL for SQL thats aims to replicate the Persistent-Esqueleto API without the
use of Template Haskell.

Squid makes use of Generic programming and type level programming to provide a similar sql query
API to that of Persistent-Esqueleto. In a way Squid is very similar to Selda as far as general approach is concerned.

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
              return person
  liftIO $ mapM_ (putStrLn . personName . entityVal) people

getPerson :: SqlPersist m Person
getPerson = do
  select $
    from $ \p -> do
    where_ (p ^. PersonAge >=. just (val 18))
    return p


```

### Squid

```haskell
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies  #-}

import Control.Monad.IO.Class
import GHC.Generics hiding (from)

import DataBase.Squid

data User = User
  { name :: String
  , age  :: Int
  , sex  :: String
  } deriving (Show, Eq, Generic, HasEntity)

data BlogPost = BlogPost
  { title  :: String
  , author :: Int
  } deriving (Eq, Show, Generic)

-- | writing instance explicitely to represent foreign key constraint
instance HasEntity BlogPost where
  type ForeignKeys BlogPost = '[ '("author", User) ]

-- | Examples

--- Initial style
getBlogPosts :: MonadIO m => SqlMock m [BlogPost]
getBlogPosts = do
  blogPosts <- select $ from $ \ p -> where_ ( p ^. Field @"title" ==. "Book")
  return $ entityVal <$> blogPosts

-- | final style
getPeople :: (MonadIO m, MonadSql m User) => m ()
getPeople =  do
  people <- select
               $ from
               $ \ person -> where_ ( person ^. Field @"name" ==. "Allan")
  liftIO $ mapM_ (putStrLn . name . entityVal) people

main' :: IO ()
main' = runMockDbIO defaultMockConfig $ do
  blogPosts <- getBlogPosts
  liftIO $ print blogPosts
  getPeople
```

## Documentation

This README is tested by `markdown-unlit` to make sure the code builds. To keep _that_ happy, we do need a `main` in this file, so ignore the following :)

```haskell
main :: IO ()
main = pure ()
```
