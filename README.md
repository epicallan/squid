# Squid

[![Hackage](https://img.shields.io/hackage/v/squid.svg?logo=haskell)](https://hackage.haskell.org/package/squid)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Build status](https://img.shields.io/travis/epicallan/squid.svg?logo=travis)](https://travis-ci.org/epicallan/squid)

## Intro

Squid is a type safe EDSL for SQL thats aims to replicate the Persistent-Esqueleto API without the
use of template haskell.

Squid makes use of Generic programming and type level programming to provide a similar user facing
API to that of Persistent - Esqueleto.

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

```haskell ignore

  type instance PersistDB = "ExampleDb"

  -- | Primary keys are automatically created just like in Persistent.
  data PersonEntity f = PersonEntity
    { personName :: f String
    , personAge :: f (Maybe Int)
    } deriving (Eq, Show, Generic, PersistEntity "ExampleDb")

  type Person = PersonEntity Identity

  data BlogPostEntity f = BlogPostEntity
    { blogPostTitle :: f (Unique String) -- unique is a type family
    , blogPostauthorId :: f (EntityId Person) -- type family -- foreign Key constraint
    } deriving (Eq, Show, Generic, PersistEntity "ExampleDb")

  type BlogPost = BlogPostEntity Identity

  data FollowEntity f = FollowEntity
    { followFollower :: f (EntityId Person)
    , followFollowed :: f (EntityId Person)
    } (Eq, Show, Generic, PersistEntity "ExampleDb")

  type Follow = FollowEntity Identity

  -- | Generates SELECT * FROM Person
  getPersons :: SqlPersist m ()
  getPersons = do
    people <- select $
                from $ \person -> do
                return person
    liftIO $ mapM_ (putStrLn . personName . entityVal) people

-- | SELECT *
-- FROM Person
-- WHERE Person.age >= 18
getPerson :: SqlPersist m Person
getPerson = do
  select $
    from $ \p -> do
    where_ (p ^. field "personAge" >=. just (val 18))
    return p

```

## Documentation

This README is tested by `markdown-unlit` to make sure the code builds. To keep _that_ happy, we do need a `main` in this file, so ignore the following :)

```haskell
main :: IO ()
main = pure ()
```
