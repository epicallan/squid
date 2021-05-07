module Main where

import Control.Monad (void)
import Control.Monad.IO.Class
import GHC.Generics hiding (from)

import Squid.Postgres

data Person = Person
  { name :: String
  , age  :: Maybe Int
  , sex  :: String
  } deriving stock (Show, Generic)
    deriving anyclass (FromRow, HasFieldValues)

instance HasEntity Person where
  type UniqueKeys Person = '[ "name" ]

data BlogPost = BlogPost
  { title  :: String
  , author :: String
  , school :: Maybe String
  } deriving stock (Show, Generic)
    deriving anyclass (FromRow, HasFieldValues)

instance HasEntity BlogPost where
  type UniqueKeys BlogPost = '[ "title" ]
  type ForeignKeys BlogPost = '[ "author" := ForeignKeyReferencedTable Person "name" ]

insertData :: SqlPersist m ()
insertData = void $ do
  _ <- insert Person
    { name = "Allan"
    , age = Just 30
    , sex = "male"
    }

  insert BlogPost
    { title = "Monads tutorial"
    , author = "Allan"
    , school = Just "KCB"
    }

getUsers :: SqlPersist m ()
getUsers = do
  people <- select @Person
              $ from
              $ \ person -> do
                    where_ (#sex person ==. "male")
                    where_ (#age person >. Just 25)

  liftIO $ mapM_ (putStrLn . name . entityVal) people

getUserBlogs :: SqlPersist m ()
getUserBlogs = do
  usersAndBlogs <- select @(Person, BlogPost)
              $ from
              $ \ (person, blog) -> do
                    where_ (#sex person ==. "male" )
                    where_ (#author blog ==. "Allan")

  liftIO $ mapM_ (putStrLn . author . entityVal . snd) usersAndBlogs

-- | should be provided by library where by sql string is an argument.
createConfig :: IO SqlConfig
createConfig = do
  conn  <- connectPostgreSQL "host=localhost port=5432 dbname=squid user=eval password=eval"
  return $ defaultConfig conn

migrateTables :: SqlPersist m ()
migrateTables = migrateAll @'[Person, BlogPost]

main :: IO ()
main = do
  config <- createConfig
  runDb config $ do
    migrateTables
    insertData
    getUsers
    getUserBlogs
