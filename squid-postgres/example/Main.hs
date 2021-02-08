module Main where

import Control.Monad.IO.Class
import GHC.Generics hiding (from)

import Squid.Postgres

data User = User
  { name :: String
  , age  :: Maybe Int
  , sex  :: String
  } deriving stock (Show, Generic)
    deriving anyclass (FromRow)

instance HasEntity User where
  type UniqueKeys User = '[ "name" ]

data BlogPost = BlogPost
  { title  :: String
  , author :: String
  , school :: Maybe String
  } deriving stock (Show, Generic)
    deriving anyclass (FromRow)

instance HasEntity BlogPost where
  type UniqueKeys BlogPost = '[ "title" ]
  type ForeignKeys BlogPost = '[ "author" := ForeignKeyReferencedTable User "name" ]

getUsers :: SqlPersist m ()
getUsers = do
  people <- select @User
              $ from
              $ \ person -> do
                    where_ (#sex person ==. "male")
                    where_ (#age person >. Just 25)

  liftIO $ mapM_ (putStrLn . name . entityVal) people

getUserBlogs :: SqlPersist m ()
getUserBlogs = do
  usersAndBlogs <- select @(User, BlogPost)
              $ from
              $ \ (person, blog) -> do
                    where_ (#sex person ==. "male" )
                    where_ (#author blog ==. "Allan")

  liftIO $ mapM_ (putStrLn . author . entityVal . snd) usersAndBlogs

-- | should be provided by library where by sql string is an argument.
createConfig :: IO SqlConfig
createConfig = do
  conn  <- connectPostgreSQL "host=localhost port=5432 dbname=squida user=allan"
  return $ defaultConfig { sqlConnection = Just conn }

migrateTables :: SqlPersist m ()
migrateTables = migrateAll @'[User, BlogPost]

main :: IO ()
main = do
  config <- createConfig
  runDb config $ do
    migrateTables
    getUsers
    getUserBlogs
