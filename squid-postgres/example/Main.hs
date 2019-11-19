{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Monad.IO.Class
import GHC.Generics hiding (from)

import Squid.Postgres

data Person = Person
  { name :: String
  , age  :: Int
  , sex  :: String
  } deriving (Show, Generic, HasEntity, FromRow)

data BlogPost = BlogPost
  { title  :: String
  , author :: String
  } deriving (Show, Generic, HasEntity, FromRow)

-- * Examples

blogTable :: Table (TableFields BlogPost)
blogTable = undefined

ex1 :: SqlPersist m [BlogPost]
ex1 = do
  blogPosts <- select $ from $ \blog -> where_ (#author blog ==. "allan")
  liftIO $ print blogPosts
  return $ entityVal <$> blogPosts

-- | final style example
ex3 :: MonadDb m BlogPost => m ()
ex3 = do
  blogPosts <- select @BlogPost $ from $ \blog -> where_ (#title blog ==. "book")
  liftIO $ print blogPosts

getPersons :: SqlPersist m ()
getPersons =  do
  people <- select
               $ from
               $ \ person -> do
                      where_ (#sex person ==. "male" )
                      where_ (#age person >. 25 )
  liftIO $ mapM_ (putStrLn . name . entityVal) people

createConfig :: IO SqlConfig
createConfig = do
  conn  <- connectPostgreSQL "host=localhost port=5432 dbname=squid user=allan"
  return $ defaultConfig { sqlConnection = Just conn }

main :: IO ()
main = do
  config <- createConfig
  runDb config $ do
    getPersons
