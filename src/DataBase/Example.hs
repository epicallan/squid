{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
module DataBase.Example where

import Control.Monad.IO.Class
import GHC.Generics hiding (from)

import DataBase.Internal
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
ex1 :: MonadIO m => SqlMock m [BlogPost]
ex1 = do
  blogPosts <- select $ from $ \ p -> where_ ( p ^. Field @"title" ==. "Book")
  return $ entityVal <$> blogPosts

-- | final style
ex2 :: (MonadIO m, MonadSql m User) => m ()
ex2 =  do
  people <- select
               $ from
               $ \ person -> where_ ( person ^. Field @"name" ==. "Allan")
  liftIO $ mapM_ (putStrLn . name . entityVal) people

main :: IO ()
main = runMockDbIO defaultMockConfig $ do
  blogPosts <- ex1
  liftIO $ print blogPosts
  ex2
