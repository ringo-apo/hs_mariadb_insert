{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Control.Exception
import Database.MySQL.Base
import qualified System.IO.Streams as Streams
import Data.Text.Internal (Text(..), safe, text)

transactional :: MySQLConn -> IO a -> IO a
transactional conn procedure = mask $ \restore -> do
  execute_ conn "BEGIN"
  a <- restore procedure `onException` (execute_ conn "ROLLBACK")
  execute_ conn "COMMIT"
  pure a

main :: IO ()
main = do
     someFunc

     word <- getLine
     let dtword = read ("\"" ++ word ++ "\"") :: Data.Text.Internal.Text

     conn <- connect
        defaultConnectInfo {ciUser = "root", ciPassword = "Password1234!", ciDatabase = "test"}

     stmt <- prepareStmt conn "insert into test (comment) values (?)"

     transactional conn $ do
       executeStmt conn stmt [MySQLText dtword]

       (defs, is) <- query_ conn "SELECT * FROM test"
       mapM_ print =<< Streams.toList is
