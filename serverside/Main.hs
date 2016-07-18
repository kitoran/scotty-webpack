{-# LANGUAGE NoMonomorphismRestriction
           , OverloadedStrings
           , DeriveGeneric
           , ImplicitParams #-}

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString.Lazy
import Data.Text.Lazy.Encoding
import GHC.Generics

import Web.Scotty
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Data.Aeson
import System.Posix.Syslog

main = withSyslog defaultConfig (\log' -> let ?log = log' in server)

server :: (?log::SyslogFn) => IO ()
server = do
  scotty 3000 $ do
    get "/" $ do
      liftIO (print ("empty request"))
      Web.Scotty.file "../generated/index.html"    
    get "/:string" $ do
      string <- param "string"
      liftIO (print ("request " ++ string))
      a <- liftIO $ queryPhoneDatabase string
      Web.Scotty.json a
    notFound $ do
      liftIO (print ("404"))
      Web.Scotty.file "../generated/Main.js"    

queryPhoneDatabase :: (?log::SyslogFn) => String -> IO [PhonebookRecord]
queryPhoneDatabase string = do 
    conn <- connectPostgreSQL ""
    i <- query conn "select string, number from helloworldt where string = ?;" $ Only string
    when (Prelude.null i)
         (?log USER Warning "No such record in database")
    return i
    
data PhonebookRecord = PhonebookRecord { phonebookName :: String, phonebookNumber :: Int } deriving (Show, Eq, Generic)

instance ToJSON PhonebookRecord where

instance FromRow PhonebookRecord where
    fromRow = PhonebookRecord <$> field <*> field
