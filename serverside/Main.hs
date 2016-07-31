{-# LANGUAGE NoMonomorphismRestriction
           , OverloadedStrings
           , DeriveGeneric
           , FlexibleContexts             #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DataKinds                  #-}
import Control.Monad.Reader
import Prelude as P
import Data.Monoid
import Data.Text.Lazy
import Data.Aeson as A
import Data.Time.Calendar
import Data.String

import GHC.Generics

import Database.Esqueleto as E ()
import Control.Monad.Logger ( runStdoutLoggingT, MonadLogger, logDebug, LoggingT )

import           Database.Persist.Postgresql
import           Database.Persist.TH

import qualified Web.Scotty as S

mkPersist sqlSettings [persistLowerCase|
Calltbl
    callertype String 
    calltype String
    calldate Day Maybe
    program String Maybe
    city String Maybe
    Primary callertype calltype 
    deriving Show Read Generic
|]
data Options = Options { programs :: [Maybe String],
                         cities :: [Maybe String],
                         partners :: [String] } deriving (Show, Generic)
instance ToJSON Options where

instance ToJSON Calltbl where

ourConnectionInfo :: ConnectionString
ourConnectionInfo =  "host=localhost port=5432 user=n dbname=postgresdatabase password=postgresspassword"  

--queryRead :: (MonadReader Connection m, ToRow args, FromRow r, MonadIO m) => Query -> args -> m [r]
--queryRead q args = ask >>= \conn -> liftIO $ query conn q args

--queryRead_ :: (MonadReader Connection m, FromRow r, MonadIO m) => Query -> m [r]
--queryRead_ q = ask >>= \conn -> liftIO $ query_ conn q 

main :: IO ()
main = runStdoutLoggingT $ withPostgresqlConn ourConnectionInfo $ (\sqlBackend ->  do
    lift $ S.scotty 3000 $ do 
      S.get "/" $ S.file "../index.html"
      S.get "/submit" $ (S.json =<< smth sqlBackend )
      S.post "/options" $ (S.json =<< options sqlBackend)
      S.get "/:file" $ do
           file <- S.param "file"
           liftIO $ print $ "file: " ++ file
           S.file ("../" ++ file)
      S.notFound $ (liftIO $ print "404") >> S.file "rks.js")

paramMaybe :: (S.Parsable a) => Text -> S.ActionM (Maybe a) 
paramMaybe s = (do d <- S.param s
                   return (Just d)) `S.rescue` const (return Nothing)

options :: SqlBackend -> S.ActionM A.Value
options sqlBackend = do
    allOfCalltbl' <- runReaderT (selectList [] []:: ReaderT SqlBackend S.ActionM [Entity Calltbl]) sqlBackend--"{\"programs\":[\"program1\", \"program2\"], \"cities\":[\"Moscow\",\"New York\"], \"partners\":[\"partner1\"]}")
    let allOfCalltbl = P.map (\(Entity a b) -> b) allOfCalltbl' -- ::_)
    liftIO (print$ unzipCalltbl allOfCalltbl)
    return $ toJSON $ unzipCalltbl allOfCalltbl
    where unzipCalltbl = P.foldr (\(Calltbl crt ct cd cp cc) ~(Options cps ccs crts) -> Options (cp:cps) (cc:ccs) (crt:crts))
                  (Options [] [] [])

smth :: SqlBackend -> S.ActionM A.Value
smth sqlBackend = do
  program' <- paramMaybe "program"
  city' <- paramMaybe "city"
  let fromDate = fromGregorian 2000 03 13 
      toDate = fromGregorian 2020 02 23
      programFilter   = maybe [] (\s -> [CalltblProgram ==. Just s]) (program'::Maybe String)
      cityString   = maybe [] (\s -> [CalltblCity ==. Just s]) (city'::Maybe String)
      --callerQuery, userCallerQuery, caseQuery::Query
      callerQuery     = selectList ([CalltblCalldate >=. Just fromDate,
                                     CalltblCalldate <. Just toDate,
                                     CalltblCalldate !=. Nothing] 
                                 ++ programFilter) [Asc CalltblCallertype,
                                                    Asc CalltblCalltype]
  entityList <- runReaderT callerQuery sqlBackend
  return $ toJSON $  fmap (\(Entity a b) -> b) entityList
  