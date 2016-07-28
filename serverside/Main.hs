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
import Control.Monad.Reader

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

import Web.Scotty as S

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

instance ToJSON Calltbl where

ourConnectionInfo :: ConnectionString
ourConnectionInfo =  "host=localhost port=5432 user=n dbname=postgresdatabase password=postgresspassword"  

--queryRead :: (MonadReader Connection m, ToRow args, FromRow r, MonadIO m) => Query -> args -> m [r]
--queryRead q args = ask >>= \conn -> liftIO $ query conn q args

--queryRead_ :: (MonadReader Connection m, FromRow r, MonadIO m) => Query -> m [r]
--queryRead_ q = ask >>= \conn -> liftIO $ query_ conn q 

main :: IO ()
main = runStdoutLoggingT $ withPostgresqlConn ourConnectionInfo $ (\sqlBackend -> do
    liftIO $ scotty 3000 $ do 
      S.get "/" $ S.file "../index.html"
      S.get "/submit" $ (S.json =<< smth sqlBackend)
      S.post "/options" $ (S.text $ "{\"programs\":[\"program1\", \"program2\"], \"cities\":[\"Moscow\",\"New York\"], \"partners\":[\"partner1\"]}")
      S.get "/:file" $ do
           file <- param "file"
           liftIO $ print $ "file: " ++ file
           S.file ("../" ++ file)
      S.notFound $ (liftIO $ print "404") >> S.file "rks.js")

paramMaybe :: (Parsable a) => Text -> ActionM (Maybe a) 
paramMaybe s = (do d <- param s
                   return (Just d)) `rescue` const (return Nothing)


smth :: SqlBackend -> ActionM A.Value
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
  entityList <- runReaderT  callerQuery  sqlBackend
  return $ toJSON $  fmap (\(Entity a b) -> b) entityList
  