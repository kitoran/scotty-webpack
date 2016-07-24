{-# LANGUAGE NoMonomorphismRestriction
           , OverloadedStrings
           , DeriveGeneric
           , FlexibleContexts             #-}

import Control.Monad.Reader

import Data.Monoid
import Data.Text.Lazy
import Data.Aeson
import Data.String

import Control.Monad.Logger ( runStdoutLoggingT, MonadLogger, logDebug, LoggingT )


import Database.PostgreSQL.Simple as Psql

import Web.Scotty


ourConnectionInfo :: ConnectInfo
ourConnectionInfo = defaultConnectInfo  

queryRead :: (MonadReader Connection m, ToRow args, FromRow r, MonadIO m) => Query -> args -> m [r]
queryRead q args = ask >>= \conn -> liftIO $ query conn q args

queryRead_ :: (MonadReader Connection m, FromRow r, MonadIO m) => Query -> m [r]
queryRead_ q = ask >>= \conn -> liftIO $ query_ conn q 

type MyMonad a = LoggingT ((ReaderT Connection) ActionM) a

runMyMonad::Connection -> MyMonad a -> ActionM a
runMyMonad conn m = runReaderT (runStdoutLoggingT m) conn 

main = do 
    conn <- (connect ourConnectionInfo)
    scotty 3000 $ do 
      post "someRoute" $ runMyMonad conn smth

paramMaybe :: (Parsable a) => Text -> ActionM (Maybe a) 
paramMaybe s = (do d <- param s
                   return (Just d)) `rescue` const (return Nothing)

program = paramMaybe "program"
city = paramMaybe "city"

smth :: MyMonad ()
smth = do
  program' <- liftAction $ (fmap.fmap)  fromString program
  city' <- liftAction $ (fmap.fmap) fromString city
  let programString   = maybe "" (\s -> "and (program is not null) and (program = " <> s <> ")") (program'::Maybe Query)
  -- look out for sql-injections! Perhaps quote_literal(" <> s <> ")?
      cityString      =  maybe "" (\s -> "and (city is not null) and (city = " <> s <> ")") ( city'::Maybe Query)
      callerQuery, userCallerQuery, caseQuery::Query
      callerQuery     = "select callertype, calltype, count(*) from calltbl where"
                       <> "(calldate >= ?) and (calldate < ?) and (calldate is not null) " <> programString <>
                             cityString <>
                             " group by callertype, calltype order by callertype, calltype"
      userCallerQuery = mconcat [
                                  "select u.realName, count(*) from calltbl c, usermetatbl u where",
                                  " u.id = c.calltaker and",
                                  " (calldate >= ?) and (calldate < ?) and (calldate is not null) ",
                                  programString, cityString,
                                  " group by u.realName order by u.realName"]
      caseQuery       = mconcat [
                            "select u.realName, count(*) from casetbl c, usermetatbl u where",
                            " u.id = c.calltaker and",
                            " (calldate >= ?) and (calldate < ?) and (calldate is not null) , ",
                            programString,  cityString,
                            " group by u.realName order by u.realName"]
  calls <- queryRead_ callerQuery
  liftAction $ Web.Scotty.json (toJSON (calls::[[Text]]))
  where liftAction = lift . lift

