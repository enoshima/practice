{-# LANGUAGE MonadComprehensions, ScopedTypeVariables #-}

module Function
( haskalDataFind
, haskalDataPoint
)
where

import Database.Relational.Query ( relationalQuery
                                 , query
                                 , relation
                                 , wheres
                                 , not'
                                 , and'
                                 , or'
                                 , value
                                 , Relation
                                 , (.>=.)
                                 , (.<=.)
                                 , (.=.)
                                 , (!)
                                 )
import Database.HDBC.Session     (withConnectionIO, handleSqlError')
import Database.HDBC.Record.Query (runQuery')
import Database.HDBC              (quickQuery', runRaw, fromSql)

import Data.Int                   (Int32)
import Data.List                  (isInfixOf)

import DataSource                 (connect)
import Framedb                    (framedb)
import qualified Framedb as Frame


haskalDataFind :: Int32 -> Int32 -> String -> IO [String]
haskalDataFind gpsstrt duration chname =
  handleSqlError' $ withConnectionIO connect $ \conn -> do
  setSqlMode conn
  outputResults conn core
  where
    outputResults c q = runQuery' c (relationalQuery q) ()

    gpsend = gpsstrt + duration

    channel = relation
      [ u
      | u <- query framedb
      , () <- wheres $ u ! Frame.chname' .=. value chname
      ]

    core :: Relation () String
    core = relation $ do
      ch <- query channel
      wheres $ not' ((ch ! Frame.gpsStart' .<=. value gpsstrt
        `and'` ch ! Frame.gpsStop'  .<=. value gpsstrt)
        `or'` (ch ! Frame.gpsStart' .>=. value gpsend
        `and'` ch ! Frame.gpsStop'  .>=. value gpsend))
      return $ ch ! Frame.fname'


haskalDataPoint :: Int32 -> String -> IO [String]
haskalDataPoint gpstime chname =
  handleSqlError' $ withConnectionIO connect $ \conn -> do
  setSqlMode conn
  outputResults conn core
  where
    outputResults c q = runQuery' c (relationalQuery q) ()

    channel = relation
      [ u
      | u <- query framedb
      , () <- wheres $ u ! Frame.chname' .=. value chname
      ]

    core :: Relation () String
    core = relation $ do
      ch <- query channel
      wheres $ ch ! Frame.gpsStart' .<=. value gpstime
      wheres $ ch ! Frame.gpsStop'  .>=. value gpstime
      return $ ch ! Frame.fname'


setSqlMode conn = do
  mode <- quickQuery' conn "SELECT @@SESSION.sql_mode" []
  newmode <- case mode of
    [[sqlval]] ->
      let val = fromSql sqlval in
        if "IGNORE_SPACE" `isInfixOf` val
          then return val
          else return $ val ++ ", IGNORE_SPACE"
    _          ->
      error "failed to get 'sql_mode'"
  runRaw conn $ "SET SESSION sql_mode = '" ++ newmode ++ "'"

