{-# LANGUAGE MonadComprehensions, ScopedTypeVariables #-}

module DBFunction
( extractTrigInfoTSNR
)
where


import Control.Monad
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT(..))
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
                                 , (|*|)
                                 , (|$|)
                                 )
import Database.HDBC.Session     (withConnectionIO, handleSqlError')
import Database.HDBC.Record.Query (runQuery')
import Database.HDBC              (quickQuery', runRaw, fromSql)

import Data.Int                   (Int32)
import Data.List                  (isInfixOf)
import Data.Maybe                 (fromJust, fromMaybe)
import qualified Data.Packed.Vector as DPV
import qualified Data.Traversable as DT

import HasKAL.DataBaseUtils.DataSource (connect)
import Burstdb (burstdb)
import qualified Burstdb as Burst



extractTrigInfoTSNR :: Int -> Int -> Double -> Double -> IO (Maybe[(Double, Double)])
extractTrigInfoTSNR gpsstart gpsstop snrlow snrhigh = runMaybeT $ MaybeT $ do
  let gpsstart' = fromIntegral gpsstart :: Int32
      gpsstop'  = fromIntegral gpsstop :: Int32
  items <- extractTrigInfoTSNRCore gpsstart' gpsstop' snrlow snrhigh
  let out = [(fromIntegral u :: Double, v)
            | (Just u, Just v) <- items
            ]
  case out of
    [] -> return Nothing
    x  -> return (Just x)


extractTrigInfoTSNRCore :: Int32 -> Int32 -> Double -> Double -> IO [(Maybe Int32, Maybe Double)]
extractTrigInfoTSNRCore gpsstart gpsstop snrlow snrhigh =
  handleSqlError' $ withConnectionIO connect $ \conn ->
  outputResults conn core
  where
    outputResults c q = runQuery' c (relationalQuery q) ()
    core :: Relation () ((Maybe Int32, Maybe Double))
    core = relation $ do
      db <- query burstdb
      wheres $ db ! Burst.eventGpsstarts' .>=. value (Just gpsstart)
        `and'` db ! Burst.eventGpsstarts' .<=. value (Just gpsstop)
        `and'` db ! Burst.snr' .>=. value (Just snrlow)
        `and'` db ! Burst.snr' .<=. value (Just snrhigh)
      return $ (,) |$| db ! Burst.eventGpsstarts' |*| db ! Burst.snr'





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





