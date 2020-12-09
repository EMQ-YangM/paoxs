{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Scheam1 where


import Data.Avro
import Data.Avro.Deriving
import Data.Avro.Encoding.ToAvro
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.Avro.Encoding.FromAvro
import Data.Avro.Schema.ReadSchema
import Data.Aeson (eitherDecode)
import Text.Show.Pretty (pPrint)
import Data.Time
import qualified Data.Text as T
import qualified Data.Map as M
import GHC.Conc.IO
import GHC.Conc
import Control.Monad (guard, join)
import Data.Time
import Control.Exception

deriveAvroFromByteString [r|
{
    "namespace": "com.github.saint1991.serialization.benchmark.avro",
    "name":"Nobid",
    "type":"record",
    "fields":[
        {"name":"adnwId","type":"long","logicalType": "timestamp-micros"},
        {"name":"appName","type":"string"},
        {"name":"auctionId","type":"string"},
        {"name":"host","type":"string"},
        {"name":"loggedAt","type":"string", "default": ""},
        {"name":"mId","type":"int"},
        {"name":"nbr","type":"int"},
        {"name":"page","type":["null", "string"], "default": null},
        {"name":"resTime","type":"int"},
        {"name":"spot","type": {
            "name": "spotRecord",
            "type": "record",
            "fields": [
                {"name": "id", "type": "int"},
                {"name": "name", "type": "string"}
            ]
       }},
        {"name": "history", "type": {
            "name": "historyItems",
            "type": "array",
            "items": {
                "name": "historyItem",
                "type": "string"
            }
        }},
        {"name": "tags", "type": {
            "name": "tag",
            "type": "map",
            "values": "string"
        }}
    ]
} 
|]


t = toAvro schema'Nobid (Nobid 23
                               "nice"
                               "baba"
                               "abadfadsbaasdf"
                               ""
                               2323
                               232
                               (Just "baba")
                               232
                               (SpotRecord 232 "ababa")
                               ["ncie", "baba"]
                               (M.fromList [("baba", "dcddd"),("ba23ba","ababaaa")])
                        )
e = toLazyByteString t



myschema :: IO Schema
myschema = do --eitherDecode  schSource :: Either String Schema
    f <- BL.readFile "person.avsc"
    case eitherDecode f :: Either String Schema of 
        Left e -> error e
        Right v -> return v


myDecode1 :: ReadSchema -> BL.ByteString -> [Either String Value]
myDecode1 schema payload =
  case runGetOrFail (getValue schema) payload of
    Left (bs, _, e)  -> if bs == BL.empty then []  else [Left e]
    Right (bs, _, v) -> Right v : myDecode1 schema bs 

rn = do
    v <- myschema    
    let s = myDecode1 (fromSchema v) e  
    pPrint s


wtest :: IO ()
wtest = do 
    let tv = BL.fromChunks $ replicate 50000 $ BL.toStrict e
    BL.writeFile "testvalue" tv

isEither :: Either a b -> Bool
isEither (Right _ ) = True
isEither _ = False

testt :: IO ()
testt = do 
    t0 <- getCurrentTime
    v <- myschema    
    f <- BL.readFile "testvalue"
--  print $ BL.length f

    t1 <- getCurrentTime 
    print $ diffUTCTime t1 t0


    t0 <- getCurrentTime
    let res = myDecode1 (fromSchema v) f
    print $ length $ map isEither res

    t1 <- getCurrentTime 
    print $ diffUTCTime t1 t0


tfun1 :: IO () 
tfun1 = do 
    t1 <- getCurrentTime
    b <- registerDelay 1000000
    join $ atomically  $ do 
               v' <- readTVar b
               guard v'
               return (print v')
    t2 <- getCurrentTime
    print $ diffUTCTime t2 t1
    tfun1

f = bracket (return $ error "ncie") (\_ -> print "error") (\_ -> print "ok")













































