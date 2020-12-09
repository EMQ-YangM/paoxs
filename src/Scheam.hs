{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Scheam where


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

deriveAvroFromByteString [r|
{
  "name": "Person",
  "type": "record",
  "fields": [
    { "name": "fullName", "type": "string" },
    { "name": "age", "type": "int" },
    { "name": "ssn", "type": ["null", "string"] },
    { "name": "gender",
      "type": { "name": "enumq", "type": "enum", "symbols": ["Male", "Female"] }
    },
    { "name": "sbn", "type": ["null", "string"] }
  ]
}
|]


schSource :: BL.ByteString
schSource = "\"name\": \"Person\""

myschema :: IO Schema
myschema = do --eitherDecode  schSource :: Either String Schema
    f <- BL.readFile "person.schema"
    case eitherDecode f :: Either String Schema of 
        Left e -> error e
        Right v -> return v

t = toAvro schema'Person (Person "ncie" 12 (Just (T.pack $ replicate 82000 'c')) EnumqMale (Just "----"))

e = toLazyByteString t

-- rs = fromSchema myschema

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
    let tv = BL.fromChunks $ replicate (1024 * 2) $ BL.toStrict e
    BL.writeFile "testvalue" tv

isEither :: Either a b -> Bool
isEither (Right _ ) = True
isEither _ = False

testt :: IO ()
testt = do 
    t0 <- getCurrentTime
    v <- myschema    
    f <- BL.readFile "testvalue"
    print $ BL.length f

    t1 <- getCurrentTime 
    print $ diffUTCTime t1 t0


    t0 <- getCurrentTime
    let res = myDecode1 (fromSchema v) f
    print $ length $ map isEither res

    t1 <- getCurrentTime 
    print $ diffUTCTime t1 t0

















