{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module New23 where


import Data.Text (Text)
import qualified Data.Vector as V
import Data.Int
import Data.ByteString (ByteString)
import Data.Coerce

data InternalSchema
    = Null
    | Boolean
    | Int
    | Long
    | Float
    | Double
    | Bytes
    | String
    | Array InternalSchema
    | Record {
        name :: Text,
        fields :: [Field]
    }
    | Enum {
        name :: Text,
        symbols :: V.Vector Text
    } deriving (Eq, Show)


data Field 
    = Field {
        fldName :: Text,
        fldIndex :: Int,
        fldType :: InternalSchema
    } deriving (Eq, Show)


class Trs a where
    trs :: InternalObject -> a

instance Trs Bool where 
    trs (OBool b) = b

instance Trs Int32 where 
    trs (OInt i) = i


data InternalObject 
    = ONull
    | OBool Bool
    | OInt Int32
    | OLong Int64
    | OFloat Float
    | ODouble Double
    | OBytes ByteString
    | OString Text
    | OArray (V.Vector InternalObject)
    | ORecord (V.Vector InternalObject)
    | OEnum Int Text
    deriving (Eq, Show)


newtype IObj = IInt Int 
    --      | IBool Bool
--    IBool :: Bool -> IObj

-- instance Coercible (IObj Int) Int


newtype Myo = Myo Int

ta = coerce [Myo 1] :: [Int]

tb = coerce (IInt 1) :: Int

---
-- Person 
-- {
--  name :: Text
--  age :: Int 
--  position :: {
--    x :: Double
--    y :: Double
    --  }
    -- }

person :: InternalSchema
person = Record {
    name = "Person",
    fields = [
        Field "name" 0 String,
        Field "age" 1 Int,
        Field "position" 2 (
            Record "position" [
                Field "x" 0 Double,
                Field "y" 1 Double
            ]
        )
    ]
}

---
-- person 
-- {
--  name :: "yang"
--  age :: 23 
--  position :: {
--    x :: 11.2
--    y :: 12.3
    --  }
    -- }

pobj :: InternalObject
pobj = ORecord $ V.fromList
    [ OString "yang",
      OInt 23,
      ORecord $ V.fromList [
          ODouble 11.2,
          ODouble 12.3
      ]
    ]


check :: InternalSchema -> InternalObject -> Bool
check Null ONull = True
check Boolean (OBool _) = True
check Int (OInt _) = True
check Long (OLong _) = True
check Float (OFloat _) = True
check Double (ODouble _) = True
check Bytes (OBytes _) = True
check String (OString _) = True
check (Array v) (OArray ls) = and (check v <$> V.toList ls)
check (Record _ fs) (ORecord ls) = 
     (length fs == V.length ls) &&  and (zipWith (check . fldType ) fs (V.toList ls))
check (Enum _ ls) (OEnum i t) = ls V.! i == t
check a b = error $ show a ++ " " ++ show b

lookS :: Text -> [Field] -> (Int, InternalSchema)
lookS _ [] = error "..."
lookS t ((Field n i s):xs) = 
    if t == n 
        then (i, s)
        else lookS t xs

t :: InternalSchema -> [Text] -> [Int]
t _ [] = []
t (Record _ fields) (x:xs) = 
    case lookS x fields of 
        (i, is) -> i : t is xs
t e v = error $ show e ++ " " ++ show v

tt :: [Int] -> InternalObject -> InternalObject
tt [x] (ORecord vi) = vi V.! x
tt (x:xs) (ORecord vi) =  tt xs (vi V.! x)


ttt :: InternalSchema -> [Text] -> InternalObject -> InternalObject
ttt is xs  = tt (t is xs ) 


--tis :: InternalSchema -> [Text] -> InternalSchema
--tis is (x:xs) = 
--    case lookS x is of 
--        (_, ins) -> tis ins xs
--

t2 = ttt person ["position", "y"] pobj

