{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module New30 where


import Data.IORef
import GHC.Conc
import Control.Monad
import Data.Data
import Data.Kind

data R1 
    = R1 {
        r11 :: Int,
        r12 :: Int
    } deriving (Show, Eq)


data R2 
    = R2 {
        r21 :: Int,
        r22 :: Int
    } deriving (Show, Eq)

data R v
    = V v 
    | L v 
    | R v
    deriving Show
    
data Record k v 
    = Record {
        rk :: k ,
        rv :: R v
    } deriving Show

data Guard a 
    = Continue a
    | Skip
    deriving (Show)

type Fun k v k1 v1 
    = Record k v -> IO (Guard (Record k1 v1))

type JoinFun k v k1 v1 k2 v2
    = Cache k v k1 v1 -> IO (Guard (Record k2 [v2]))

type Cache k v k1 v1 = ([Record k v], [Record k1 v1])

data FS k v where 
    FS :: (Typeable v1, Typeable v, Typeable k1, Typeable k) 
        => Fun k v k1 v1 
        -> [FS k1 v1] 
        -> FS k v
    JF :: ( Typeable k, Typeable v
          , Typeable k1, Typeable v1
          , Typeable k2, Typeable v2
          , Typeable k3, Typeable v3
          ) 
        => IORef (TVar Bool, Cache k v k1 v1) 
            -> JoinFun k v k1 v1 k2 v2 
            -> [FS k2 [v2]]
            -> FS k3 v3

source1 :: Fun k String k R1
source1 (Record k (V _)) = return $ Continue $ Record k (L (R1 101 2))

source2 :: Fun k String k R2
source2 (Record k (V _)) = return $ Continue $ Record k (R (R2 101 201))

join1 :: JoinFun Int R1 Int R2 Int (Int, Int)
join1 (ls, rs) = return $ Continue $ Record 1 $ V $ concatMap findv ls
    where 
        findv (Record _ (V l)) = 
            case filter (\(Record _ (V r)) -> r21 r == r11 l ) rs of 
                [] -> []
                (Record _ (V x)) : _ -> [(r11 l, r22 x)]

sink :: Show (Record k v) => Fun k v () ()
sink r = print r >> return Skip

jfun :: Typeable v3 => IORef (TVar Bool, Cache Int R1 Int R2) -> FS Int v3
jfun ref = JF ref join1 [FS sink []]

myfs ref = [FS source1 [jfun ref], FS source2 [jfun ref]]

m1 :: IO ()
m1 = do 
    tvar <- registerDelay 1000000
    ref <- newIORef (tvar, ([],[]))
    let myfs1 = myfs ref
    forM_ ( replicate 30 $ Record 1 (V "ncie")) $ 
        \r -> do
            threadDelay 100000
            forM_ myfs1 $ \f -> runFS r f

runFS :: (Typeable v, Typeable k) => Record k v -> FS k v -> IO ()
runFS r@(Record _ (V _)) (FS fun fs) = do 
    v1 <- fun r
    case v1 of 
        Skip -> return ()
        Continue rv -> forM_ fs $ \f -> runFS rv f
runFS (Record k (L v)) (JF ref jfun fs) = do 
    (tvbool, ca@(leftv, rightv)) <- readIORef ref
    b <- readTVarIO tvbool
    if b 
        then do 
           vb <- registerDelay 1000000
           writeIORef ref (vb, ([],[]))
           Continue vv <- jfun ca
           forM_ fs $ \f -> runFS vv f
        else case cast v of 
                   Just lv -> case cast k of 
                            Just kk -> writeIORef ref (tvbool, ( Record kk (V lv) :leftv, rightv))
                            Nothing -> error "eeeeeeeee"
                   Nothing -> error "left trans error"
runFS (Record k (R v)) (JF ref jfun fs) = do 
    (tvbool, ca@(leftv, rightv)) <- readIORef ref
    b <- readTVarIO tvbool
    if b 
        then do 
           vb <- registerDelay 1000000
           writeIORef ref (vb, ([],[]))
           Continue vv <- jfun ca
           forM_ fs $ \f -> runFS vv f
        else case cast v of 
                   Just rv -> case cast k of 
                            Just kk -> writeIORef ref (tvbool, ( leftv, Record kk (V rv) : rightv))
                            Nothing -> error "aaaaaaaaaa"
                   Nothing -> error "right trans error"

addFun :: (Typeable k0, Typeable k, Typeable v0, Typeable v) 
    => Fun k0 v0 k v -> FS k v -> FS k0 v0
addFun fun fs = FS fun [fs] 

ss :: Fun Int String Int String 
ss = undefined

t1 = addFun ss 
    $ addFun ss 
    $ addFun ss 
    $ addFun ss 
    $ FS source1[FS sink []]

data CC = forall a. CC String String a

c1 = CC "" "source" ss
c2 = CC "source" "p1" ss
c3 = CC "p1" "sink" undefined

















