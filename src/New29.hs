{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
module New29 where


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


data Record v 
    = Record {
        rk :: String,
        rv :: v
    } deriving (Show, Eq)

data Guard a 
    = Continue a
    | Skip
    deriving (Show)

type Fun v v1 
    = Record  v -> IO (Guard (Record v1))

type Choise v 
    = Record  v -> IO Int 

type JoinFun v v1 v2
    = Cache v v1 -> IO (Guard [Record v2])

type Cache v v1 = ([Record v], [Record v1])

data v :< v1 
    = L v 
    | R v1
    deriving Show

class MyLift a b where 
    myLift :: a -> b

instance {-# OVERLAPPABLE #-} MyLift a () where
    myLift _ = ()

instance {-# OVERLAPPABLE #-} MyLift a a where
    myLift = id

instance MyLift a (a :< b) where
    myLift a = L a

instance MyLift b (a :< b) where
    myLift b = R b

data FS v where
     TE :: FS ()
     FC :: Choise v -> [FS v] -> FS v
     FS :: MyLift v1 b =>  Fun v v1 -> [FS b] -> FS v
     TF :: IORef (TVar Bool, Cache v v1) -> JoinFun v v1 v2 -> [FS v2] -> FS (v :< v1)

source1 :: Record R1 -> IO (Guard (Record R1))
source1 (Record a r) = return $ Continue $  Record a $ myLift r

source1' :: Record R1 -> IO (Guard (Record Int))
source1' (Record a r) = return $ Continue $  Record a $ myLift (r11 r)

source2 :: Record R2 -> IO (Guard (Record R2))
source2 (Record a r) = return $ Continue $  Record a $ myLift r

sink :: Show a => a -> IO (Guard ())
sink r = print r >> return Skip

s1 :: FS (R1 :< R2)
s1 = TF undefined undefined [TE]

ss1 :: Record String -> IO (Guard (Record R1))
ss1 = undefined

ss2 :: Record String -> IO (Guard (Record R2))
ss2 = undefined
              
mm1 = FS ss1 [ FS source1 [ FS source1 [ s1 ]
                          , FS source1' [TE] ] 
                          ]

mm2 = FS ss2 [FS source2 [s1]]

fff :: Record String -> IO Int
fff  = undefined

mm3 = FC fff [mm1, mm2]

runFS :: Record v -> FS v -> IO ()
runFS r@(Record k v) (FS fun fs) = do 
    v1 <- fun r 
    case v1 of 
        Skip -> return ()
        Continue (Record a v2) -> 
            forM_ fs $ \f -> runFS (Record a $ myLift v2) f
runFS (Record a v) (TF ref jfun fs) = do 
    (tbool, (lefts, rights)) <- readIORef ref
    case v of 
        (L v1') -> writeIORef ref (undefined, ( Record a v1' : lefts, rights))
        (R v1') -> writeIORef ref (undefined, ( lefts, Record a v1' : rights))
    return undefined


--runFS :: forall b v. MyLift v b => Proxy b -> Record v -> FS b -> IO ()
--runFS _ (Record k v) (FS fun fs) = do 
--    v1 <- fun (Record k (myLift v))
--    case v1 of 
--      Skip -> return ()
--      Continue (Record a v2) -> 
--        forM_ fs $ \f -> runFS Proxy (Record a v2) f
--runFS _ (Record k v) (FC choise fs) = do 
--    i <- choise (Record k $ myLift v) 
--    let v1 = fs !! i
--    runFS Proxy (Record k v) v1
--runFS _ _ TE = return ()
--runFS (Proxy :: Proxy b) (Record a v) (TF ref jfun fs) = do 
--    let v1 = myLift v :: b
--    (tbool, (lefts, rights)) <- readIORef ref
--    b <- readTVarIO tbool
--    if b 
--        then undefined -- join function
--        else undefined 
        --do -- addCache
        --  case v1 of 
        --      Nothing -> undefined
        --    --Just (L c) -> writeIORef ref (tbool, (c : lefts, rights))
        --  undefined


--runFS :: forall b v. MyLift v b => Proxy b -> Record v -> FS b -> IO ()
--runFS _ (Record k v) (FS fun fs) = do 
--    v1 <- fun (Record k (myLift v))
--    case v1 of 
--      Skip -> return ()
--      Continue (Record a v2) -> 
--        forM_ fs $ \f -> runFS Proxy (Record a v2) f
--runFS _ (Record k v) (FC choise fs) = do 
--    i <- choise (Record k $ myLift v) 
--    let v1 = fs !! i
--    runFS Proxy (Record k v) v1
--runFS _ _ TE = return ()
--runFS (Proxy :: Proxy b) (Record a v) (TF ref jfun fs) = do 
--    let v1 = myLift v :: b
--    (tbool, (lefts, rights)) <- readIORef ref
--    b <- readTVarIO tbool
--    if b 
--        then undefined -- join function
--        else undefined 
        --do -- addCache
        --  case v1 of 
        --      Nothing -> undefined
        --    --Just (L c) -> writeIORef ref (tbool, (c : lefts, rights))
        --  undefined

runfs :: Record (a :< b) -> FS (a :< b) -> IO ()
runfs = undefined


join1 :: ([Record R1], [Record R2]) -> IO (Guard [Record (Int, Int)])
join1 (ls, rs) = return $ Continue $ concatMap findv ls
    where 
        findv (Record a l) = 
            case filter (\(Record _ r) -> r21 r == r11 l) rs  of
                    [] ->  []
                    (Record _ x):_ -> [Record a (r11 l, r21 x)]





















