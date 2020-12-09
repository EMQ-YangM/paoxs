{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module New17 where

import Control.Concurrent
import Control.Concurrent.Chan
import Data.Dynamic
import GHC.TypeNats (KnownNat, Nat, natVal, type (+), type (-))
import Data.Kind
import Data.Data (Proxy)
import GHC.Base (Symbol)
import Data.Proxy
import GHC.TypeLits
import Data.Void (Void)
import Data.Map (Map, empty, insert, toList, lookup)
import Data.List (intersperse)
import Shelly
import Data.IORef
import System.IO.Unsafe
import Control.Monad (forM_)


data IN (s :: Symbol) (i :: [Symbol]) (o :: Type) = IN

data Node (s :: Symbol) (i :: [(Symbol, Type)]) (o :: (Type, [Symbol]))
    = Node String 
            (CList i) 
            (Chan Dynamic) 
            (CList i -> Get o )

type SSS = (Symbol, [(Symbol, Type)], (Type, [Symbol]) )

data State :: [SSS] -> Type where
    Base :: State '[]
    (:>) ::  forall s i o zs. NotElem s zs =>
        State zs -> IN s i o -> State (AddNode s i o (InsOuts s i zs))

type family NotElem (s :: Symbol) (es :: [SSS] ) :: Constraint where
    NotElem _ '[] = ()
    NotElem s ('(v, _, _) ': xs) =  
        If (CmpSymbol s v) ( TypeError (Text "Node "  :<>: ShowType v :<>: Text " already exists!!!" )) (NotElem s xs)

type family Get s where
    Get '(t, _) = t

type family If (a :: Ordering) b c  where
    If EQ b _ = b
    If _  _ c = c

type family InsOuts (s :: Symbol) (is :: [Symbol]) (ss :: [SSS]) where
    InsOuts sb (s ': vs) ss = InsOuts sb vs (InsOut sb s ss)
    InsOuts _ '[] ss = ss

type family InsOut (s :: Symbol) (i :: Symbol) (ss :: [SSS]) where
    InsOut sb i ('(v, xs, '(t , ys)) ': zs) =
        If (CmpSymbol i v)  ( '(v, xs, '(t , sb ': ys)) ': zs) ('(v, xs,'(t , ys)) ': InsOut sb i zs  )
    InsOut sb i '[] = TypeError ( Text "node " :<>: ShowType sb :<>: Text " inputs: " :<>: ShowType i :<>: Text " not found in graph")

type family GetType (i :: Symbol) (ss :: [SSS]) where
    GetType i ('(v, _, '(t , _)) ': zs) = 
        If (CmpSymbol i v) '(i,t) (GetType i zs)

type family GetTypes (is :: [Symbol]) (ss :: [SSS]) where
    GetTypes '[] _ = '[]
    GetTypes ( v ': vs ) zs = GetType v zs : GetTypes vs zs

type family AddNode (s :: Symbol) (i :: [Symbol]) (o :: Type) (ss :: [SSS]) where
    AddNode s i o zs =  '(s, GetTypes i zs, '(o, '[]) ) ': zs

infixl 4 :>

data SList (s :: Symbol) a = SList [a]

instance (KnownSymbol s, Show a) => Show (SList s a) where
    show (SList xs) = " (" ++ symbolVal (Proxy :: Proxy s)  ++ " " ++ show xs ++ ")"

data Sval (s :: Symbol) v = Sval v

instance (KnownSymbol s, Show v) => Show (Sval s v) where 
    show (Sval v) = "symbol is " ++ symbolVal (Proxy :: Proxy s) ++ ", value is " ++ show v

inSval :: Sval s v -> SList s v -> SList s v
inSval (Sval x) (SList xs) = SList (x : xs)

data CList :: [(Symbol, Type)] -> Type where
    CNil :: CList '[]
    CAdd :: (KnownSymbol s, Show a) => SList s a -> CList xs -> CList ('(s,a) ': xs)

instance Show (CList xs) where
    show CNil= ""
    show (CAdd x ys) = show x ++ show ys

--------------------------------------------

type family InList (s :: Symbol) (es :: [(Symbol, Type)]) where
    InList s '[] = TypeError (Text "there is not symbol " :<>: ShowType s :<>: Text " in CList")
    InList s ('(v, _) ': xs) = If (CmpSymbol s v) () (InList s xs)


class InCList a s where
    inCList :: a -> s -> s

instance (Typeable t, InCList Dynamic (CList xs)) 
    => InCList Dynamic (CList ('(s, t) ': xs)) where
    inCList d (CAdd y ys) 
        = case fromDynamic d :: Maybe (Sval s t) of 
            Just v -> CAdd (inSval v y) ys
            Nothing -> CAdd y (inCList d ys)

instance InCList Dynamic (CList '[]) where 
    inCList _ _ = error "strange happened"

--------------------------------------------

class CreateEmptyList s where
    cEmptyList :: Proxy s -> CList s

instance (CreateEmptyList xs, KnownSymbol s, Show a) 
   => CreateEmptyList ( '(s, a) ': xs ) where
    cEmptyList _ = CAdd (SList [] :: SList s a) (cEmptyList (Proxy :: Proxy xs))

instance CreateEmptyList '[] where
    cEmptyList _ = CNil


instance {-# OVERLAPPABLE #-}
   forall s b v xs. ( InCList (Sval s v) (CList xs), InList s xs ~ ())
    => InCList (Sval s v) (CList ( '(b, v) ': xs )) where 
    inCList x (CAdd y ys) = CAdd y $ inCList x ys

instance {-# OVERLAPPABLE #-}
    forall s v xs. InCList (Sval s v) (CList ( '(s, v) ': xs )) where 
    inCList x (CAdd y ys) = CAdd (inSval x y) ys

instance InCList (Sval s v) (CList '[]) where
    inCList _ CNil = error "strange happened"


tf :: CreateEmptyList s => CList s
tf  = cEmptyList (Proxy :: Proxy s)

tt :: CList '[ '("a", Int) ,  '("b", Int) ]
tt = tf 

tt1 = inCList (Sval 1 :: Sval "b" Int) tt

-------------
-- a  b
--
--  c  
-- a  b
-- -  -
-- -  -
-- -  -
-- -  -
--

source :: IN "source" '[] Int
source = IN

t1 = IN :: IN "source" '[] Int
t2 = IN :: IN "a" '["source"] Int  --  + 1
t3 = IN :: IN "b" '["source"] Int  -- + 2
t4 = IN :: IN "c" '["a", "b"] Int  -- a + b
t5 = IN :: IN "d" '["a", "b"] Int  -- a + b
t6 = IN :: IN "s" '["c", "d", "e"] Int

tv = Base :> t1
          :> t2
          :> t3
          :> t4
          :> t5
          :> (IN :: IN "e" '[ "c","a", "b"] Int)
          :> t6


func :: (Translate (Reverse ss '[]) ~ v) => State ss -> v -> v
func _ y = y

ff = func tv 
   ( cNode (\_ -> 1) :<|>
     cNode (\(CAdd (SList xs) CNil) -> sum xs + 100) :<|>
     cNode (\(CAdd (SList xs) CNil) -> sum xs + 2) :<|>
     cNode  (\(CAdd (SList xs) (CAdd (SList ys) CNil)) -> sum xs + sum ys) :<|>
     cNode  (\(CAdd (SList xs) (CAdd (SList ys) CNil)) -> sum xs - sum ys) :<|>
     cNode  (\(CAdd (SList xs) (CAdd (SList ys) (CAdd (SList zs) CNil))) -> sum xs - sum ys) :<|>
     cNode (\(CAdd (SList xs) (CAdd (SList ys) (CAdd (SList zs) CNil))) -> sum xs + sum ys ) :<|>
     End )  

hf :<|> kf = ff

test :: IO ()
test = do 
    logChan <- newChan :: IO (Chan String)
    ref <- newIORef (Data.Map.empty)
    runsource logChan hf ref
    start logChan ref kf
    handleLog logChan
  where 
    handleLog chan = do 
        v <- readChan chan
        putStrLn v 
        handleLog chan
        
run ::forall s i o.
    (InCList Dynamic (CList i), KnownSymbol s, Typeable (Get o), H3 o, Show (Get o)) 
    => LogChan -> Node s i o -> IORef (Map String (Chan Dynamic)) 
        -> IO () 
run lc (Node s cl c f ) mref = 
    do m <- readIORef mref
       let newMap = insert s c m
       writeIORef mref newMap
       forkIO loop
       return ()
    where 
       loop :: IO ()
       loop = do 
         v <- readChan c
         let ncl = inCList v cl
             v1 = f ncl
             names = h3 (Proxy :: Proxy o)
         writeChan lc ( "the node " ++ symbolVal (Proxy :: Proxy s) ++ " value is " ++ show v1)
         m <- readIORef mref
         forM_ names $ \name -> do 
           case Data.Map.lookup name m of 
             Nothing -> return ()
             Just chan -> writeChan chan (toDyn (Sval v1 :: Sval s (Get o)))
         loop

runsource ::forall s i o.
    (InCList Dynamic (CList i), KnownSymbol s, Typeable (Get o), H3 o, Show (Get o)) 
    => LogChan -> Node s i o -> IORef (Map String (Chan Dynamic)) 
        -> IO ()
runsource lc (Node s cl c f ) mref = 
    do m <- readIORef mref
       let newMap = insert s c m
       writeIORef mref newMap
       forkIO loop
       return ()
    where 
       loop :: IO ()
       loop = do 
         threadDelay (1*10^6)
         let v1 = f cl
             names = h3 (Proxy :: Proxy o)
         writeChan lc ("source node value is " ++ show v1)
         m <- readIORef mref
         forM_ names $ \name -> do 
           case Data.Map.lookup name m of 
             Nothing -> return ()
             Just chan -> writeChan chan (toDyn (Sval v1 :: Sval s (Get o)))
         loop

type MapRef = IORef (Map String (Chan Dynamic))

type LogChan = Chan String

class Start s where
    start :: LogChan -> MapRef -> s -> IO ()

instance (InCList Dynamic (CList i), KnownSymbol s, Typeable (Get o), H3 o, Start ns, Show (Get o)) 
    => Start (Node s i o :<|> ns) where
    start lc ref (n :<|> ns) = do
        New17.run lc n ref
        start lc ref ns
        
instance Start End where
    start _ _ _ = print "start finish"


cNode :: forall s i o. (KnownSymbol s, CreateEmptyList i) 
    => (CList i -> Get o) -> Node s i o 
cNode f = Node (symbolVal (Proxy :: Proxy s )) 
             (cEmptyList (Proxy :: Proxy i) ) 
             (unsafePerformIO (newChan :: IO (Chan v)))
             f

data a :<|> b = a :<|> b

instance (Show a, Show b) => Show (a :<|> b) where
    show (a :<|> b) = show a  ++ "\n" ++ show b

infixr 4 :<|>

instance Show (Node s i o) where
    show (Node s cl _ _) = "Node " ++ s ++ show cl

data End = End

instance Show End where 
    show End = ""

type family Translate xs where
    Translate '[] = End
    Translate ('(v, xs, t) ': zs) 
     = Node v xs t :<|> Translate zs

type family Reverse xs ys where
    Reverse '[] y = y
    Reverse (x ': xs) y = Reverse xs (x ': y)

----------------------------------------
cc = cr tv

runCmd :: IO ()
runCmd = shelly $ do
    run_ "dot" ["-Tpng", "-o", "wf.png", "wf.dot" ]
    run_ "open" ["wf.png"]

cr ::(HasState s) => State s -> IO ()
cr s = do 
    let r = fun1 Proxy s
    createDot r 
    runCmd

fun1 :: (HasState s) => Proxy s -> State s -> Map String [String]
fun1 p _  = has p

createDot :: Map String [String] -> IO ()
createDot m = writeFile "wf.dot" res 
    where ls = Data.Map.toList m
          loop [] = ""
          loop ((name, outList) : xs) = name ++ 
                        " -> {" ++ (concat $ intersperse "," outList) ++ "}\n"  ++ loop xs
          res = "digraph test{\n" ++ loop ls ++ "\n}"

class HasState s where
    has :: Proxy s -> Map String [String]

instance (H1 x, HasState xs) => HasState (x ': xs) where
    has _ = insert k a $ has (Proxy :: Proxy xs)
        where (k, a) = h1 (Proxy :: Proxy x)

instance HasState '[] where
    has _ = empty

class H1 s where 
    h1 :: Proxy s -> (String, [String])

instance (KnownSymbol s, H2 ss) => H1 '(s, ys, '(t, ss)) where
    h1 _ = (symbolVal (Proxy :: Proxy s), h2 (Proxy :: Proxy ss))

class H2 s where 
    h2 :: Proxy s -> [String]

instance (KnownSymbol s, H2 ss) => H2 ( s ': ss ) where
    h2 _ = symbolVal (Proxy :: Proxy s) : (h2 (Proxy :: Proxy ss))

instance H2 '[] where 
    h2 _ = []

class H3 s where
    h3 :: Proxy s -> [String]

instance H2 xs => H3 '(t, xs) where
    h3 _ = h2 (Proxy :: Proxy xs)

