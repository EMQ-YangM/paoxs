{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module New18 where

import Control.Concurrent
import Control.Monad (forM_)
import Data.Dynamic
import Data.IORef
import Data.Kind
import Data.List (intersperse)
import Data.Map (Map, empty, insert, lookup, toList)
import Data.Proxy
import GHC.TypeLits
import Shelly
import System.IO.Unsafe

data WindowType = WA | WB | WC deriving (Show, Ord, Eq)

data TriggerType = TA | Reapet Int | Immediately deriving (Show, Ord, Eq)

data IN (s :: Symbol) (i :: [Symbol]) (o :: Type) = IN WindowType TriggerType

data Node (s :: Symbol) (i :: [(Symbol, Type)]) (o :: (Type, [Symbol]))
  = Node
      String
      WindowType
      TriggerType
      (Chan Dynamic)
      (CList i -> IO (Get o))

type SSS = (Symbol, [(Symbol, Type)], (Type, [Symbol]))

data State :: [SSS] -> Type where
  Base :: State '[]
  (:>) ::
    forall s i o zs.
    NotElem s zs =>
    State zs ->
    IN s i o ->
    State (AddNode s i o (InsOuts s i zs))

type family NotElem (s :: Symbol) (es :: [SSS]) :: Constraint where
  NotElem _ '[] = ()
  NotElem s ('(v, _, _) ': xs) =
    If (CmpSymbol s v) (TypeError (Text "Node " :<>: ShowType v :<>: Text " already exists!!!")) (NotElem s xs)

type family Get s where
  Get '(t, _) = t

type family If (a :: Ordering) b c where
  If EQ b _ = b
  If _ _ c = c

type family InsOuts (s :: Symbol) (is :: [Symbol]) (ss :: [SSS]) where
  InsOuts sb (s ': vs) ss = InsOuts sb vs (InsOut sb s ss)
  InsOuts _ '[] ss = ss

type family InsOut (s :: Symbol) (i :: Symbol) (ss :: [SSS]) where
  InsOut sb i ('(v, xs, '(t, ys)) ': zs) =
    If (CmpSymbol i v) ('(v, xs, '(t, sb ': ys)) ': zs) ('(v, xs, '(t, ys)) ': InsOut sb i zs)
  InsOut sb i '[] = TypeError (Text "node " :<>: ShowType sb :<>: Text " inputs: " :<>: ShowType i :<>: Text " not found in graph")

type family GetType (i :: Symbol) (ss :: [SSS]) where
  GetType i ('(v, _, '(t, _)) ': zs) =
    If (CmpSymbol i v) '(i, t) (GetType i zs)

type family GetTypes (is :: [Symbol]) (ss :: [SSS]) where
  GetTypes '[] _ = '[]
  GetTypes (v ': vs) zs = GetType v zs : GetTypes vs zs

type family AddNode (s :: Symbol) (i :: [Symbol]) (o :: Type) (ss :: [SSS]) where
  AddNode s i o zs = '(s, GetTypes i zs, '(o, '[])) ': zs

infixl 4 :>

data SList (s :: Symbol) a = SList [a]

instance (KnownSymbol s, Show a) => Show (SList s a) where
  show (SList xs) = " (" ++ symbolVal (Proxy :: Proxy s) ++ " " ++ show xs ++ ")"

data Sval (s :: Symbol) v = Sval v

instance (KnownSymbol s, Show v) => Show (Sval s v) where
  show (Sval v) = "symbol is " ++ symbolVal (Proxy :: Proxy s) ++ ", value is " ++ show v

inSval :: Sval s v -> SList s v -> SList s v
inSval (Sval x) (SList xs) = SList (x : xs)

data CList :: [(Symbol, Type)] -> Type where
  E :: CList '[]
  (:<) :: (KnownSymbol s, Show a) => SList s a -> CList xs -> CList ('(s, a) ': xs)

infixr 4 :<

instance Show (CList xs) where
  show E = "E"
  show (x :< ys) = show x ++ show ys

--------------------------------------------

type family InList (s :: Symbol) (es :: [(Symbol, Type)]) where
  InList s '[] = TypeError (Text "there is not symbol " :<>: ShowType s :<>: Text " in CList")
  InList s ('(v, _) ': xs) = If (CmpSymbol s v) () (InList s xs)

class InCList a s where
  inCList :: a -> s -> s

instance
  (Typeable t, InCList Dynamic (CList xs)) =>
  InCList Dynamic (CList ('(s, t) ': xs))
  where
  inCList d (y :< ys) =
    case fromDynamic d :: Maybe (Sval s t) of
      Just v -> inSval v y :< ys
      Nothing -> y :< inCList d ys

instance InCList Dynamic (CList '[]) where
  inCList _ _ = error "strange happened"

--------------------------------------------

class CreateEmptyList s where
  cEmptyList :: Proxy s -> CList s

instance
  (CreateEmptyList xs, KnownSymbol s, Show a) =>
  CreateEmptyList ('(s, a) ': xs)
  where
  cEmptyList _ = (SList [] :: SList s a) :< cEmptyList (Proxy :: Proxy xs)

instance CreateEmptyList '[] where
  cEmptyList _ = E

instance
  {-# OVERLAPPABLE #-}
  forall s b v xs.
  (InCList (Sval s v) (CList xs), InList s xs ~ ()) =>
  InCList (Sval s v) (CList ('(b, v) ': xs))
  where
  inCList x (y :< ys) = y :< inCList x ys

instance {-# OVERLAPPABLE #-} forall s v xs. InCList (Sval s v) (CList ('(s, v) ': xs)) where
  inCList x (y :< ys) = inSval x y :< ys

instance InCList (Sval s v) (CList '[]) where
  inCList _ E = error "strange happened"

tf :: CreateEmptyList s => CList s
tf = cEmptyList (Proxy :: Proxy s)

tt :: CList '[ '("a", Int), '("b", Int)]
tt = tf

tt1 = inCList (Sval 1 :: Sval "b" Int) tt

----------------------------------------
tv =
  Base
    :> (IN WB TA :: IN "source" '[] Int)
    :> (IN WB TA :: IN "source1" '[] Int)
    :> (IN WA (Reapet 1) :: IN "a" '["source", "source1"] Int)
    :> (IN WA (Reapet 1) :: IN "b" '["a"] String)
    :> (IN WA (Reapet 5) :: IN "c" '["a", "b"] Int)

ff =
  func
    tv
    ( Point
        :<|> (\_ -> return 1)
        :<|> (\_ -> return 2)
        :<|> (\((SList xs) :< (SList ys) :< E) -> return $ sum xs + sum ys)
        :<|> (\((SList xs) :< E) -> return $ show xs)
        :<|> (\((SList xs) :< (SList ys) :< E) -> return $ sum xs)
    )

test :: IO ()
test = do
  logChan <- newChan :: IO (Chan String)
  ref <- newIORef (Data.Map.empty)
  start logChan ref ff
  handleLog logChan
  where
    handleLog chan = do
      v <- readChan chan
      putStrLn v
      handleLog chan

----------------------------------------

func :: forall ss. Trans ss (Funs ss) (Nodes ss) => State ss -> Funs ss -> Nodes ss
func s = trans (reverse $ s2wt s) (Proxy :: Proxy ss)

type family Funs s where
  Funs ('(_, i, o) ': xs) = Funs xs :<|> (CList i -> IO (Get o))
  Funs '[] = Point

type family Nodes s where
  Nodes ('(s, i, o) ': xs) = Nodes xs :<|> Node s i o
  Nodes '[] = Point

data Message
  = Calculation
  | Stop

-----------------------------------------

timer :: Int -> IO () -> IO ()
timer i action = loop
  where
    loop = do
      threadDelay (i * 10 ^ 6)
      action
      loop

ctime :: Int -> Message -> Chan Dynamic -> IO ()
ctime i m chan =
  forkIO (timer i (writeChan chan (toDyn m))) >> return ()

run ::
  forall s i o.
  (InCList Dynamic (CList i), KnownSymbol s, Typeable (Get o), H3 o, Show (Get o), CreateEmptyList i) =>
  LogChan ->
  Node s i o ->
  IORef (Map String (Chan Dynamic)) ->
  IO ()
run lc (Node s wt tt c f) mref =
  do
    m <- readIORef mref
    let newMap = insert s c m
    writeIORef mref newMap
    case tt of
      Reapet i -> (forkIO $ loop (cEmptyList (Proxy :: Proxy i))) >> ctime i Calculation c
      Immediately -> forkIO loopImm >> return ()
      _ -> return ()
    return ()
  where
    loop cacheList = do
      v <- readChan c
      case fromDynamic v :: Maybe Message of
        Just message -> case message of
          Calculation -> do
            v1 <- f cacheList
            let names = h3 (Proxy :: Proxy o)
            --  writeChan lc $ "[" ++ symbolVal (Proxy :: Proxy s) ++ "] -> " ++ show cacheList
            writeChan lc ("[" ++ symbolVal (Proxy :: Proxy s) ++ "] -> " ++ show v1)
            m <- readIORef mref
            forM_ names $ \name -> do
              case Data.Map.lookup name m of
                Nothing -> return ()
                Just chan -> writeChan chan (toDyn (Sval v1 :: Sval s (Get o)))
            loop (cEmptyList (Proxy :: Proxy i))
        Nothing -> do
          let ncl = inCList v cacheList
          loop ncl
    loopImm = do
      v <- readChan c
      let ncl = inCList v (cEmptyList (Proxy :: Proxy i))
      v1 <- f ncl
      writeChan lc ("[" ++ symbolVal (Proxy :: Proxy s) ++ "] -> " ++ show v1)
      m <- readIORef mref
      let names = h3 (Proxy :: Proxy o)
      forM_ names $ \name -> do
        case Data.Map.lookup name m of
          Nothing -> return ()
          Just chan -> writeChan chan (toDyn (Sval v1 :: Sval s (Get o)))
      loopImm

runsource ::
  forall s i o.
  (InCList Dynamic (CList i), KnownSymbol s, Typeable (Get o), H3 o, Show (Get o)) =>
  LogChan ->
  Node s i o ->
  IORef (Map String (Chan Dynamic)) ->
  IO ()
runsource lc (Node s wt tt c f) mref =
  do
    m <- readIORef mref
    let newMap = insert s c m
    writeIORef mref newMap
    forkIO loop
    return ()
  where
    loop :: IO ()
    loop = do
      threadDelay (1 * 10 ^ 3)
      let names = h3 (Proxy :: Proxy o)
      v1 <- f undefined -- (cEmptyList (Proxy :: Proxy i))
      --   writeChan lc ( "[" ++ s ++ "] -> " ++ show v1)
      m <- readIORef mref
      forM_ names $ \name -> do
        case Data.Map.lookup name m of
          Nothing -> return ()
          Just chan -> writeChan chan (toDyn (Sval v1 :: Sval s (Get o)))
      loop

runsink ::
  forall s i o.
  (KnownSymbol s, Typeable (Get o), Show (Get o)) =>
  LogChan ->
  Node s i o ->
  IORef (Map String (Chan Dynamic)) ->
  IO ()
runsink lc (Node s wt tt c f) mref =
  do
    m <- readIORef mref
    let newMap = insert s c m
    writeIORef mref newMap
    forkIO loop
    return ()
  where
    loop :: IO ()
    loop = do
      v <- readChan c
      writeChan lc $ show v
      --case fromDynamic v :: Maybe (Get o) of
      --  Just val -> writeChan lc $ show val
      --  Nothing -> writeChan lc "some error happened ........"
      loop

type MapRef = IORef (Map String (Chan Dynamic))

type LogChan = Chan String

class Start s where
  start :: LogChan -> MapRef -> s -> IO ()

instance
  {-# OVERLAPPABLE #-}
  (InCList Dynamic (CList i), KnownSymbol s, Typeable (Get o), H3 o, Start ns, Show (Get o), CreateEmptyList i) =>
  Start (ns :<|> Node s i o)
  where
  start lc ref (ns :<|> n) = do
    putStrLn $ "[" ++ symbolVal (Proxy :: Proxy s) ++ "] start success!!"
    New18.run lc n ref
    start lc ref ns

instance
  {-# OVERLAPPABLE #-}
  (KnownSymbol s, Typeable (Get o), H3 o, Start ns, Show (Get o)) =>
  Start (ns :<|> Node s '[] o)
  where
  start lc ref (ns :<|> n) = do
    New18.runsource lc n ref
    start lc ref ns

instance
  {-# OVERLAPPABLE #-}
  (KnownSymbol s, Typeable t, Show t, Start ns) =>
  Start (ns :<|> Node s i '(t, '[]))
  where
  start lc ref (ns :<|> n) = do
    New18.runsink lc n ref
    start lc ref ns

instance Start Point where
  start _ _ _ = print "start finish"

data a :<|> b = a :<|> b

instance (Show a, Show b) => Show (a :<|> b) where
  show (a :<|> b) = show a ++ "\n" ++ show b

infixl 4 :<|>

instance Show (Node s i o) where
  show (Node s _ _ _ _) = "Node " ++ s

data Point = Point

instance Show Point where
  show Point = ""

class Trans s funs nodes where
  trans :: [(WindowType, TriggerType)] -> Proxy s -> funs -> nodes

instance
  (Get o ~ t, Trans xs fs ns, KnownSymbol s, CreateEmptyList i) =>
  Trans
    ('(s, i, o) ': xs)
    (fs :<|> (CList i -> IO t))
    (ns :<|> Node s i o)
  where
  trans ((wt, tt) : ts) _ (fs :<|> f) =
    trans ts (Proxy :: Proxy xs) fs
      :<|> Node
        (symbolVal (Proxy :: Proxy s))
        wt
        tt
        (unsafePerformIO (newChan :: IO (Chan v)))
        f

instance Trans '[] Point Point where
  trans _ _ _ = Point

s2wt :: State s -> [(WindowType, TriggerType)]
s2wt Base = []
s2wt (a :> (IN x y)) = s2wt a ++ [(x, y)]

----------------------------------------
cc = cr tv

runCmd :: IO ()
runCmd = shelly $ do
  run_ "dot" ["-Tpng", "-o", "wf.png", "wf.dot"]
  run_ "open" ["wf.png"]

cr :: (HasState s) => State s -> IO ()
cr s = do
  let r = fun1 Proxy s
  createDot r
  runCmd

fun1 :: (HasState s) => Proxy s -> State s -> Map String [String]
fun1 p _ = has p

createDot :: Map String [String] -> IO ()
createDot m = writeFile "wf.dot" res
  where
    ls = Data.Map.toList m
    loop [] = ""
    loop ((name, outList) : xs) =
      name
        ++ " -> {"
        ++ (concat $ intersperse "," outList)
        ++ "}\n"
        ++ loop xs
    res = "digraph test{\n" ++ loop ls ++ "\n}"

class HasState s where
  has :: Proxy s -> Map String [String]

instance (H1 x, HasState xs) => HasState (x ': xs) where
  has _ = insert k a $ has (Proxy :: Proxy xs)
    where
      (k, a) = h1 (Proxy :: Proxy x)

instance HasState '[] where
  has _ = empty

class H1 s where
  h1 :: Proxy s -> (String, [String])

instance (KnownSymbol s, H2 ss) => H1 '(s, ys, '(t, ss)) where
  h1 _ = (symbolVal (Proxy :: Proxy s), h2 (Proxy :: Proxy ss))

class H2 s where
  h2 :: Proxy s -> [String]

instance (KnownSymbol s, H2 ss) => H2 (s ': ss) where
  h2 _ = symbolVal (Proxy :: Proxy s) : h2 (Proxy :: Proxy ss)

instance H2 '[] where
  h2 _ = []

class H3 s where
  h3 :: Proxy s -> [String]

instance H2 xs => H3 '(t, xs) where
  h3 _ = h2 (Proxy :: Proxy xs)
