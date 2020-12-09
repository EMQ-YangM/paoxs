{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module New4 where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.Reader
import Control.Monad.ST
import Control.Monad.State
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as M
import Data.STRef
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Dynamic
import Language.SQL.SimpleSQL.Dialect (ansi2011)
import Language.SQL.SimpleSQL.Parse
import Language.SQL.SimpleSQL.Pretty (prettyStatements)
import Language.SQL.SimpleSQL.Syntax
import System.IO.Unsafe

t :: Either ParseError [Statement]
t =
  parseStatements
    ansi2011
    "hello SQL"
    Nothing
    "SELECT sum(sum(sum(sum(c)))) FROM Person GROUP BY TIMEWINDOW(5) "

-- "SELECT sum(sum(sum(c))) FROM Person GROUP BY TIMEWINDOW(5) "

-- "SELECT  sum(c), sum(sum(c)), sum(sum(sum(a.value))), c as ccc FROM Person "

-- "SELECT sum(a.value), sub(b), sum(b), c>10, sum(c+20) as ccc FROM Person GROUP BY swindow(5) "

----       Input channel  Output Channel
---- source  I1 Person  O5 a.value b b c c+20
---- sum(a.value) I1 a.value O1
---- sub(b) I1 b O1
---- thread chan chan

v :: String
v =
  let Right r = t
   in prettyStatements ansi2011 r

hs :: Statement
hs =
  let Right r = t
   in head r

mkName :: Dynamic -> [Name] -> Dynamic
mkName d [] = d
mkName d ((Name _ n) : xs) = mkName (d ! (String $ T.pack n)) xs

-- 怎么描述窗口之间存在依赖关系
-- 暂时不考虑窗口之间的依赖关系

data Time = Time Int deriving (Show, Eq, Ord)

data TimeWindow = TimeWindow Int Int deriving (Show, Ord, Eq)

type Tdynamic = (Time, Dynamic)

inTimeWindow :: Time -> TimeWindow -> Bool
inTimeWindow (Time t) (TimeWindow s e) =
  if t < e && t >= s
    then True
    else False

createWindow :: Time -> TimeWindow
createWindow (Time t) = TimeWindow (v * 5) ((v + 1) * 5)
  where
    v = t `div` 5

-- sumFun3 :: IORef (Map TimeWindow Dynamic)
-- Chan Input, Chan Output, State
-- Process
type Input = Chan Tdynamic

type Output = Chan Tdynamic

sumFun3 ::
  Input ->
  Output ->
  IORef (Map TimeWindow [Dynamic]) ->
  (Tdynamic -> IO Tdynamic) ->
  IO ()
sumFun3 ic oc stref func = do
  v'@(jt, _) <- readChan ic
  m <- readIORef stref
  case v' of
    (_, Calc s e) -> case M.lookup (TimeWindow s e) m of
      Nothing -> error "this impossible"
      Just v -> do
        let tal = sum v
            val = tal
        writeChan oc (Time $ (s + e) `div` 2, val)
    (t, PefWatermark) -> do
      let cw = createWindow t
      case M.lookup cw m of
        Nothing -> error "error happened"
        Just v -> do
          let tal = sum v
              val = tal
          writeChan oc (t, val)
          writeChan oc (t, PefWatermark)
    vv@(t, _) -> do
      let ks = M.keys m
      (_, v1) <- func vv
      case filter (inTimeWindow t) ks of
        [] -> do
          let cw@(TimeWindow a b) = createWindow t
              m1 = M.insert cw [v1] m
          writeIORef stref m1
          timeOut (a, b) 1 ic
        [x] ->
          case M.lookup x m of
            Nothing -> error "error happened"
            Just d -> do
              writeIORef stref (M.insert x (v1 : d) m)
  sumFun3 ic oc stref func

-- Map TimeWindow (Tdynamic -> IO Tdynamic)
-- fun :: OldState -> Dynamic -> (NewState, Dynamic)?? 有点问题 无IO
-- 是否向下继续输出？
-- fun :: OldState -> Dynamic -> IO (NewState, Dynamic)？？ 有点问题 有IO

maen :: IORef (Map TimeWindow (Int, Dynamic)) -> Tdynamic -> IO Tdynamic
maen stref (t, dyn) = do
  m <- readIORef stref
  let ks = M.keys m
  case filter (inTimeWindow t) ks of
    [] -> do
      let cw = createWindow t
          tv = dyn
          m1 = M.insert cw (1, tv) m
      writeIORef stref m1
      return (t, tv)
    [x] -> case M.lookup x m of
      Nothing -> error "error happened"
      (Just (i, d)) -> do
        let tv = d + dyn
            me = tv / fromIntegral (i + 1)
        writeIORef stref (M.insert x (i + 1, tv) m)
        -- print m
        return (t, me)

init :: a -> (IORef a -> Tdynamic -> IO Tdynamic) -> IO (Tdynamic -> IO Tdynamic)
init d f = do
  v <- newIORef d
  return $ f v

data Re
  = Re (Tdynamic -> IO Tdynamic)
  | Rd (Chan Tdynamic) (Chan Tdynamic)

-- runQueryExpr3 :: ScalarExpr -> IO (Chan, Chan)

runQueryExpr3 :: ScalarExpr -> IO Re --(Tdynamic -> IO Tdynamic)
runQueryExpr3 (NumLit s) = return $ Re $ \(t, _) -> return $ (t, Double $ read s)
runQueryExpr3 (Iden ns) = return $ Re $ \(t, d) -> return $ (t, mkName d ns)
runQueryExpr3 (BinOp e1 ns e2) =
  case ns of
    [Name _ "+"] -> do
      v1' <- runQueryExpr3 e1
      v2' <- runQueryExpr3 e2
      case (v1', v2') of
        (Re v1, Re v2) -> return $
          Re $ \d1@(t, _) -> do
            (_, v1') <- v1 d1
            (_, v2') <- v2 d1
            return $ (t, v1' + v2')
        (Rd ic oc, Re v2) -> do
          oc1 <- newChan
          return $ error "nice"
runQueryExpr3 (App [Name _ calc] [e2]) = do
  res <- runQueryExpr3 e2
  print e2
  case res of
    Re e2' -> do
      ic <- newChan
      oc <- newChan
      case calc of
        "sum" -> do
          ref <- newIORef M.empty
          print "start Re fun"
          forkIO $ sumFun3 ic oc ref e2'
          return $ Rd ic oc
    Rd ic oc -> do
      case calc of
        "sum" -> do
          oc1 <- newChan
          ref <- newIORef M.empty
          print "start state sum"
          forkIO $ sumFun3 oc oc1 ref (\x -> return x)
          return $ Rd ic oc1

runState1 :: Statement -> IO [Re]
runState1 (SelectStatement Select {..}) =
  let [TRSimple ns] = qeFrom
      ta = map fst qeSelectList
      ta1 = map runQueryExpr3 ta
   in sequence ta1

gp :: Chan Tdynamic -> IO ()
gp oc = loop
  where
    loop = do
      v <- readChan oc
      print v
      loop

test = do
  [Rd ic oc] <- runState1 hs
  forkIO $
    forM_ dd2 $ \(t, v) -> do
      --   print $ (t, v ! "Person")
      writeChan ic (t, v ! "Person")
      threadDelay (10 ^ 6 * 0)
  loop oc
  where
    loop oc = do
      v <- readChan oc
      print v
      loop oc

myHm :: HashMap Text Dynamic
myHm = HM.fromList [("a", Dictionary $ HM.fromList $ [("value", Double 1)]), ("b", Double 1), ("c", Double 1)]

myHm1 :: HashMap Text Dynamic
myHm1 = HM.fromList [("a", Dictionary $ HM.fromList $ [("value", Double 2)]), ("b", Double 1), ("c", Double 2)]

-- myHm1 :: HashMap Text Dynamic
-- myHm1 = HM.fromList [("a", Dictionary $ HM.fromList $ [("value", Double 2)]), ("b", Double 1), ("c", Double 2)]

dd :: Int -> Tdynamic
dd i = (Time i, Dictionary $ HM.fromList [("Person", Dictionary myHm)])

dd1 :: Int -> Tdynamic
dd1 i = (Time i, Dictionary $ HM.fromList [("Person", Dictionary myHm1)])

dd1' :: Int -> Tdynamic
dd1' i = (Time i, PefWatermark)

dd2 :: [Tdynamic]
dd2 =
  [ dd 1,
    dd 2,
    dd 5,
    dd 6,
    dd 7,
    dd 8,
    dd 3,
    dd1' 4,
    dd1' 9,
    dd 10,
    dd 11,
    dd 12
  ]

timeOut :: (Int, Int) -> Int -> Chan (Time, Dynamic) -> IO ()
timeOut (a, b) i m = do
  forkIO $ sleepThred
  return ()
  where
    sleepThred = do
      threadDelay (10 ^ 6 * i)
      writeChan m (Time 1000, Calc a b)
      sleepThred
