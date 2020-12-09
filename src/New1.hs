{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module New1 where

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
    "SELECT sum(c), mean(sum(c)), show(c) FROM Person GROUP BY TIMEWINDOW(5) "

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

sumFun2 :: IORef (Map TimeWindow Dynamic) -> Tdynamic -> IO Tdynamic
sumFun2 stref (t, dyn) = do
  m <- readIORef stref
  let ks = M.keys m
  case filter (inTimeWindow t) ks of
    [] -> do
      print $ "there is not the window s" ++ show m
      let cw = createWindow t
          tv = 0 + dyn
          m1 = M.insert cw tv m
      writeIORef stref m1
      return (t, tv)
    [x] -> case M.lookup x m of
      Nothing -> error "error happened"
      (Just d) -> do
        let tv = d + dyn
        writeIORef stref (M.insert x tv m)
        print m
        return (t, tv)

maen :: IORef (Map TimeWindow (Int, Dynamic)) -> Tdynamic -> IO Tdynamic
maen stref (t, dyn) = do
  m <- readIORef stref
  let ks = M.keys m
  case filter (inTimeWindow t) ks of
    [] -> do
      print $ "there is not the window s" ++ show m
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
        print m
        return (t, me)

init :: a -> (IORef a -> Tdynamic -> IO Tdynamic) -> IO (Tdynamic -> IO Tdynamic)
init d f = do
  v <- newIORef d
  return $ f v

runQueryExpr3 :: ScalarExpr -> IO (Tdynamic -> IO Tdynamic)
runQueryExpr3 (NumLit s) = return $ \(t, _) -> return $ (t, Double $ read s)
runQueryExpr3 (Iden ns) = return $ \(t, d) -> return $ (t, mkName d ns)
runQueryExpr3 (BinOp e1 ns e2) =
  case ns of
    [Name _ "+"] -> do
      v1 <- runQueryExpr3 e1
      v2 <- runQueryExpr3 e2
      return $ \d1@(t, _) -> do
        (_, v1') <- v1 d1
        (_, v2') <- v2 d1
        return $ (t, v1' + v2')
runQueryExpr3 (App [Name _ calc] [e2]) = do
  e2' <- runQueryExpr3 e2
  sum <- case calc of
    "sum" -> New1.init M.empty sumFun2
    "mean" -> New1.init M.empty maen
    "show" -> return $ \d -> print d >> return d
  return $ \d -> do
    e2'' <- e2' d
    sum e2''

runStatement3 :: [Tdynamic] -> Statement -> IO ([[Tdynamic]])
runStatement3 ds' (SelectStatement Select {..}) =
  let [TRSimple ns] = qeFrom
      ta = map fst qeSelectList
      ta1 = map runQueryExpr3 ta
   in do
        xs' <- sequence ta1
        forM ds' $ \(t, ds) ->
          forM xs' $ \x -> x $ (t, mkName ds ns)

res3 = runStatement3 dd2 hs

myHm :: HashMap Text Dynamic
myHm = HM.fromList [("a", Dictionary $ HM.fromList $ [("value", Double 1)]), ("b", Double 1), ("c", Double 1)]

myHm1 :: HashMap Text Dynamic
myHm1 = HM.fromList [("a", Dictionary $ HM.fromList $ [("value", Double 2)]), ("b", Double 1), ("c", Double 2)]

dd :: Int -> Tdynamic
dd i = (Time i, Dictionary $ HM.fromList [("Person", Dictionary myHm)])

dd1 :: Int -> Tdynamic
dd1 i = (Time i, Dictionary $ HM.fromList [("Person", Dictionary myHm1)])

dd2 :: [Tdynamic]
dd2 =
  [ dd1 1,
    dd 2,
    dd 5,
    dd 6,
    dd 7,
    dd 8,
    dd 3,
    dd 4,
    dd 9,
    dd 10,
    dd 11,
    dd 12
  ]
