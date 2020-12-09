{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module New where

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
    "SELECT  sum(c), sum(sum(c)), sum(sum(sum(a.value))), c as ccc FROM Person "

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


sumFun1 :: IORef Dynamic -> Dynamic -> IO Dynamic
sumFun1 stref dyn = do
  v <- readIORef stref
  writeIORef stref (v + dyn)
  -- print v
  return (v + dyn)

mean :: IORef (Int, Dynamic) -> Dynamic -> IO Dynamic
mean stref dyn = do
  (num, all) <- readIORef stref
  let (num', all') = (num + 1, all + dyn)
  writeIORef stref (num', all')
  print (num, all)
  return (all' / fromIntegral num')

init :: a -> (IORef a -> Dynamic -> IO Dynamic) -> IO (Dynamic -> IO Dynamic)
init d f = do
  v <- newIORef d
  return $ f v

runQueryExpr3 :: ScalarExpr -> IO (Dynamic -> IO Dynamic)
runQueryExpr3 (NumLit s) = return $ \_ -> return $ Double $ read s
runQueryExpr3 (Iden ns) = return $ \d -> return $ mkName d ns
runQueryExpr3 (BinOp e1 ns e2) =
  case ns of
    [Name _ "+"] -> do
      v1 <- runQueryExpr3 e1
      v2 <- runQueryExpr3 e2
      return $ \d -> do
        v1' <- v1 d
        v2' <- v2 d
        return $ v1' + v2'
runQueryExpr3 (App [Name _ calc] [e2]) = do
  e2' <- runQueryExpr3 e2
  sum <- case calc of
    "sum" -> New.init 0 sumFun1
    "mean" -> New.init (0, 0) mean
  return $ \d -> do
    e2'' <- e2' d
    sum e2''

runStatement3 ds' (SelectStatement Select {..}) =
  let [TRSimple ns] = qeFrom
      ta = map fst qeSelectList
      ta1 = map runQueryExpr3 ta
   in do
        xs' <- sequence ta1
        forM ds' $ \ds ->
          forM xs' $ \x -> x $ mkName ds ns

res3 = runStatement3 (concat $ replicate 3 dd1) hs

myHm :: HashMap Text Dynamic
myHm = HM.fromList [("a", Dictionary $ HM.fromList $ [("value", Double 1)]), ("b", Double 1), ("c", Double 1)]

myHm1 :: HashMap Text Dynamic
myHm1 = HM.fromList [("a", Dictionary $ HM.fromList $ [("value", Double 2)]), ("b", Double 1), ("c", Double 1)]

dd :: Dynamic
dd = Dictionary $ HM.fromList [("Person", Dictionary myHm)]

dd1 :: [Dynamic]
dd1 =
  [ Dictionary $ HM.fromList [("Person", Dictionary myHm)],
    Dictionary $ HM.fromList [("Person", Dictionary myHm1)],
    Dictionary $ HM.fromList [("Person", Dictionary myHm1)]
  ]
