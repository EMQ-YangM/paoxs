{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Sql where

import Control.Monad.Reader
import Control.Monad.State
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Dynamic
import Language.SQL.SimpleSQL.Dialect
import Language.SQL.SimpleSQL.Parse
import Language.SQL.SimpleSQL.Pretty (prettyStatements)
import Language.SQL.SimpleSQL.Syntax

t :: Either ParseError [Statement]
t =
  parseStatements
    ansi2011
    "hello SQL"
    Nothing
    "SELECT sum(a.value + 1), sub(b), sum(b), c>10, c+20 as ccc from Person group by swindow(5) "

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

myHm :: HashMap Text Dynamic
myHm = HM.fromList [("a", Dictionary $ HM.fromList $ [("value", Double 1)]), ("b", Double 1), ("c", Double 1)]

dd :: Dynamic
dd = Dictionary $ HM.fromList [("Person", Dictionary myHm)]

-- 怎么描述窗口之间存在依赖关系
-- 暂时不考虑窗口之间的依赖关系

data Time = Time Int

data Rosetree a = Rosetree a [Rosetree a] deriving (Show, Eq, Ord)

data Input = Input Time Dynamic

data ProcState = ProcState (Map String Dynamic) deriving (Show, Eq, Ord)

data Output = Output Time Dynamic

data Env = Env

type MT = ReaderT Env (StateT ProcState IO)

mkstr :: String -> ScalarExpr -> String
mkstr v (Iden [Name _ s]) = v ++ " iden " ++ s
mkstr v (Iden xs) = v ++ (concatMap (\(Name _ s) -> s ++ " ") xs)
mkstr v (BinOp _ _ _) = v ++ " Binop "

runQueryExpr1 :: Dynamic -> ScalarExpr -> MT Dynamic
runQueryExpr1 _ (NumLit s) = return $ Double $ read s
runQueryExpr1 d (Iden ns) = return $ mkName d ns
runQueryExpr1 d (BinOp e1 ns e2) =
  case ns of
    [Name _ "+"] -> (+) <$> runQueryExpr1 d e1 <*> runQueryExpr1 d e2
    [Name _ ">"] -> Bool <$> ((>) <$> runQueryExpr1 d e1 <*> runQueryExpr1 d e2)
runQueryExpr1 d (App [Name _ sum] [e2]) = do
  e2' <- runQueryExpr1 d e2
  let nm = mkstr sum e2
  state
    ( \(ProcState m) ->
        ( undefined,
          ProcState $
            case M.lookup nm m of
              Nothing -> M.insert nm (conv sum 0 e2') m
              Just v -> M.insert nm (conv sum v e2') m
        )
    )
  ProcState v <- Control.Monad.State.get
  let Just v' = M.lookup nm v
  liftIO $ print $ nm ++ "  " ++ show v'
  return v'

conv :: (Num a, Fractional a) => String -> a -> a -> a
conv "mul" = (*)
conv "sum" = (+)
conv "sub" = (-)
conv "div" = (/)

runStatement1 ds (SelectStatement Select {..}) =
  let [TRSimple ns] = qeFrom
   in runStateT
        ( runReaderT
            ( forM ds $ \d -> mapM (runQueryExpr1 (mkName d ns) . fst) qeSelectList
            )
            Env
        )
        (ProcState M.empty)

res1 = runStatement1 (replicate 10 dd) hs