{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module New6 where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.Reader
import Control.Monad.ST
import Control.Monad.State as S
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
    "SELECT sum(*) FROM (SELECT sum(a.value) From Person where a.value+1 > 100 GROUP BY TIMEWINDOW(5)) GROUP BY TIMEWINDOW(5) "

-- "SELECT sum(c) + a  FROM Person GROUP BY TIMEWINDOW(5) "

-- "SELECT sum(sum(sum(c))) FROM Person GROUP BY TIMEWINDOW(5) "

-- "SELECT  sum(c), sum(sum(c)), sum(sum(sum(a.value))), c as ccc FROM Person "

-- "SELECT sum(a.value), sub(b), sum(b), c>10, sum(c+20) as ccc FROM Person GROUP BY swindow(5) "

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
  = Re (Tdynamic -> Tdynamic)
  | Rd (Chan Tdynamic) (Chan Tdynamic)

myHm :: HashMap Text Dynamic
myHm = HM.fromList [("a", Dictionary $ HM.fromList $ [("value", Double 1)]), ("b", Double 1), ("c", Double 1)]

myHm1 :: HashMap Text Dynamic
myHm1 = HM.fromList [("a", Dictionary $ HM.fromList $ [("value", Double 2)]), ("b", Double 1), ("c", Double 2)]

-- myHm1 :: HashMap Text Dynamic
-- myHm1 = HM.fromList [("a", Dictionary $ HM.fromList $ [("value", Double 2)]), ("b", Double 1), ("c", Double 2)]

dd :: Int -> Double -> Tdynamic
dd i j =
  ( Time i,
    Dictionary $
      HM.fromList
        [ ( "Person",
            Dictionary $
              HM.fromList [("a", Dictionary $ HM.fromList $ [("value", Double j)])]
          )
        ]
  )

dd1 :: Int -> Tdynamic
dd1 i = (Time i, Dictionary $ HM.fromList [("Person", Dictionary myHm1)])

dd1' :: Int -> Tdynamic
dd1' i = (Time i, PefWatermark)

barrier :: Int -> Int -> Tdynamic
barrier i j = (Time i, Barrier j)

dd2 :: [Tdynamic]
dd2 =
  [ barrier 1 0,
    dd 1 1,
    barrier 1 1,
    dd 2 2,
    barrier 1 2,
    dd 5 5,
    barrier 1 3,
    dd1' 4,
    barrier 1 4,
    dd 18 18,
    dd1' 19,
    barrier 1 5,
    dd1' 9,
    barrier 1 6
    -- dd 6,
    -- dd 7,
    -- dd 8,
    -- dd 3,
    -- dd1' 4,
    -- dd1' 9,
    -- barrier 100,
    -- dd 10,
    -- dd 11,
    -- dd 12
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

fresh :: MonadState Int m => m Int
fresh = do
  v <- S.get
  S.put (v + 1)
  return (v + 1)

trans :: MonadState Int m => ScalarExpr -> m MarkScalarExpr
trans (NumLit s) = fresh >>= \v -> return $ Mnumlit v s
trans (StringLit _ _ s) = fresh >>= \v -> return $ Mstringlit v s
trans (Iden ns) = fresh >>= \v -> return $ Mident v ns
trans (BinOp s1 ns s2) =
  fresh >>= \v -> do
    s1' <- trans s1
    s2' <- trans s2
    return $ Mbinop v s1' ns s2'
trans (App ns xs) =
  fresh >>= \v -> do
    xs' <- mapM trans xs
    return $ Mapp v ns xs'
trans Star = fresh >>= \v -> return $ Mstar v
trans e = error $ show e

tt =
  let SelectStatement Select {..} = hs
      sls = map fst qeSelectList
   in runState (sequence $ map trans sls) 0

sumFun4 ::
  Int ->
  Input ->
  Output ->
  IORef (Map TimeWindow [Dynamic]) ->
  (Tdynamic -> Tdynamic) ->
  IO ()
sumFun4 ident ic oc stref func = do
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
        Nothing -> do
          writeChan oc (t, 0)
          writeChan oc (t, PefWatermark)
        Just v -> do
          let tal = sum v
              val = tal
          writeChan oc (t, val)
          writeChan oc (t, PefWatermark)
    (t, Barrier i) -> do
      appendFile
        "save.txt"
        ( "  --------------------------------------------\n"
            ++ show i
            ++ "  "
            ++ show (ident, Barrier i, m)
            ++ "\n"
        )
      writeChan oc (t, Barrier i)
    vv@(t, _) -> do
      let ks = M.keys m
      (_, v1) <- return $ func vv
      case filter (inTimeWindow t) ks of
        [] -> do
          let cw@(TimeWindow a b) = createWindow t
              m1 = M.insert cw [v1] m
          writeIORef stref m1
        --   timeOut (a, b) 1 ic
        [x] ->
          case M.lookup x m of
            Nothing -> error "error happened"
            Just d -> do
              writeIORef stref (M.insert x (v1 : d) m)
  sumFun4 ident ic oc stref func

data MarkScalarExpr
  = Mnumlit Int String
  | Mstringlit Int String
  | Mident Int [Name]
  | Mbinop Int MarkScalarExpr [Name] MarkScalarExpr
  | Mapp Int [Name] [MarkScalarExpr]
  | Mstar Int
  deriving (Show)

runQ4 :: Input -> MarkScalarExpr -> IO Re
runQ4 _ (Mnumlit i s) = return $ Re $ \(t, _) -> (t, Double $ read s)
runQ4 _ (Mident i ns) = return $ Re $ \(t, d) -> (t, mkName d ns)
runQ4 c (Mbinop i e1 ns e2) = do
  case ns of
    [Name _ "+"] -> do
      v1' <- runQ4 c e1
      v2' <- runQ4 c e2
      case (v1', v2') of
        (Re v1, Re v2) -> return $
          Re $ \d1@(t, _) -> (t, snd (v1 d1) + snd (v2 d1))
    [Name _ ">"] -> do
      v1' <- runQ4 c e1
      v2' <- runQ4 c e2
      case (v1', v2') of
        (Re v1, Re v2) -> return $
          Re $ \d1@(t, _) -> (t, Bool $ snd (v1 d1) > snd (v2 d1))
runQ4 inputChan (Mapp i [Name _ calc] [e2]) = do
  res <- runQ4 inputChan e2
  print e2
  case res of
    Re e2' -> do
      let ic = inputChan
      oc <- newChan
      case calc of
        "sum" -> do
          ref <- newIORef M.empty
          print "start Re fun"
          forkIO $ sumFun4 i ic oc ref e2'
          return $ Rd ic oc
    Rd ic oc -> do
      case calc of
        "sum" -> do
          oc1 <- newChan
          ref <- newIORef M.empty
          print "start state sum"
          forkIO $ sumFun4 i oc oc1 ref id
          return $ Rd ic oc1
runQ4 _ (Mstar i) = return $ Re id
runQ4 _ e = error $ show e

filterFun :: Input -> (Tdynamic -> Tdynamic) -> IO Output
filterFun ic fun = do
  oc <- newChan
  forkIO $ loop oc
  return oc
  where
    loop oc = do
      v <- readChan ic
      case v of
        -- (_, Bool True) -> writeChan oc v
        (t, Barrier i) -> writeChan oc (t, Barrier i)
        (t, PefWatermark) -> writeChan oc (t, PefWatermark)
        _ -> case fun v of
          (_, Bool True) -> writeChan oc v
          _ -> return ()
      loop oc

runState4 :: Statement -> IO [Re]
runState4 (SelectStatement selec0) =
  let [TRQueryExpr selec1] = qeFrom selec0
   in do
        ic <- newChan
        let Just tw = qeWhere selec1
            (tw1, s1) = runState (trans tw) 0
        Re tw1' <- runQ4 ic tw1

        ocw <- filterFun ic tw1'

        let ta1 = map fst $ qeSelectList selec1
            (ta1', s) = runState (sequence $ map trans ta1) s1
            ta1'' = map (runQ4 ocw) ta1'

        [Rd ic1 oc1] <- sequence ta1''
        let ta0 = map fst $ qeSelectList selec0
            ta0' = fst $ runState (sequence $ map trans ta0) s
            ta0'' = map (runQ4 oc1) ta0'
        [Rd ic2 oc2] <- sequence ta0''

        return [Rd ic oc2]

test4 = do
  xs <- runState4 hs
  forkIO $ do
    ls <- forM xs $ \(Rd ic oc) -> do
      forM dd2 $ \(t, v) -> do
        threadDelay (3 * 5 ^ 6)
        writeChan ic (t, v ! "Person")
      return oc
    logChan <- newChan -- :: IO (Chan Dynamic)
    mapM forkIO $ map (loop logChan) ls
    forkIO $ printLog logChan
    return ()
  threadDelay (10 ^ 6 * 3)
  where
    loop lc ocs = do
      v <- readChan ocs
      writeChan lc v
      loop lc ocs
    printLog lc = do
      v <- readChan lc
      print v
      printLog lc