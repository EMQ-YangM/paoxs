{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module New20 where

import Codec.Serialise
import Control.Concurrent (Chan, forkIO, newChan, threadDelay, writeChan)
import Control.Concurrent.Chan (readChan)
import Control.Monad (replicateM, void, when)
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (forM_)
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, newIORef)
import qualified Data.IORef as R
import Data.Map hiding ((!))
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Data.Time
import Data.Traversable (forM)
import qualified Data.Vector as V
import Dynamic
import GHC.IORef (readIORef)
import New18
import System.IO.Unsafe
import System.Random
import Data.IORef (writeIORef)

data WindowType = WindowType -- three winow

data DisStrategy = DisStrategy -- Distribution strategy

-- create process info
-- source (name,  IORef [(String, ProcessChan)] -> IO ()  , DisStrategy )
-- sink (name, input, Dynamic -> IO(), immediately process)
-- work (name, inputs, s -> HashMap String [Dynamic] -> IO (s, Dynamic), DisStrategy)
-- immediatelyWork (name, input, s -> Dynamic -> IO (s, Dynamic), DisStrategy, , immediately process)
-- filterWork (name, Dynamic -> Bool, immediately process)

-- run process state
-- source (IORef [(String, ProcessChan)])
-- sink (ProcessChan)
-- work (ProcessChan, s, HashMap String [Dynamic], IORef [(String, ProcessChan)])
-- immediatelyWork (ProcessChan, s, IORef [(String, ProcessChan)])
-- filterWokr (Dynamic -> Bool)

type Input = String

logChan :: Chan String
logChan = unsafePerformIO newChan
{-# NOINLINE logChan #-}

type ProcessChan = Chan (String, Dynamic)

forkSource ::
  IORef [(String, ProcessChan)] ->
  (IORef [(String, ProcessChan)] -> IO ()) ->
  IO ()
forkSource ref f = void $ forkIO (f ref)

forkSink :: ProcessChan -> (Dynamic -> IO ()) -> IO ()
forkSink chan f = void $ forkIO loop
  where
    loop = do
      v <- readChan chan
      f (snd v)
      loop

forkImmediately ::
  String ->
  ProcessChan ->
  IORef [(String, ProcessChan)] ->
  s ->
  (s -> Dynamic -> IO (s, Dynamic)) ->
  IO ()
forkImmediately name chan ref s fun = void $ forkIO $ loop s
  where
    loop state = do
      v <- readChan chan
      (ns, dy) <- fun state $ snd v
      m <- readIORef ref
      forM_ m $ \(_, c) -> do
        writeChan c (name, dy)
      loop ns

forkFilter ::
  String ->
  ProcessChan ->
  IORef [(String, ProcessChan)] ->
  (Dynamic -> Bool) ->
  IO ()
forkFilter name chan ref fun = void $ forkIO loop
  where
    loop = do
      v <- readChan chan
      when (fun $ snd v) $
        do
          m <- readIORef ref
          forM_ m $ \(_, c) -> do
            writeChan c (name, snd v)
      loop

forkWorker ::
  String ->
  ProcessChan ->
  IORef [(String, ProcessChan)] ->
  s ->
  (s -> [[Dynamic]] -> IO (s, Dynamic)) ->
  [Input] ->
  IO ()
forkWorker name chan ref s fun inputs =
  void $ forkIO $ loop s empty -- Map String [Dynamic]
  where
    loop os cache = do
      (name1, dy) <- readChan chan
      case dy of
        Dynamic.Calculation -> do
          let vs = Prelude.map (`Data.Map.lookup` cache) inputs
              xs = Prelude.map (fromMaybe []) vs
          (ns, dy) <- fun os xs
          m <- readIORef ref
          forM_ m $ \(_, c) -> do
            writeChan c (name, dy)
            loop ns empty
        _ -> do
          let ncache = insertWith (++) name1 [dy] cache
          loop os ncache

-- Manager Process
-- Map String [IORef [(String, ProcessChan)]]
--
forkManager :: Chan Command -> IO ()
forkManager chan = void $ forkIO $ cManager empty
  where
    cManager m = do
      readChan chan >>= \case
        CreateSource name fun -> do
          writeChan logChan $ "create source " ++ name
          ref <- newIORef []
          forkSource ref fun
          cManager (insert name ref m)
        CreateSink name input fun -> do
          case Data.Map.lookup input m of
            Nothing -> error "strange error happened"
            Just ref -> do
              writeChan logChan $ "create sink " ++ name
              chan <- newChan
              R.atomicModifyIORef ref (\v -> ((name, chan) : v, ()))
              r <- newIORef []
              forkSink chan fun
              cManager (insert name r m)
        CreateImmediately name input state fun -> do
          case Data.Map.lookup input m of
            Nothing -> error "strange error happened"
            Just ref -> do
              writeChan logChan $ "create immediate " ++ name
              chan <- newChan
              R.atomicModifyIORef ref (\v -> ((name, chan) : v, ()))
              r <- newIORef []
              forkImmediately name chan r state fun
              cManager (insert name r m)
        CreateFilter name input fun -> do
          case Data.Map.lookup input m of
            Nothing -> error "strange error happened"
            Just ref -> do
              writeChan logChan $ "create filter " ++ name
              chan <- newChan
              R.atomicModifyIORef ref (\v -> ((name, chan) : v, ()))
              r <- newIORef []
              forkFilter name chan r fun
              cManager (insert name r m)
        CreateWorker name inputs state fun tt -> do
          case mapM (`Data.Map.lookup` m) inputs of
            Nothing -> error "strange error, don't find the input"
            Just ls -> do
              writeChan logChan $ "create worker " ++ name
              chan <- newChan
              forM_ ls $ \ref -> R.atomicModifyIORef ref (\v -> ((name, chan) : v, ()))
              r <- newIORef []
              forkWorker name chan r state fun inputs
              case tt of
                TA -> undefined
                Reapet i -> do
                  forkIO $ createTimer i Dynamic.Calculation chan
                  cManager (insert name r m)
        CreateDot -> do
          m' <- New20.trans m
          createDot m'
          runCmd
          cManager m
        Command -> undefined

trans :: Map String (IORef [(String, ProcessChan)]) -> IO (Map String [String])
trans m = do
  let ls = Data.Map.toList m
  ls' <- forM ls $ \(k, ref) -> do
    v <- readIORef ref
    return (k, Prelude.map fst v)
  return $ Data.Map.fromList ls'

create :: String -> IO Dynamic -> IORef [(String, ProcessChan)] -> IO ()
create name dy ref = do
  m <- readIORef ref
  dy' <- dy
  forM_ m $ \(_, c) -> do
    writeChan c (name, dy')
  threadDelay (1 * 10 ^ 3)
  create name dy ref

createTimer :: Int -> Dynamic -> ProcessChan -> IO ()
createTimer i m chan =
  void $ forkIO (timer i (writeChan chan ("timer", m)))

data Command
  = CreateSource String (IORef [(String, ProcessChan)] -> IO ())
  | CreateSink String Input (Dynamic -> IO ())
  | forall s. CreateImmediately String Input s (s -> Dynamic -> IO (s, Dynamic))
  | CreateFilter String Input (Dynamic -> Bool)
  | forall s. CreateWorker String [Input] s (s -> [[Dynamic]] -> IO (s, Dynamic)) New18.TriggerType
  | CreateDot
  | Command

test1 :: IO ()
test1 = do
  chan <- newChan
  forkManager chan

  writeChan chan $ CreateSource "source_person" (create "source_person" $ randomPerson 1 5)
  writeChan chan $ CreateSource "source_order" (create "source_order" $ randomOrder 1 5)
--writeChan chan $ CreateFilter "filterp" "source_person" (\p -> p ! "age" > 1)
--writeChan chan $ CreateFilter "filtero" "source_order" (\p -> p ! "orderNumber" > 490)
  writeChan chan $
    CreateWorker
      "join"
      ["source_person", "source_order"]
      (Double 0)
      (\s [xs, ys] -> return (s, join xs ys ejoinFun cjoinFun))
      (Reapet 1)
--writeChan chan $ CreateSink "sink1" "join" (\v -> writeChan logChan ("sink1 " ++ show v))
--writeChan chan $
--  CreateWorker
--    "total_person"
--    ["source_person"]
--    (Double 0)
--    (\x [xs] -> return (x + fromIntegral (length xs), x + fromIntegral (length xs)))
--    (Reapet 3)
--writeChan chan $
--  CreateWorker
--    "total_order"
--    ["source_order"]
--    (Double 0)
--    (\x [xs] -> return (x + fromIntegral (length xs), x + fromIntegral (length xs)))
--    (Reapet 3)
--writeChan chan $ CreateSink "sink2" "total_person" (\v -> writeChan logChan ("total person -----> " ++ show v))
--writeChan chan $ CreateSink "sink3" "total_order" (\v -> writeChan logChan ("total order -----> " ++ show v))
--writeChan chan $ CreateFilter "filtermy" "source_person" (\p -> p ! "age" > 49)
--writeChan chan $ CreateSink "sink4" "filtermy" (\v -> writeChan logChan ("warning age > 45 " ++ show v))
  writeChan chan $ CreateSink "sink" "join" (\_ -> writeChan logChan ("warning age > 45 " ))

  writeChan chan CreateDot
  handleLog
  where
    handleLog = do
      v <- readChan logChan
   -- putStrLn v
      handleLog

test2 :: IO ()
test2 = do
  chan <- newChan
  forkManager chan
  loop chan
  where
    loop c = do
      ws <- getLine
      case words ws of
        "source" : name : _ -> writeChan c $ CreateSource name (create name $ randomPerson 1 5)
        "filter" : name : input : _ -> writeChan c $ CreateFilter name input (\p -> p ! "age" > 49)
        "sink" : name : input : _ ->
          writeChan c $ CreateSink name input (writeChan logChan . show)
      writeChan c CreateDot
      loop c

person :: Dynamic
person =
  Dictionary $
    HM.fromList
      [ ("name", Dynamic.String "yang"),
        ("id", Double 1),
        ("age", Double 23)
      ]

order :: Dynamic
order =
  Dictionary $
    HM.fromList
      [ ("orderNumber", Double 1993),
        ("personId", Double 1)
      ]

join ::
  [Dynamic] ->
  [Dynamic] ->
  (Dynamic -> Dynamic -> Bool) ->
  (Dynamic -> Dynamic -> Dynamic) ->
  Dynamic
join l r ef cf = Dynamic.Array $ V.fromList $ join' l r ef cf

join' ::
  [Dynamic] ->
  [Dynamic] ->
  (Dynamic -> Dynamic -> Bool) ->
  (Dynamic -> Dynamic -> Dynamic) ->
  [Dynamic]
join' (l : ls) rs efun cfun =
  case inrs rs of
    Nothing -> join' ls rs efun cfun
    Just x -> cfun l x : join' ls rs efun cfun
  where
    inrs :: [Dynamic] -> Maybe Dynamic
    inrs (x : xs) = if efun l x then Just x else inrs xs
    inrs [] = Nothing
join' [] _ _ _ = []

ejoinFun :: Dynamic -> Dynamic -> Bool
ejoinFun l r = l ! "id" == r ! "personId"

cjoinFun :: Dynamic -> Dynamic -> Dynamic
cjoinFun l r =
  Dynamic.Array $
    V.fromList
      [ l ! "name",
        l ! "age",
        r ! "orderNumber"
      ]

tj = join' [person] [order] ejoinFun cjoinFun

randomString :: IO String
randomString = Control.Monad.replicateM 3 $ randomRIO ('a', 'z')

randomPerson :: Int -> Int -> IO Dynamic
randomPerson lv hv = do
  id <- randomRIO (lv, hv)
  name <- randomString
  age <- randomRIO (20, 50) :: IO Int
  return $
    Dictionary $
      HM.fromList
        [ ("name", Dynamic.String $ pack name),
          ("id", Double $ fromIntegral id),
          ("age", Double $ fromIntegral age)
        ]

randomOrder :: Int -> Int -> IO Dynamic
randomOrder lv hv = do
  number <- randomRIO (10, 1000) :: IO Int
  id <- randomRIO (lv, hv)
  return $
    Dictionary $
      HM.fromList
        [ ("orderNumber", Double $ fromIntegral number),
          ("personId", Double $ fromIntegral id)
        ]

tempScheme :: [String]
tempScheme = ["UTCTime", "Int"]

data Temp = Temp
  { time :: UTCTime,
    temp :: Double
  }

encodeTemp :: Temp -> BL.ByteString
encodeTemp (Temp time temp) = lt1 <> lt2 <> time' <> temp'
  where
    time' = serialise time
    temp' = serialise temp
    lt1 = serialise $ BL.length $ time'
    lt2 = serialise $ BL.length temp'


--dec :: String -> BL.ByteString ->  (forall s. s -> IO ()) -> IO ()
--dec "Int" bs func = func (deserialise bs :: Int)
--dec "Double" bs func = func (deserialise bs :: Double)
--
--
--
--add1 :: s -> IO ()
--add1 x = print $ (unsafeCoerce x :: Int) + 1
--
--tttt = dec "Double" (serialise (1 :: Double)) add1

--dec "[Int]" bs =  deserialise bs :: [Int]

-- dec1 s bs =  deserialise bs :: s

--dec :: String -> BL.ByteString -> s
--dec "Int"    bs  = unsafeCoerce (deserialise bs :: Int)
--dec "Double" bs  = unsafeCoerce (deserialise bs :: Double)
--dec "String" bs  = unsafeCoerce (deserialise bs :: String)
--
--add1 :: Int -> Int
--add1 x = x + 1
--
--tttt = add1 $ dec "String" (serialise ("babaa" :: String))

--newtype Age = MkAge { unAge :: Int }
--
--type family Inspect x
--type instance Inspect Age = Bool
--type instance Inspect Int = Bool
--
--class BadIdea a where
--  bad :: a -> Inspect a
--
--instance BadIdea Int where
--  bad = (> 0)
--
--deriving instance BadIdea Age
--


