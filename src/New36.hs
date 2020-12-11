{-# LANGUAGE TemplateHaskell #-}

module New36 where

import qualified Control.Monad.State as S
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Language.Haskell.TH hiding (Name)
import Language.Haskell.TH.Quote

dgraph :: QuasiQuoter
dgraph =
  QuasiQuoter
    { quoteExp = \x -> [|$(test x)|], -- [|parseInput x|],
      quotePat = error "no used",
      quoteType = error "no used",
      quoteDec = error "no used"
    }

sp :: String -> [String]
sp [] = []
sp s =
  a : case b of
    [] -> []
    _ -> sp (drop 1 b)
  where
    (a, b) = break (== ',') s

removeEmpty :: String -> [String]
removeEmpty "" = []
removeEmpty s = [s | not $ all (== ' ') s]

type Name = String

type Fun = String

type Parent = String

type InitState = String

type Backend = String

type Time = String

type CacheFun = String

data Mark
  = Normal Name Fun Parent
  | Choise Name Fun Parent
  | Filter Name Fun Parent
  | Sink Name Fun Parent
  | StateFun Name Fun InitState Backend Parent
  | Process Name Fun InitState Backend Time [Parent]
  | Mutil Name [Parent]
  | Source Name Fun
  deriving (Show)

getParents :: Mark -> [Parent]
getParents (Normal _ _ p) = [p]
getParents (Choise _ _ p) = [p]
getParents (Filter _ _ p) = [p]
getParents (Sink _ _ p) = [p]
getParents (StateFun _ _ _ _ p) = [p]
getParents (Process _ _ _ _ _ ps) = ps
getParents (Mutil _ ps) = ps
getParents (Source _ _) = []

isSource :: Mark -> Bool
isSource (Source _ _) = True
isSource _ = False

-- parseInput :: String -> ExpQ
parseInput s = (cp, pc, sources)
  where
    s1 = map (parseLine . map words . sp) $ concatMap removeEmpty $ lines s
    cp = foldl' foldFun M.empty s1
    pc = reverseCP cp
    sources = findSources cp

parseLine :: [[String]] -> (Name, Mark)
parseLine [["Normal"], [name], [fun], [parent]] = (name, Normal name fun parent)
parseLine [["Choise"], [name], [fun], [parent]] = (name, Choise name fun parent)
parseLine [["Filter"], [name], [fun], [parent]] = (name, Filter name fun parent)
parseLine [["Sink"], [name], [fun], [parent]] = (name, Sink name fun parent)
parseLine [["StateFun"], [name], [fun], [init], [backend], [parent]] =
  (name, StateFun name fun init backend parent)
parseLine [["Process"], [name], [fun], [init], [backend], [time], ps] =
  (name, Process name fun init backend time ps)
parseLine [["Mutil"], [name], ps] = (name, Mutil name ps)
parseLine [["Source"], [name], [fun]] = (name, Source name fun)
parseLine e = error $ show e

type CP = Map Name Mark

type PC = Map Name [Name]

foldFun :: CP -> (Name, Mark) -> CP
foldFun m (name, mark) =
  case M.lookup name m of
    Just _ -> error "node arealy exist"
    Nothing -> M.insert name mark m

reverseCP :: CP -> PC
reverseCP m = foldl' (createFun context) M.empty names
  where
    (names, context) = (\v -> (map fst v, map (\v' -> (getParents $ snd v', fst v')) v)) $ M.toList m

createFun :: [([Parent], Name)] -> PC -> Name -> PC
createFun context m name =
  case M.lookup name m of
    Just _ -> error "node arealy exist"
    Nothing ->
      case filter (elem name . fst) context of
        [] -> M.insert name [] m
        xs -> M.insert name (map snd xs) m

findSources :: CP -> [Name]
findSources m = map fst $ M.toList $ M.filter isSource m

test x =
  doE
    [ noBindS $ appE (appE (varE $ mkName "writeIORef") (varE $ mkName x)) (litE $ integerL 2020),
      noBindS $ appE (varE $ mkName "readIORef") $ varE $ mkName x
    ]

data HT a = HT [a] [a] deriving (Show)

addHead :: a -> HT a -> HT a
addHead a (HT xs ys) = HT (a : xs) ys

addTail :: a -> HT a -> HT a
addTail a (HT xs ys) = HT xs (a : ys)

lookChildren :: Name -> PC -> [Name]
lookChildren name pc = fromMaybe [] $ M.lookup name pc

createExpQ :: Name -> CP -> PC -> S.State (Map Name ExpQ, HT StmtQ) ExpQ
createExpQ name cp pc =
  case M.lookup name cp of
    Nothing -> error "can't find the node"
    Just mark ->
      case mark of
        Normal name fun _ -> do
          v <- case lookChildren name pc of
            [] -> error "Normal node need children node"
            xs -> mapM (\n -> createExpQ n cp pc) xs
          return undefined




