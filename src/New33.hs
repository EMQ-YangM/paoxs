{-# LANGUAGE TemplateHaskell #-}

module New33 where

import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Data.Map (Map)
import qualified Data.Map as M
import Data.List
import GHC.IORef
import System.IO.Unsafe
import New31
import GHC.Conc
import Data.Coerce (coerce)
import Language.Haskell.TH.Syntax (liftData)

q :: QuasiQuoter 
q = QuasiQuoter {
    quoteExp  = parseInput
  , quotePat = error "nice"
  , quoteType = error "nice"
  , quoteDec = error "nice"
}

parseInput s = createE sourceNode (ctop m) m (unsafePerformIO $ newIORef M.empty) -- (m, ctop m, sourceNode) -- createExpr (head sinkNodes) m
    where s1 = map (map words . sp) $ concatMap removeEmpty $ lines s
          m = foldl' foldFunc M.empty s1
          sourceNode = getSourceNode m

sp :: String -> [String]
sp [] = []
sp s = a : case b of 
                [] -> []
                _ -> sp (drop 1 b)
   where (a, b) = break (== ',') s 

removeEmpty :: String -> [String]
removeEmpty "" = []
removeEmpty s = [s | not $ all (==' ') s]

type MChildToParents = Map String ([String], String)

type MParentToChild = Map String ([String], String)

foldFunc :: MChildToParents -> [[String]] -> MChildToParents
foldFunc m [[name], parents, [function]] = case M.lookup name m of 
                                                Just _ -> error $ "node aleady exist: " ++ name
                                                Nothing -> M.insert name (parents, function) m
foldFun _ e = error $ "error input: " ++ show e


ctop :: MChildToParents -> MParentToChild 
ctop m = foldl' cfun M.empty l
    where l = M.toList m
          l1 = map (\v -> (fst v, fst $ snd v)) l
          cfun m0 (name, (_, function)) = 
            case filter (\(_, ps) -> name `elem` ps) l1 of 
                [] -> M.insert name ([], function) m0
                xs -> M.insert name (map fst xs,function) m0

getSourceNode :: MChildToParents -> String
getSourceNode m = case l of 
                    [] -> error $ "no source node " ++ show m
                    [x] -> x
                    _ -> error "not support muitle source"
    where l = map fst $ filter (null . fst . snd) $ M.toList m

createE :: String -> MParentToChild -> MChildToParents -> IORef (Map String ExpQ) -> ExpQ
createE name m mr state = 
    case M.lookup name m of 
        Nothing -> error "can't find node"
        Just ([], function) -> appE (varE $ mkName "dealSink") (varE $ mkName function)
        Just ([x], function) ->
            case M.lookup x mr of 
                Nothing -> error "strange happened"
                Just ([], _) -> error "strange happened"
                Just ([_], _) -> let v = appE (varE $ mkName "addFun") (varE $ mkName function) 
                                 in appE v (createE x m mr state)
                Just ([_,_], _) ->   
                    let refstate = unsafePerformIO $ readIORef state
                    in case M.lookup name refstate of 
                        Nothing -> let v1 = appE (varE $ mkName "addJoin") (varE $ mkName "creatIORef")
                                       v2 = appE v1 (varE $ mkName function)
                                       v3 = appE v2 (createE x m mr state)
                                   in unsafePerformIO (writeIORef state (M.insert name v3 refstate)) `seq` v3
                        Just resv -> let v = appE (varE $ mkName "addFun") (varE $ mkName function) 
                                     in appE v resv
        Just (xs, function) -> let v = appE (varE $ mkName "addFuns") (varE $ mkName function)
                                in appE v (listE $ map (\v -> createE v m mr state) xs)






--
--getSinkNode :: MChildToParents -> [String]
--getSinkNode m = filter (`notElem` l)  la
--    where l = nub $ concatMap (fst . snd) $ M.toList m -- all parents
--          la = nub $ map fst $ M.toList m -- all Node


--parseInput :: String -> [String]  -- Map String ([String], String)
-- parseInput s = let [a,b] = words s
--                 in appE (varE $ mkName a) (varE $ mkName b) -- (a, b)
---------------------------------------------------------------------------------------
createExpr :: String -> MChildToParents -> ExpQ
createExpr sink m 
    = case M.lookup sink m of 
           Nothing -> error $ "parents node doesn't exist: " ++ sink
           Just ([], _) -> error "sing sink" -- varE $ mkName function
           Just ([x], function) -> let v = appE (varE $ mkName "dealSink") (varE $ mkName function) -- uInfixE (createExpr x m) (varE $ mkName ".") (varE $ mkName function)
                                   in createExpr' x m v

createExpr' :: String -> MChildToParents -> ExpQ -> ExpQ 
createExpr' name m e  
    = case  M.lookup name m of 
        Nothing -> error $ "parents node doesn't exist: " ++ name
        Just ([], function) -> let v = appE (varE $ mkName "addFun") (varE $ mkName function) in appE v e
        Just ([x], function) -> let v = appE (varE $ mkName "addFun") (varE $ mkName function) 
                                    v' = appE v e
                                in createExpr' x m v'
---------------------------------------------------------------------------------------
























