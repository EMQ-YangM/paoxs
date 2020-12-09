module Main where

import Paxos ( start )
import System.Environment ( getArgs )
-- import New19
import New20
import Scheam1

--main :: IO ()
--main = do 
--    a:b:_ <- getArgs
--    let va = read a 
--        vb = read b 
--    start va vb
--
main :: IO ()
main = tfun1
