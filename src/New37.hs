{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}


module New37 where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import New36
import New34
import Data.IORef

-- t = 
--  [dgraph|
--   Normal,   name, fun, parentO
--   Choise,   name, fun, parentO
--   Filter,   name, fun, parentO
--   Sink,     name, fun, parentO

--   StateFun, name, fun, initState, backend, parentO
--   Process,  name, fun, initState, backend, time, parentM
--   Mutil,    name, parentM
--   Source,   name, fun
--  |]

-- t1 = 
--   [dgraph|
--   Source,   source, sourceFun
--   StateFun, sum,    sumFun, 0, fileBackend, source 
--   Sink,     sink,   sinkFun, sum
--   |]


t2 :: IO ()
t2 = do 
    myref <- newIORef 100
    t1 <- [dgraph|myref|]
    -- v <- readIORef t1 
    print t1
    return ()








