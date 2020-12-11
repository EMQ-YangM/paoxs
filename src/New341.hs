module New341 where

import Data.IORef
import New34
import System.IO

data StateManage

createIORefBackend :: a -> IO (State a)
createIORefBackend initState = do
  ref <- newIORef initState
  return $ ioRefBackendState ref

createFileBackend :: (Show a, Read a) => String -> a -> IO (State a)
createFileBackend name initState = do
  handle <- openFile name ReadWriteMode
  hPutStr handle $ show initState
  return $ fileBackendState handle

-- StateManage -> State a

--- create a state
-- init a state
-- release state
