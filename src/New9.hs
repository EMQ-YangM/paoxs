{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module New9 where

import Control.Concurrent
import Control.Concurrent.Chan
import Dynamic
import GHC.TypeLits

-- data Fun s d
--   = F (d -> d)
--   | FIO (d -> IO d)
--   | SF (s -> d -> (s, d))
--   | SFIO (s -> d -> IO (s, d))

data Message = Message
  { nodeNumber :: Int,
    -- messageNumber :: Int, 下游去重,指定message的编号
    messageBody :: Dynamic
  }

-- data Fun s d m n
--   = F (s -> Cache m d -> (s, Cache m d, Handle n d))

infixr 4 :|

data C m f d where
  Nil :: C 0 f d
  (:|) :: f d -> C m f d -> C (m + 1) f d


-- | babaaa 
-- bbbbbb
-- godless
-- sing god less
-- 1 + 1 = 2
wmChan :: d -> C n Chan d -> IO ()
wmChan _ Nil = return ()
wmChan d (x :| xs) = do
  writeChan x d
  wmChan d xs

data Stream n d = Stream (Chan d -> C n Chan d -> IO ())

-- join :: Stream 2 d -> Stream 1 d -> Stream 1 d -> Stream 2 d
-- join = undefined
  
-- cnn :: Stream m d -> C m (Stream n) d -> IO (Stream k d)
-- cnn (Stream ic) cs = return $
--   Stream $ \chan -> do
--     return undefined
