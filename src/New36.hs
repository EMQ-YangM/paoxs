{-# LANGUAGE TemplateHaskell #-}

module New36 where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

dgraph :: QuasiQuoter
dgraph =
  QuasiQuoter
    { quoteExp = \x -> [|parseInput x|],
      quotePat = error "no used",
      quoteType = error "no used",
      quoteDec = error "no used"
    }

-- parseInput :: String -> ExpQ
parseInput s = s
