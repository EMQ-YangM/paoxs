{-# LANGUAGE TemplateHaskell #-}

module New22 where


import Language.Haskell.TH





emptyShow :: Name -> Q [Dec]
emptyShow name = [d|instance Show $(conT name) where show _ = ""|] 








