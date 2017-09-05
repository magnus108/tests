{-# LANGUAGE TemplateHaskell #-}
module TemplateHaskellSimple
where

import Language.Haskell.TH


foo :: Q Exp
foo  = [| \n -> show n |]


baz :: Q (TExp Int)
baz = [||  3 + 3  ||]
