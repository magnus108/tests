{-# LANGUAGE TemplateHaskell #-}
module TemplateHaskellTuples
where

import Language.Haskell.TH


sel :: Int -> Int -> ExpQ
sel i n = [| \tup -> case tup of {$pat -> $res} |]
  where
    pat = tupP (map varP names)
    res = varE (names !! (i-1))
    names = [mkName $ "v" ++ show i | i <- [1..n]]
