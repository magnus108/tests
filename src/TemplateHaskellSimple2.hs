{-# LANGUAGE TemplateHaskell #-}
module TemplateHaskellSimple2
where

import TemplateHaskellSimple


bar :: Int -> String
bar = $(foo)


quux :: Int
quux = $$baz


main :: IO ()
main = print (bar (quux + 1))
