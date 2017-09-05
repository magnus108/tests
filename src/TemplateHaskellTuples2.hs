{-# LANGUAGE TemplateHaskell #-}
module TemplateHaskellTuples2
where


import TemplateHaskellTuples


main :: IO ()
main = print $ $(sel 5 5) ("a", "b", "c", "d", "e")
