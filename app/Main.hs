module Main where

import qualified Shallow
import qualified Deep
import qualified Deeper
import qualified Gadts
import qualified Rebindable
import qualified EvenDeeper
import qualified Sharing
import qualified SharingSolved
import qualified TemplateHaskellSimple2
import qualified TemplateHaskellTuples2
import qualified LaCarte
import qualified LaCarte2

import qualified Contracts


main :: IO ()
main = Contracts.main
