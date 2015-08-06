{-# OPTIONS_GHC -F -pgmF htfpp #-}
module GameTest where

import           Test.Framework

prop_reverse :: [Int] -> Bool
prop_reverse xs = xs == (reverse (reverse xs))
