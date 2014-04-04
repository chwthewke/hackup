{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where


import Test.Framework

import {-@ HTF_TESTS @-} Hackup.Config.ParserTest

main :: IO()
main = htfMain htf_importedTests
