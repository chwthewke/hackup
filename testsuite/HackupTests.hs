{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where


import                   Test.Framework

import {-@ HTF_TESTS @-} Hackup.SelectorsTest
import {-@ HTF_TESTS @-} Hackup.Config.ParserTest
import {-@ HTF_TESTS @-} Hackup.Driver.IODriverTest

main :: IO()
main = htfMain htf_importedTests
