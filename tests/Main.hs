module Main where

import Test.Tasty

import TestsProject

main = defaultMain testSuite

--TODO: figure out how to set the timeout

testSuite =
  testGroup
    "testsProject"
    [
        TestsProject.test1,
        TestsProject.test2,
        TestsProject.test3,
        TestsProject.test4,
        TestsProject.test5,
        TestsProject.test6,
        TestsProject.test7,
        TestsProject.test8,
        TestsProject.test9,
        TestsProject.test10,
        TestsProject.test11,
        TestsProject.test12,
    ]
