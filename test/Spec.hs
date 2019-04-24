module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Helpers

main :: IO ()
main = do
  defaultMain (testGroup "Our Library Tests" [parseCommandTest])

parseCommandTest :: TestTree
parseCommandTest = testGroup "Testing parseCommand"
	[
		testCase "Testing parseCommand Single Argument"
  		(assertEqual "Should split into command and arguments" ("nano", "helloworld.txt") (parseCommand "nano helloworld.txt")),
		testCase "Testing parseCommand Multiple Arguments"
  		(assertEqual "Should split into command and arguments" ("cp", "Documents/Book.pdf Downloads/Books/") (parseCommand "cp Documents/Book.pdf Downloads/Books/")),
		testCase "Testing parseCommand with Flags"
  		(assertEqual "Should split into command and arguments" ("ls", "-la") (parseCommand "ls -la"))
	]
