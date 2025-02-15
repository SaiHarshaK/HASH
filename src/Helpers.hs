module Helpers
  where

import System.IO

-- | Takes in a raw String and returns two strings split by the first space.
--
-- >>> parseCommand "find src/ Lib"
-- >>> ("find","src/ Lib")
parseCommand :: String -> (String, String)
-- Initially command is an empty string
parseCommand = cleanAndSplit ""
  where
    cleanAndSplit :: String -> String -> (String, String)
    -- remove blank space
    cleanAndSplit "" (' ':ys) = cleanAndSplit ""  ys
    -- If empty args, just pass commnad with empty string
    -- as arguments
    cleanAndSplit accum ""       = (reverse accum, "")
    -- reverse the word removed from the user string
    cleanAndSplit accum (' ':ys) = (reverse accum, ys)
    -- get first word out of the string
    cleanAndSplit accum (y:ys)   =  cleanAndSplit (y:accum) ys
