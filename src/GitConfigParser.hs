module GitConfigParser
    where

import System.IO
import System.Directory
import Data.Maybe
import Data.List.Split
import Helpers

-- | Returns the contents of the file from the given path, as a List of String
getLines :: FilePath -> IO [String]
getLines path = do
                    contents <- readFile path
                    return (lines contents)

-- | Returns True if both strings are nonEmpty (spaces are treated as empty) else False
--
-- >>> nonEmpty "Ha" "Shell"
-- True
-- >>> nonEmpty "" " "
-- False
notEmpty :: (String, String) -> Bool
notEmpty ("","") = False
notEmpty (a,b)
  | a == "" && all (==' ') b = False
  | otherwise = True

-- | finds the branch of the git repository, the user currently is on and returns it
getBranch :: IO String
getBranch = do
    line <- getConfigLine
    let lineOne = line !! 0
    let slashSeperated = splitOn "/" (snd lineOne)
    return (last slashSeperated)

-- | Helper function made for checking if a directory exists
check :: (FilePath -> IO Bool) -> FilePath -> IO Bool
check p s = do
  result <- p s
  return result

-- | Checks if ".git" exists in the $PWD and returns a boolean
-- | True if present, otherwise false
isGitRepository :: IO Bool
isGitRepository = do
    isGit <- check doesDirectoryExist ".git"
    return isGit

-- | Parse through the content of a file using its path, line by line and return them as a List of tuples
parseLines :: FilePath -> IO [(String, String)]
parseLines path = do
                        lines <- getLines path
                        return $ filter notEmpty $ map parseCommand lines

-- | Get ref to the HEAD branch as a List of tuple
-- | Second element of the tuple will contain the branch name
getConfigLine :: IO [(String, String)]
getConfigLine = parseLines ".git/HEAD"
