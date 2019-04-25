module GitConfigParser
    where

import System.IO
import System.Directory
import Data.Maybe
import Data.List.Split
import Helpers

getLines :: FilePath -> IO [String]
getLines path = do
                    contents <- readFile path
                    return (lines contents)

notEmpty :: (String, String) -> Bool
notEmpty ("","") = False
notEmpty (a,b)
  | a == "" && all (==' ') b = False
  | otherwise = True

getBranch :: IO String
getBranch = do
    line <- getConfigLine
    let lineOne = line !! 0
    let slashSeperated = splitOn "/" (snd lineOne)
    return (last slashSeperated)

check :: (FilePath -> IO Bool) -> FilePath -> IO Bool
check p s = do
  result <- p s
  return result

isGitRepository :: IO Bool
isGitRepository = do
    isGit <- check doesDirectoryExist ".git"
    return isGit

parseLines :: FilePath -> IO [(String, String)]
parseLines path = do
                        lines <- getLines path
                        return $ filter notEmpty $ map parseCommand lines

getConfigLine :: IO [(String, String)]
getConfigLine = parseLines ".git/HEAD"