module Lib
    (
        getDirPrompt,
        runBuiltin,
        builtins,
        makeSureFileExists,
        getHistFile,
        addCommandToHistory,
        find,
        export,
        help,
        canSetVar
    ) where

import System.Process
import System.Directory
import System.Posix.User
import System.IO
import Control.Exception
import System.Posix.Env
import Data.List (isPrefixOf, isInfixOf, nub)
import Data.List.Split
import Data.Strings
import GitConfigParser
import Data.Text (strip, pack, unpack)

getDirPrompt :: IO String
getDirPrompt =  do
                  -- current working directory
                  workingDir <- getCurrentDirectory
                  -- the home diectory
                  homeDir <- getHomeDirectory
                  -- construct directory relative string
                  relDir <- constructString homeDir workingDir
                  return relDir
                  where
                    constructString :: String -> String -> IO FilePath
                    -- default
                    constructString [] [] = return "~/"
                    constructString [] a = return ("~" ++ a)
                    constructString _ [] = getCurrentDirectory
                    constructString (x:xs) (y:ys)
                    -- compare to check if the current directory is home
                      | x == y = constructString xs ys
                      | x /= y = getCurrentDirectory

runBuiltin :: (String, String) -> IO ()
-- handle cd builtin
runBuiltin ("cd", argString) = do
  dirExists <- check doesDirectoryExist argString
  if dirExists || argString == "" then do
   changeWorkingDirectory argString
  else
    putStr ""
-- handle history builtin
runBuiltin ("history", argString) = historyBuiltIn argString
-- handle unset builtin
runBuiltin ("unset", argString) = unsetVar (words . unpack . strip . pack $ argString)
-- handle find builtin
runBuiltin ("find", argString) = find argString
-- handle export builtin
runBuiltin ("export", argString) = export argString
-- handle help builtin
runBuiltin ("help", argString) = help argString
-- function to change the workind directory

changeWorkingDirectory :: String -> IO ()
changeWorkingDirectory "" = do
                          homeDir <- getHomeDirectory
                          setCurrentDirectory homeDir
changeWorkingDirectory dir  = setCurrentDirectory dir

builtins :: [String]
builtins = ["cd", "history", "unset", "help", "find", "export"]

-- Get the path to the user-specific history file
getHistFile = do
  user <- getEffectiveUserName
  let histFilePath = "/home/" ++ user ++ "/.hash_history"
  return histFilePath

-- function to make sure that a file exists
makeSureFileExists :: String -> IO()
makeSureFileExists fileName = do
   fileExist <- doesFileExist fileName
   if not fileExist
   then writeFile fileName ""
   else return ()

-- handle the history builtin
historyBuiltIn :: String -> IO()
historyBuiltIn opts = do
    histFileName <- getHistFile
    makeSureFileExists histFileName
    handle <- openFile histFileName ReadMode
    contents <- hGetContents handle
    let contentsList = splitOn "\n" contents
    -- putStr (giveOneLine contents)
    putStr contents
    hClose handle
    -- contentsList

-- validate and add command to history
addCommandToHistory :: String -> IO()
addCommandToHistory "" = do
  histFileName <- getHistFile
  appendFile histFileName ""
addCommandToHistory (' ':command) = addCommandToHistory command
addCommandToHistory ('\n':command) = addCommandToHistory command
addCommandToHistory command = do
  histFileName <- getHistFile
  appendFile histFileName command

-- unset the environment variable
unsetVar :: [String] -> IO()
unsetVar ("":[]) = return ()
unsetVar (command:_) = unsetEnv command

printPath :: [String] -> IO ()
printPath (sugg:[]) = putStr $ id sugg
printPath (sugg:suggList) = do
                putStrLn $ id sugg
                printPath suggList

find :: String -> IO ()
find argStr = do
  let argList = filter (not . null) (strSplitAll " " argStr)
      dir = head argList
      arg = tail argList
      exp = last argList
  absPath <- makeAbsolute dir
  -- if '/' is the prefix
  if "/" `isPrefixOf` dir then do
    possiblePath <- findRootPaths absPath
    let path = dir
        pathList = strSplitAll "\n" possiblePath
    if length arg == 1 then do
      let reqd = filter (isInfixOf exp) (pathList)
      printPath reqd
    else if length arg > 1 then do
      let reqd = [ allReqd | x <- arg, allReqd <- filter (isInfixOf x) (pathList)]
      printPath . nub $ reqd
    else
      printPath pathList
  else if "~" `isPrefixOf` dir then do
    user <- getEffectiveUserName
    let absPath = strReplace "~" ("/home/" ++ user) dir
    paths <- findPaths absPath
    let pathList = strSplitAll "\n" paths
    if length arg == 1 then do
      let reqd = filter (isInfixOf exp) (pathList)
      printPath reqd
    else if length arg > 1 then do
      let reqd = [ allReqd | x <- arg, allReqd <- filter (isInfixOf x) (pathList)]
      printPath . nub $ reqd
    else
      printPath pathList
    --relative
  else do
    absPath <- makeAbsolute dir
    paths <- findPaths absPath
    let pathList = strSplitAll "\n" paths
    if length arg == 1 then do
      let reqd = filter (isInfixOf exp) (pathList)
      printPath reqd
    else if length arg > 1 then do
      let reqd = [ allReqd | x <- arg, allReqd <- filter (isInfixOf x) (pathList)]
      printPath . nub $ reqd
    else
      printPath pathList

promptLine :: String -> IO String
promptLine prompt = do
    putStr prompt
    getLine

getPassword :: IO String
getPassword = do
  putStr "Enter Password: "
  hFlush stdout
  pass <- withEcho False getLine
  putChar '\n'
  return pass

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

findRootPaths :: String -> IO String
findRootPaths path = do
    pass <- getPassword
    readProcess "/usr/bin/sudo" ["-S","find",path] (pass ++ "\n")

findPaths :: String -> IO String
findPaths path = readProcess "find" [path] []

countCmds :: String -> Int
countCmds = length . words

canSetVar :: String -> Bool
canSetVar varVal = -- check if only one word
                if '=' `notElem` varVal then do
                    False
                else if countCmds varVal > 1 then do
                    False
                else do
                    True

export :: String -> IO ()
export argStr = do
  if canSetVar argStr then do
    let (var: _: values) = split (oneOf "=") (unpack . strip . pack $ argStr)
    let val = concat values
    setEnv var val True
  else
    putStrLn "Wrong number of arguments"

help :: String -> IO()
help "" = do putStr(helpString "all")

helpString :: String -> String
helpString "builtins" = concat
    [
      "Builtins :\n ",
      "help: Displays this text\n ",
      "cd: Change working Directory\n ",
      "history: Display previous commands entered\n",
      "export: set export attribute for variables <export keyword optional>\n ",
      "unset: unset export attribute for variables\n ",
      "find: search for files in a directory hierarchy. <no args lists all files>\n "
    ]

-- format for adding to help
-- helpString "<type>" = "<details>"
--  then add to help all using ++
helpString "all" = helpString "builtins"
