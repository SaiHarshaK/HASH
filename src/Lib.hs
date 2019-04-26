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

-- | Construct and return Present Working Directory
--
-- >>> getDirPrompt
-- ~/haskell-9
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

-- | Takes the builtIn command and its argument to execute it.
--
-- >>> find src/ Help
-- /home/harsha/haskell-9/src/Helpers.hs
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

-- | Takes a String and changes the PWD to the given path
--
-- >>> harsha@hash (~/haskell-9) $ cd src
-- >>> harsha@hash (~/haskell-9/src) $
changeWorkingDirectory :: String -> IO ()
changeWorkingDirectory "" = do
                          homeDir <- getHomeDirectory
                          setCurrentDirectory homeDir
changeWorkingDirectory dir  = setCurrentDirectory dir

-- | BuiltIn Commands
builtins :: [String]
builtins = ["cd", "history", "unset", "help", "find", "export"]

-- | Returns a string with path to .hash_history file
getHistFile :: IO String
getHistFile = do
  user <- getEffectiveUserName
  let histFilePath = "/home/" ++ user ++ "/.hash_history"
  return histFilePath

-- | Function to make sure that a file exists
-- | Create the file if it does not exit with the given fileName
makeSureFileExists :: String -> IO()
makeSureFileExists fileName = do
   fileExist <- doesFileExist fileName
   if not fileExist
   then writeFile fileName ""
   else return ()

-- TODO: Parse Flags and handle the history file accordingly.
-- | Handles the history builtin based on the flag passed
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

-- | Validate and add command to history
addCommandToHistory :: String   -- ^ command
                    -> IO()
addCommandToHistory "" = do
  histFileName <- getHistFile
  appendFile histFileName ""
addCommandToHistory (' ':command) = addCommandToHistory command
addCommandToHistory ('\n':command) = addCommandToHistory command
addCommandToHistory command = do
  histFileName <- getHistFile
  appendFile histFileName command

-- | Returns number of words in string after beraking up the whitespaces
--
-- >>> countCmds "hi there"
-- >>> 2
countCmds :: String -> Int
countCmds = length . words

-- | Checks if the argument passed to export is of valid syntax to assign value to variable
-- | Returns True if it is of valid syntax otherwise false
canSetVar :: String -> Bool
canSetVar varVal = -- check if only one word
                if '=' `notElem` varVal then do
                    False
                else if countCmds varVal > 1 then do
                    False
                else do
                    True

-- | Sets the environment variable
--
-- >>> export WORK=/home/work
-- >>> echo $WORK
-- >>> /home/work
export :: String -- ^ is of the form "variable=value", where the first '=' is given preference.
          -> IO ()
export argStr = do
  if canSetVar argStr then do
    let (var: _: values) = split (oneOf "=") (unpack . strip . pack $ argStr)
    let val = concat values
    setEnv var val True
  else
    putStrLn "Wrong number of arguments"

-- | Unset the environment variable
--
-- >>> unset $PWD
-- >>> echo $PWD
-- >>>
unsetVar :: [String] -- ^ Variable name
            -> IO()
unsetVar ("":[]) = return ()
unsetVar (command:_) = unsetEnv command

-- | Prints a List of Strings without Quotes
--
-- >>> printPath ["hello","how-are-you?"]
-- >>> hello
-- >>> how-are-you?
printPath :: [String] -- ^ List of Strings
             -> IO ()
printPath (sugg:[]) = putStrLn $ id sugg
printPath (sugg:suggList) = do
                putStrLn $ id sugg
                printPath suggList

-- | Prompt user for input and returns it as a String
promptLine :: String -- ^ Prompt to user
              -> IO String
promptLine prompt = do
    putStr prompt
    getLine

-- | Asks user for Password and masks the password (also hides number of characters)
-- | Then returns the password as a String
getPassword :: IO String
getPassword = do
  putStr "Enter Password: "
  hFlush stdout
  pass <- withEcho False getLine
  putChar '\n'
  return pass

-- | Lets User Mask any input given to stdin
withEcho :: Bool -- ^ If False then does not send the characters entered by user by stdin, otherwise output on stdout
            -> IO a
            -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

-- | Takes a path and returns all the files in the directory hierarchy with sudo privileges
findRootPaths :: String -- ^ Path
                 -> IO String
findRootPaths path = do
    pass <- getPassword
    readProcess "/usr/bin/sudo" ["-S","find",path] (pass ++ "\n")

-- | Takes a path and returns all the files in the directory hierarchy
findPaths :: String -- ^ Path
                 -> IO String
findPaths path = readProcess "find" [path] []

-- | Parses the given String for root directory path and expressions.
-- | Prints a List of Paths from root directory matching the given expressions
-- | Prints all files inside the root directory with no expressions are given
--
-- >>> find src/
-- /home/harsha/haskell-9/src/
-- /home/harsha/haskell-9/src/Helpers.hs
-- /home/harsha/haskell-9/src/GitConfigParser.hs
-- /home/harsha/haskell-9/src/Lib.hs
-- /home/harsha/haskell-9/src/hist.txt
find :: String -- ^ String to be Parsed
        -> IO ()
find "" = putStrLn "Incorrect arguments"
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

-- | Provide information about the usage of shell
help :: String -> IO()
help "" = do putStr(helpString "all")

-- | List Information which is concatenated and outputted on stdout
helpString :: String -> String
-- format for adding to help
-- helpString "<type>" = "<details>"
-- then add to help all using ++
helpString "all" = helpString "builtins"
helpString "builtins" = concat
    [
      "Builtins :\n ",
      "help: Displays this text\n ",
      "cd: Change working Directory\n ",
      "history: Display previous commands entered\n ",
      "export: set export attribute for variables <export keyword optional>\n ",
      "unset: unset export attribute for variables\n ",
      "find: search for files in a directory hierarchy (Lists all files if no arguments)\n "
    ]
