module Lib
    (
        getDirPrompt,
        runBuiltin,
        builtins,
        makeSureFileExists,
        getHistFile,
        addCommandToHistory
    ) where

import System.Process
import System.Directory
import System.Posix.User
import System.Posix.Types
import System.IO
import System.Posix.Env
import Data.List.Split
import System.Posix.Files
import System.Posix.Files.ByteString(intersectFileModes)
import GitConfigParser
import Data.Text (strip, pack, unpack)
import Data.List

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
runBuiltin ("help", argString) = help argString
runBuiltin ("ls", argString) = ls argString
-- function to change the workind directory
changeWorkingDirectory :: String -> IO ()
changeWorkingDirectory "" = do
                          homeDir <- getHomeDirectory
                          setCurrentDirectory homeDir
changeWorkingDirectory dir  = setCurrentDirectory dir

builtins :: [String]
builtins = ["cd", "history", "unset", "help","ls"]

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

help :: String -> IO()
help "" = do putStr(helpString "all")

helpString :: String -> String
helpString "builtins" = "Builtins :\n help: Displays this text\n cd: Change working Directory,\n history: Display previous commands entered\n"
-- format for adding to help
-- helpString "<type>" = "<details>"
--  then add to help all using ++
helpString "all" = helpString "builtins"

ls :: String -> IO ()

ls arg = do
  currDir <- getCurrentDirectory
  contents <- getDirectoryContents currDir
  let filtered = filter (not . isPrefixOf ".") contents
  if arg == []
    then do
    printContent filtered
    else do
    printContents filtered

printContent :: [String] -> IO()

printContent [] = putStrLn ""

printContent (x:xs) = do
  dirExist <- doesDirectoryExist x
  if dirExist
    then do
    putStr $ "\x1b[32m" ++ x ++ "\t"
    printContent xs
    else do
    putStr $ "\x1b[0m" ++ x ++ "\t"
    printContent xs


printContents :: [String] -> IO()

printContents [] = putStr ""

printContents (x:xs) = do
	status <- getFileStatus x
	uname <- getUserEntryForID (fileOwner status)
	gname <- getGroupEntryForID (fileGroup status)
	if isDirectory status
		then do
		putStr "d"
		else do
		putStr "-"
	if intersectFileModes (fileMode status) ownerReadMode == ownerReadMode
		then do
		putStr "r"
		else do
		putStr "-"
	if intersectFileModes (fileMode status) ownerWriteMode == ownerWriteMode
		then do
		putStr "w"
		else do
		putStr "-"
	if intersectFileModes (fileMode status) ownerExecuteMode == ownerExecuteMode
		then do
		putStr "x"
		else do
		putStr "-"
	if intersectFileModes (fileMode status) groupReadMode == groupReadMode
		then do
		putStr "r"
		else do
		putStr "-"
	if intersectFileModes (fileMode status) groupWriteMode == groupWriteMode
		then do
		putStr "w"
		else do
		putStr "-"
	if intersectFileModes (fileMode status) groupExecuteMode == groupExecuteMode
		then do
		putStr "x"
		else do
		putStr "-"
	if intersectFileModes (fileMode status) otherReadMode == otherReadMode
		then do
		putStr "r"
		else do
		putStr "-"
	if intersectFileModes (fileMode status) otherWriteMode == otherWriteMode
		then do
		putStr "w"
		else do
		putStr "-"
	if intersectFileModes (fileMode status) otherExecuteMode == otherExecuteMode
		then do
		putStr "x"
		else do
		putStr "-"
	putStr "\t"
	putStr ((userName uname) ++ "\t")
	putStr ((groupName gname) ++ "\t")
	putStr ((show (fileSize status)) ++ "\t")
	putStr x
	putStrLn ""
	printContents xs

pwd = do
  currDir <- getCurrentDirectory
  putStrLn $ "\x1b[32m" ++ currDir
