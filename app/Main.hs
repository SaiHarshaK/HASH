import Control.Monad.Trans
import System.Process
import System.Directory
import System.Posix.User
import System.Console.ANSI
import System.Console.Haskeline
import System.IO
import System.Exit
import System.Posix.Signals
import System.Posix.Env
import Control.Concurrent
import Data.List.Split
import Data.Text (strip, pack, unpack)
import Helpers
import Lib
import GitConfigParser

type REPL a = InputT IO a

prompt :: REPL()
prompt = do
  promptText <- (liftIO $ getUserPrompt)
  setSGR [SetColor Foreground Vivid Yellow]
  setSGR [SetColor Background Dull Blue]
  installHandler keyboardSignal (Catch ctrlC) Nothing
  eof <- isEOF
  handleEOF eof
  inpLine <- getInputLine promptText
  setSGR [Reset]
  case inpLine of
    Nothing -> outputStrLn "Exit."
    -- Do not recursively call the REPL again, when exiting
    Just "exit" -> return()
    -- Call the REPL recursively for the next command
    Just command -> (liftIO $ handleCommand command) >> prompt

getUserPrompt = do
  userName <- getEffectiveUserName
  dirPrompt <- getDirPrompt
  isGit <- isGitRepository
  if isGit then
	do
		branch <- getBranch
		return (userName ++ "@hash (" ++ dirPrompt ++ ") (" ++ branch ++ ") $ ")
	else
		return (userName ++ "@hash (" ++ dirPrompt ++ ") $ ")
		
-- Handle the command entered in prompt
handleCommand :: String -> IO()
handleCommand command = do
  addCommandToHistory (command ++ "\n")
  -- execute the line of command
  executeLine command

handleEOF b = if b then
              do 
                  handleCommand "exit" 
              else
                putStr("")

ctrlC :: IO ()
ctrlC = do
    putStrLn ""
    prompt

executeLine :: String -> IO ()
-- empty command, just print empty string
executeLine [] = putStr ""
executeLine command =
	-- check if command is from built-ins
    if elem commandName builtins
    	then runBuiltin ( parseCommand command)
    	else do
    		-- if not built-in command, run as sys command
    		system command
    		return ()
    where
    	-- parse the command-name out of the string
    (commandName, args) = parseCommand command

main :: IO ()
main = do
  -- Check if hist file exists, only on startup
  -- This file is then re-used, rather than checking
  -- for the file everytime a command is to be appended
  histFile <- getHistFile
  makeSureFileExists histFile
  -- Run the shell
  runInputT defaultSettings prompt
