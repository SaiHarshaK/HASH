import System.Process
import System.Directory
import System.Posix.User
import System.IO
import System.Exit
import System.Posix.Signals
import System.Posix.Env
import Control.Concurrent
import Data.List.Split
import Data.Text (strip, pack, unpack)
import Helpers
import Lib

-- The command prompter for the shell
prompt :: IO ()
prompt = do
  -- get the username of the user
        userName <- getEffectiveUserName
        -- print the prompt
        dirPrompt <- getDirPrompt
        putStr (userName ++ "@hash>" ++ dirPrompt ++" ")
        -- flush stdout
        hFlush stdout
        makeSureFileExists histFileName
        -- In case of interrupts, handle them instead of exiting shell
        installHandler keyboardSignal (Catch ctrlC) Nothing
        command <- getLine
        addCommandToHistory (command ++ "\n")
        -- Handle the incoming command
        handleCommand command

-- Handle the command entered in prompt
handleCommand :: String -> IO()
-- if exit is typed, we return
handleCommand command = if command == "exit" then do
                            putStrLn "Bye :)"
                            return ()
                        else if setVar command then do
                            let (var: _: values) = split (oneOf "=") (unpack . strip . pack $ command)
                            let val = concat values
                            setEnv var val True
                            prompt
                        else do
                      -- execute the line of command
                            executeLine command
                            prompt

main :: IO ()
main =  prompt

countCmds :: String -> Int
countCmds = length . words

setVar :: String -> Bool
setVar varVal = -- check if only one word
                if '=' `notElem` varVal then do
                    False
                else if countCmds varVal > 1 then do
                    False
                else do
                    True

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
