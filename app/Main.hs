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

builtins :: [String]
builtins = ["cd"]

-- The command prompter for the shell
prompt :: IO ()
prompt = do
  -- get the username of the user
        userName <- getEffectiveUserName
        -- print the prompt
        putStr (userName ++ "@hash> ")
        -- flush stdout
        hFlush stdout
        -- In case of interrupts, handle them instead of exiting shell
        installHandler keyboardSignal (Catch ctrlC) Nothing
        command <- getLine
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

executeLine :: String -> IO ()
executeLine [] = putStr ""
executeLine a = do
                system a
                return ()

ctrlC :: IO ()
ctrlC = do
    putStrLn ""
    prompt
