import System.Process
import System.Directory
import System.Posix.User
import System.IO
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
        command <- getLine
        -- Handle the incoming command
        handleCommand command

-- Handle the command entered in prompt
handleCommand :: String -> IO()
-- if exit is typed, we return
handleCommand command = if command == "exit" then do
		                	putStrLn "Bye :)"
		                	return ()
		                else do 
		                	-- execute the line of command
		                    executeLine command
		                    prompt

main :: IO ()
main =  prompt

executeLine :: String -> IO ()
executeLine [] = putStr ""
executeLine a = do 
                system a
                return ()


