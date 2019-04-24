import System.Process
import System.Directory
import System.Posix.User
import System.Console.ANSI
import System.IO
import Helpers
import Lib

-- The command prompter for the shell
prompt :: IO ()
prompt = do
	-- get the username of the user
        userName <- getEffectiveUserName
       	-- print the prompt
        dirPrompt <- getDirPrompt
        setSGR [SetColor Foreground Vivid Yellow]
        setSGR [SetColor Background Dull Blue]
        putStr (userName ++ "@hash>" ++ dirPrompt)
        -- flush stdout
        setSGR [Reset]  -- Reset to default colour scheme
        putStr(" ")
        hFlush stdout
        makeSureFileExists histFileName
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
		                else do 
		                	-- execute the line of command
		                    executeLine command
		                    prompt

main :: IO ()
main =  prompt

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