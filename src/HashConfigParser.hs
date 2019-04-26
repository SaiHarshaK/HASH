module HashConfigParser
	where

import System.IO
import Control.Monad
import Data.List.Split
import Data.Text (unpack, pack, strip)
import Lib (getConfFile)

--aliasKeywords :: [String]
--aliasDefs :: [String]

configFile = getConfFile

--aliasKeywords = getAliasKeywords configFile
--aliasDefs = getAliasDefinitions configFile

getAliasKeywords confFile = do
	conf <- readFile(confFile)
	let confLines = lines conf
	let keywords = [keyword | confLine <- confLines, keyword <- getKeyword confLine]
	let cleanKeywords = filter (not . null) keywords
	return cleanKeywords

getAliasDefinitions confFile = do
	conf <- readFile(confFile)
	let confLines = lines conf
	let keywords = [keyword | confLine <- confLines, keyword <- getKeyword confLine]
	let definitions = [definition | confLine <- confLines, definition <- getDefinition confLine]
	let cleanDefs = filter (not . null) definitions
	return cleanDefs

getKeyword confLine = do
	let command = head (splitOn " " confLine)
	let commandArgs = (concat(tail (splitOn " " confLine)))
	if command == "alias" then do
		let keyword = head (splitOn "=" commandArgs)
		return keyword
	else
		return ""

getDefinition confLine = do
	let command = head (splitOn " " confLine)
	let commandArgs = unwords (tail (splitOn " " confLine))
	if command == "alias" then do
		let definition = unwords (tail (splitOn "=" commandArgs))
		return (unpack . strip . pack $ definition)
	else
		return ""
