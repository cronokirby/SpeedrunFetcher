module Main where

import Control.Monad

import Api

main :: IO ()
main = do
    putStrLn "Enter a command: use --help for a list of commands"
    forever $ do
        command <- getLine
        handleCommand (words command)

handleCommand :: [String] -> IO ()
handleCommand (command:xs)
    | command == "--help" = putStrLn $
        "Use '--categories gamename' to get a list of categories for a game\n"
        ++ "Use '--search GameName' to find the abbreviation for a game"
    | command == "--search" = search $ unwords xs
handleCommand (command:arg:xs)
    | command == "--categories" = categories arg

type GameName = String
categories :: GameName -> IO ()
categories abbreviation = do
    let url = "http://www.speedrun.com/api/v1/games/"
            ++ abbreviation ++ "/categories"
    categories <- fetchCategories url
    case categories of
        Left err -> putStrLn err
        Right cats -> putStrLn $ "Here's a list of categories for "
                            ++ abbreviation ++ ":\n" ++ show cats

search :: GameName -> IO ()
search gameName = do
    let url = "http://www.speedrun.com/api/v1/games?name=" ++ gameName
    abbreviation <- fetchAbbreviation url
    case abbreviation of
        Left err -> putStrLn err
        Right abbr -> putStrLn $ "The abbreviation for '" ++ gameName
                              ++ "' is:\n" ++ abbr
