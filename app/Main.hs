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
handleCommand (command:arg:xs)
    | command == "--categories" = categories arg
    | command == "--search"     = search arg
handleCommand (command:game:xs)
    | command == "--wr" = getTime 1 game (unwords xs)
handleCommand (command:place:game:xs)
    | command == "--time" = getTime (read place) game (unwords xs)
handleCommand _ = putStrLn "unkown command"


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


getTime :: Int -> GameName -> String -> IO ()
getTime place abbreviation catName = do
    let catUrl = "http://www.speedrun.com/api/v1/games/"
            ++ abbreviation ++ "/categories"
    leaderboard <- fetchLeaderboard catName catUrl
    case leaderboard of
        Left err -> putStrLn err
        Right url -> do
            runString <- fetchTime (place-1) url
            case runString of
                Left err -> putStrLn err
                Right info -> putStrLn info
