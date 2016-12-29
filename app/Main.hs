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
    | command == "--help" = putStrLn
        "Use '--categories gamename' to get a list of categories for a game\n\
        \Use '--search GameName' to find the abbreviation for a game\n\
        \Use '--wr gamename' to get a wr for a game\n\
        \Use '--time place gamename' to get the nth place time\n\
        \These commands require the game's abbreviation, to use them \
        \with a full name, use ?command with the game's name, with \
        \dashes in place of spaces"
handleCommand (command:arg:xs)
    | command == "--categories" = categories arg
    | command == "?categories"  = abbreviated categories arg
    | command == "--search"     = search arg
handleCommand (command:game:xs)
    | command == "--wr" = getTime 1 (unwords xs) game
    | command == "?wr"  = abbreviated (getTime 1 (unwords xs)) game
handleCommand (command:place:game:xs)
    | command == "--time" = getTime (read place) (unwords xs) game
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
search dashedName = do
    let gameName = map (\c -> if c == '-' then ' ' else c) dashedName
    let url = "http://www.speedrun.com/api/v1/games?name=" ++ gameName
    abbreviation <- fetchAbbreviation url
    case abbreviation of
        Left err -> putStrLn err
        Right abbr -> putStrLn $ "The abbreviation for '" ++ gameName
                              ++ "' is:\n" ++ abbr


getTime :: Int -> String -> GameName -> IO ()
getTime place catName abbreviation  = do
    let catUrl = "http://www.speedrun.com/api/v1/games/"
            ++ abbreviation ++ "/categories"
    leaderboard <- fetchLeaderboard catName catUrl
    case leaderboard of
        Left err -> putStrLn err
        Right url -> do
            runString <- fetchTime (place-1) url
            case runString of
                Left err -> putStrLn err
                Right runString -> putStrLn runString

--Fetches the abbreviation for a game, before calling a function with that
abbreviated :: (GameName -> IO ()) -> String -> IO ()
abbreviated func dashedName = do
    let gameName = map (\c -> if c == '-' then ' ' else c) dashedName
    let url = "http://www.speedrun.com/api/v1/games?name=" ++ gameName
    abbreviation <- fetchAbbreviation url
    case abbreviation of
        Left err -> putStrLn err
        Right abbreviation -> func abbreviation
