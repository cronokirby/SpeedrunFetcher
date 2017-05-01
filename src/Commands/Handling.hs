module Commands.Handling ( handleCommand ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Speedrun.Api ( fetchCategories, fetchAbbreviation
                    , fetchLeaderboard, fetchTime )
import Speedrun.Parsing ( ParseResult (..) )


printErr :: (a -> IO ()) -> ParseResult a -> IO ()
printErr = eitherT putStrLn


helpMsg :: String
helpMsg =
  "Use '--categories gamename' to get a list of categories for a game\n\
  \Use '--search GameName' to find the abbreviation for a game\n\
  \Use '--wr gamename' to get a wr for a game\n\
  \Use '--time place gamename' to get the nth place time\n\
  \These commands require the game's abbreviation, to use them \
  \with a full name, use ?command with the game's name, with \
  \dashes in place of spaces"


handleCommand :: [String] -> IO ()
handleCommand (command:xs)
    | command == "--help" = putStrLn helpMsg
handleCommand (command:arg:xs)
    | command == "--categories" = categories arg
    | command == "?categories"  = abbreviated categories arg
    | command == "--search"     = search arg
handleCommand (command:game:xs)
    | command == "--wr"         = getTime 1 (unwords xs) game
    | command == "?wr"          = abbreviated (getTime 1 (unwords xs)) game
handleCommand (command:place:game:xs)
    | command == "--time"       = getTime (read place) (unwords xs) game
handleCommand _ = putStrLn "unkown command"


type GameName = String

categories :: GameName -> IO ()
categories abbreviation = printErr display $ fetchCategories url
  where
    url = "http://www.speedrun.com/api/v1/games/"
       ++ abbreviation
       ++ "/categories"
    display = putStrLn .
      (("Here's a list of categories for " ++ abbreviation ++ ":\n") ++) . show


abbreviate :: GameName -> ParseResult String
abbreviate = fetchAbbreviation . gameUrl
  where  -- Replace all dashes with spaces, prepend the root
    gameUrl = ("http://www.speedrun.com/api/v1/games?name=" ++) .
              map (\c -> if c == '-' then ' ' else c)


search :: GameName -> IO ()
search dashedName = printErr display $ abbreviate dashedName
  where
    display abbr = putStrLn $
      "The abbreviation for '" ++ dashedName ++ "' is:\n" ++ abbr


getTime :: Int -> String -> GameName -> IO ()
getTime place catName abbreviation = printErr putStrLn $
  fetchLeaderboard catName catUrl >>= fetchTime (place - 1)
  where
    catUrl = "http://www.speedrun.com/api/v1/games/"
          ++ abbreviation ++ "/categories"


--Fetches the abbreviation for a game, before calling a function with that
abbreviated :: (GameName -> IO ()) -> String -> IO ()
abbreviated func = printErr func . abbreviate
