module Main where

import Control.Monad

import Commands.Handling ( handleCommand )

main :: IO ()
main = do
    putStrLn "Enter a command: use --help for a list of commands"
    forever $ do
        command <- getLine
        handleCommand (words command)
