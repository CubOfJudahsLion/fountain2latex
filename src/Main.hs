{-|
Module      : Main
Description : Processes command-line options and invokes business logic.
Copyright   : (c) Alexander Feterman Naranjo, 2023
License     : MIT
Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
Stability   : experimental
Portability : POSIX,Windows

fountain2latex is a simple utility to convert the .fountain format
to LaTeX.
-}

module Main where


import CommandLine
import GitVersion
import System.Environment ( getArgs, getProgName )
import System.IO ( FilePath )


--  Version and Hash, actually
showVersion :: IO ()
showVersion = do
  progName  <- getProgName
  putStrLn $ progName ++ " version " ++ gitVersion ++ "."


showHelp :: IO () 
showHelp = do
  progName <- getProgName
  putStrLn $ progName ++  " is a simple utility to convert from the \
                          \.fountain screenplay format to LaTeX.\n\n\
                          \Modes: \n\
                          \  {-v|--version}: Print program version\n\n\
                          \  {-u|--usage}  : Display basic usage\n\
                          \  {-h|--help}   : Display this help\n\
                          \  [-p|--as-part] [<infile>[.fountain] [.|<outfile>[.tex]]]:\n\
                          \                 Read from standard input (unless <infile>.fountain\n\
                          \                 is specified) and convert to standard output, or\n\
                          \                 <outfile>.tex if specified. If a period is given\n\
                          \                 instead of the name for outfile, the output file\n\
                          \                 will have the name <infile>.tex."


showUsage :: IO ()
showUsage = do
  progName <- getProgName
  putStrLn  $  "Usage: "
            ++ progName
            ++ " {-u|-h|-v|[-p] [<infile>[.fountain] [.|<outfile>[.tex]]]}"


main :: IO ()
main = do
  args <- getArgs
  let (solvedArgs, errorList) = solveArguments args
  case solvedArgs of


