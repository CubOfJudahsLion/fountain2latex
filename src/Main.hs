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
{-# LANGUAGE ViewPatterns #-}
module Main where


import Data.Char ( toLower )
import Data.List ( foldl', isSuffixOf )
import GitVersion
import System.Directory ( doesFileExist )
import System.Environment ( getArgs, getProgName )
import System.Exit ( die )
import System.IO ( FilePath )


--  Version and Hash, actually
showVersion :: IO ()
showVersion = do
  progName  <- getProgName
  putStrLn $ progName ++ " version " ++ gitVersion ++ '.' : []


--showHelp :: IO ()
--showHelp = do
--  progName <- getProgName
--  putStrLn $ progName ++  " is a simple utility to convert from the \
--                          \.fountain screenplay format to LaTeX.\n\n\
--                          \Modes: \n\
--                          \  -u|--usage]  : Display basic usage\n\
--                          \  -h|--help]   : Display this help\n\
--                          \  -v|--version]: Print program version\n\n\
--                          \  <infile>[.fountain] [<outfile>[.tex]]:\n\
--                          \                 Read fountain file infile.fountain;\n\
--                          \                 if specified, write to outfile.tex,\n\
--                          \                 or to standard output otherwise."

showUsage :: IO ()
showUsage = do
  progName <- getProgName
  putStrLn  $  "Usage: "
            ++ progName
            ++ " {-u|-h|-v|[-f] <infile>[.fountain] [<outfile>[.tex]]}"


convert :: String -> Maybe String -> IO ()
convert inFile maybeOutFile = do
  let inputExtension    =   ".fountain"
  let outputExtension   =   ".tex"
  let inFileWithExt     =   ensureExtension inputExtension inFile
  let mayOutFileWithExt =   ensureExtension outputExtension <$> maybeOutFile
  inputExists           <-  doesFileExist inFileWithExt
  if inputExists
    then  putStrLn $ "File " ++ inFileWithExt ++ " found."
    else  die $ "Can't find file " ++ inFileWithExt ++ "."

 where
  ensureExtension :: String -> String -> String
  ensureExtension extension fileName =
    if extension `isSuffixOf` fmap toLower fileName
      then  fileName
      else  fileName ++ extension


data CmdLineSwitch = UsageSwitch
                   | VersionSwitch
                   | HelpSwitch
  deriving (Default)

data CmdLineArg = Switch CmdLineSwitch
                | FileArg FilePath

parseCmdLineArg :: String -> CmdLineArg
parseCmdLineArg "-u"        = Switch UsageSwitch
parseCmdLineArg "--usage"   = Switch UsageSwitch
parseCmdLineArg "-v"        = Switch VersionSwitch
parseCmdLineArg "--version" = Switch VersionSwitch
parseCmdLineArg "-h"        = Switch HelpSwitch
parseCmdLineArg "--help"    = Switch HelpSwitch
parseCmdLineArg fileName    = FileArg FilePath

data OperationMode  = Switch CmdLineSwitch
                    | FileToStdout FilePath
                    | FileToFile FilePath FilePath
                    | Error [String]


interpretArg :: String -> CmdLineArg
interpretArg arg
  | 


main :: IO ()
main = do
  args <- getArgs
  case args of
    (fmap (span (== '-')) -> [(_:_, mode)]) ->
      if modeMatches mode "help" then
        showHelp
      else if modeMatches mode "version" then
        showVersion
      else
        showUsage
    [inFile]                                ->
      convert inFile Nothing
    [inFile, outFile]                       ->
      convert inFile (Just outFile)
    _                                       ->
      showUsage
 where

