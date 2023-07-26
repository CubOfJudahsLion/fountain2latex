{-|
Module      : CommandLine
Description : Processes command-line arguments.
Copyright   : (c) Alexander Feterman Naranjo, 2023
License     : MIT
Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
Stability   : experimental
Portability : POSIX,Windows
-}

{-# LANGUAGE ViewPatterns #-}

module CommandLine ( CommandLineSwitch(..) , FinalArguments(..) , solveArguments ) where


import Control.Monad.Writer ( Writer, runWriter, tell )
import Data.Char ( toLower )
import Data.List ( foldl' )
import System.IO ( FilePath, Handle, stdin, stdout )


data CommandLineSwitch = VersionSwitch
                       | UsageSwitch
                       | HelpSwitch
                       | AsPartSwitch
                       | UnknownSwitch String

instance Show CommandLineSwitch where
    show :: CommandLineSwitch -> String
    show VersionSwitch      = "--version"
    show UsageSwitch        = "--usage"
    show HelpSwitch         = "--help"
    show AsPartSwitch       = "--as-part"
    show (UnknownSwitch u)  = u


data CommandLineArgument = SwitchArgument CommandLineSwitch
                         | FileArgument FilePath
                         | ArgumentError [String]
  deriving(Show)

parseArgument :: String -> CommandLineArgument
parseArgument "-v"        = SwitchArgument VersionSwitch
parseArgument "--version" = SwitchArgument VersionSwitch
parseArgument "-u"        = SwitchArgument UsageSwitch
parseArgument "--usage"   = SwitchArgument UsageSwitch
parseArgument "-h"        = SwitchArgument HelpSwitch
parseArgument "--help"    = SwitchArgument HelpSwitch
parseArgument "-p"        = SwitchArgument AsPartSwitch
parseArgument "--as-part" = SwitchArgument AsPartSwitch
parseArgument u@('-':_)   = SwitchArgument (UnknownSwitch u)
parseArgument arg         = FileArgument arg


data FinalArguments = FinalSwitch CommandLineSwitch
                    | FinalFileSet { asPart :: Bool
                                   , input  :: Maybe FilePath
                                   , output :: Maybe FilePath }
  deriving(Show)

solveArguments :: [String] -> Writer [String] FinalArguments
solveArguments =
  let initial = pure (FinalFileSet True Nothing Nothing)
  in  foldl' nextArg initial . fmap parseArgument
 where
  revealArgs :: Writer [String] FinalArguments -> FinalArguments
  revealArgs = fst . runWriter
  --
  nextArg :: Writer [String] FinalArguments -> CommandLineArgument -> Writer [String] FinalArguments
  nextArg accum (SwitchArgument VersionSwitch)                                            = FinalSwitch VersionSwitch <$ accum
  nextArg accum (SwitchArgument UsageSwitch)                                              = FinalSwitch UsageSwitch   <$ accum
  nextArg accum (SwitchArgument HelpSwitch)                                               = FinalSwitch HelpSwitch    <$ accum
  nextArg accum (SwitchArgument (UnknownSwitch u))                                        = tell ["Unknown argument: " ++ u ++ "."] >> accum
  nextArg accum@(revealArgs -> FinalSwitch _) (SwitchArgument AsPartSwitch)               = FinalFileSet True Nothing Nothing         <$ accum
  nextArg accum@(revealArgs -> FinalSwitch _) (FileArgument file)                         = FinalFileSet False (Just file) Nothing    <$ accum
  nextArg accum@(revealArgs -> FinalFileSet _ input output) (SwitchArgument AsPartSwitch) = FinalFileSet True input output            <$ accum
  nextArg accum@(revealArgs -> FinalFileSet p Nothing Nothing) (FileArgument input)       = FinalFileSet p (Just input) Nothing       <$ accum
  nextArg accum@(revealArgs -> FinalFileSet p (Just input) Nothing) (FileArgument output) = FinalFileSet p (Just input) (Just output) <$ accum
  nextArg accum@(revealArgs -> FinalFileSet p (Just _) (Just _)) (FileArgument fname)     = tell ["Unused extra filename:\"" ++ fname ++ "\"."] >> accum

