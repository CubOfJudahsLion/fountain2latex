{-|
Module      : CommandLine
Description : Processes command-line arguments.
Copyright   : (c) Alexander Feterman Naranjo, 2023
License     : MIT
Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
Stability   : experimental
Portability : POSIX,Windows
-}

module CommandLine
    ( CommandLineSwitch(..)
    , SolvedArgumentSet(..)
    , solveArguments
    ) where


import Control.Monad.Writer ( Writer, runWriter, tell )
import Data.Char ( toLower )
import Data.List ( foldl' )


-- |  Types of switches that can be passed through the command line.
data CommandLineSwitch = VersionSwitch          -- ^  -v or --version
                       | UsageSwitch            -- ^  -u or --usage
                       | HelpSwitch             -- ^  -h or --help
                       | AsPartSwitch           -- ^  -p or --as-part
                       | UnknownSwitch String   -- ^  any other (unrecognized) switch

instance Show CommandLineSwitch where
    show VersionSwitch      = "--version"
    show UsageSwitch        = "--usage"
    show HelpSwitch         = "--help"
    show AsPartSwitch       = "--as-part"
    show (UnknownSwitch u)  = u


data CommandLineArgument  = SwitchArgument CommandLineSwitch
                          | FileArgument FilePath
                        deriving(Show)

parseArgument :: String -> CommandLineArgument
parseArgument arg = parseLower $ fmap toLower arg
  where
    parseLower :: String -> CommandLineArgument
    parseLower "-v"         = SwitchArgument VersionSwitch
    parseLower "--version"  = SwitchArgument VersionSwitch
    parseLower "-u"         = SwitchArgument UsageSwitch
    parseLower "--usage"    = SwitchArgument UsageSwitch
    parseLower "-h"         = SwitchArgument HelpSwitch
    parseLower "--help"     = SwitchArgument HelpSwitch
    parseLower "-p"         = SwitchArgument AsPartSwitch
    parseLower "--as-part"  = SwitchArgument AsPartSwitch
    parseLower u@('-':_)    = SwitchArgument (UnknownSwitch u)
    parseLower _            = FileArgument arg


-- |  Target form of accumulated arguments after @solveArguments@.
data SolvedArgumentSet  = SolvedSwitch   CommandLineSwitch
                        | SolvedStdIO    Bool
                        | SolvedOneFile  { asPart :: Bool
                                         , input  :: FilePath }
                        | SolvedTwoFiles { asPart :: Bool
                                         , input  :: FilePath
                                         , output :: FilePath }
                      deriving(Show)

-- |  Goes through arguments in order, trying to arrive at
--    a final argument set. When new arguments are
--    contradictory, the previous ones are disposed.
solveArguments :: [String] -> Writer [String] SolvedArgumentSet
solveArguments =
  let
    initial = pure (SolvedStdIO False)
  in
    foldl' nextArg initial . fmap parseArgument
  where
    nextArg :: Writer [String] SolvedArgumentSet -> CommandLineArgument -> Writer [String] SolvedArgumentSet
    nextArg accum = accumCase $ fst $ runWriter accum
      where
        accumCase :: SolvedArgumentSet
                  -> CommandLineArgument
                  -> Writer [String] SolvedArgumentSet
        accumCase _ (SwitchArgument VersionSwitch) =
          SolvedSwitch VersionSwitch              <$ accum
        accumCase _ (SwitchArgument UsageSwitch) =
          SolvedSwitch UsageSwitch                <$ accum
        accumCase _ (SwitchArgument HelpSwitch) =
          SolvedSwitch HelpSwitch                 <$ accum
        accumCase _ (SwitchArgument (UnknownSwitch u)) =
          tell ["Unknown switch: " ++ u ++ "."]   >> accum
        accumCase (SolvedSwitch _) (SwitchArgument AsPartSwitch) =
          SolvedStdIO True                        <$ accum
        accumCase (SolvedStdIO _) (SwitchArgument AsPartSwitch) =
          SolvedStdIO True                        <$ accum
        accumCase (SolvedOneFile _ file) (SwitchArgument AsPartSwitch) =
          SolvedOneFile True file                 <$ accum
        accumCase (SolvedTwoFiles _ inFile outFile) (SwitchArgument AsPartSwitch) =
          SolvedTwoFiles True inFile outFile      <$ accum
        accumCase (SolvedSwitch _) (FileArgument inFile) =
          SolvedOneFile False inFile              <$ accum
        accumCase (SolvedStdIO asPart) (FileArgument inFile) =
          SolvedOneFile asPart inFile             <$ accum
        accumCase (SolvedOneFile asPart inFile) (FileArgument outFile) =
          SolvedTwoFiles asPart inFile outFile    <$ accum
        accumCase (SolvedTwoFiles {}) (FileArgument fname) =
          tell ["Unused extra filename: \"" ++ fname ++ "\"."] >> accum

