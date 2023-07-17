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


import GitVersion


main :: IO ()
main = putStrLn $ "fountain2latex version " ++ gitVersion

