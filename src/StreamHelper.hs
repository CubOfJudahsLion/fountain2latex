{-|
Module      : IOStreams
Description : Sets up input and output streams from arguments
Copyright   : (c) Alexander Feterman Naranjo, 2023
License     : MIT
Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
Stability   : experimental
Portability : POSIX,Windows
-}

{-# LANGUAGE ScopedTypeVariables #-}

module StreamHelper ( readInputStream, writeOutputStream ) where


import Control.Exception ( catch, IOException )
import Data.Char ( toLower )
import Data.List ( isSuffixOf, partition, intercalate )
import Data.Text ( Text )
import qualified Data.Text.IO as T ( getContents, readFile
                                   , putStr, writeFile )
import System.Directory ( doesFileExist )


fixInputFileName :: FilePath -> IO (Either [FilePath] FilePath)
fixInputFileName fileName = do
  fileExists <- doesFileExist fileName
  if fileExists then
    pure $ Right fileName
  else if ".fountain" `isSuffixOf` (toLower <$> fileName) then
    pure $ Left [fileName]
  else do
    let fileNamesWithExt = (fileName ++) <$> [".fountain", ".Fountain", ".FOUNTAIN"]
    fileAndExistenceTestPairs <- zip fileNamesWithExt <$> mapM doesFileExist fileNamesWithExt
    let (existentFiles, nonExistentFiles) = partition snd fileAndExistenceTestPairs
    pure $ if null existentFiles
      then  Left $ fst <$> nonExistentFiles
      else  Right $ fst $ head existentFiles


-- |  Reads the input stream whether it's from an actual file or standard input.
readInputStream :: Maybe FilePath -> IO (Either String Text)
readInputStream Nothing = (Right <$> T.getContents) `catch` (\ (_ :: IOException) -> pure (Left "Couldn't read stdin."))
readInputStream (Just fileName) = do
  fixAttempt <- fixInputFileName fileName
  case fixAttempt of
    Right fixedFileName       ->
      catch
        (T.readFile fixedFileName >>= \ fileContent -> pure $ Right fileContent)
        (\ (_ :: IOException) -> pure $ Left $ "Couldn't read file \"" ++ fixedFileName ++ "\".")
    Left []                   ->
      pure $ Left "No files found."
    Left failedFilenameFixes  ->
      pure $ Left $   "File"
                  ++  if null failedFilenameFixes
                        then  " \""  ++ head failedFilenameFixes
                        else  "s \""
                          ++  intercalate "\", \"" (init failedFilenameFixes)
                          ++ "\" and \""
                          ++ last failedFilenameFixes
                  ++  "\" not found."


fountainExtLower :: String
fountainExtLower = ".fountain"

latexExtLower :: String
latexExtLower = ".tex"

fixOutputFileName :: FilePath -> FilePath -> FilePath
fixOutputFileName inFile "."
  | fountainExtLower `isSuffixOf` fmap toLower inFile =
      take (length inFile - length fountainExtLower) inFile ++ latexExtLower
  | otherwise                                         =
      inFile ++ latexExtLower
fixOutputFileName _ outFile
  | latexExtLower `isSuffixOf` fmap toLower outFile   =
      outFile
  | otherwise                                         =
      outFile ++ latexExtLower


-- |  Writes the text into file or standard output, depending on the parameters.
writeOutputStream :: Maybe (FilePath, Maybe FilePath) -> Text -> IO (Either String ())

writeOutputStream (Just (inFile, Just outFile)) text =
  let outFile' = fixOutputFileName inFile outFile
  in  catch
    (Right <$> T.writeFile outFile' text)
    (\ (_ :: IOException) -> pure $ Left $ "Couldn't write file \"" ++ outFile' ++ "\".")

writeOutputStream _ text =
  catch
    (Right <$> T.putStr text)
    (\ (_ :: IOException) -> pure (Left "Couldn't write to stdout."))

