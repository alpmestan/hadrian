module Hadrian.Oracles.DirectoryContents (
    directoryContents, directoryContentsOracle, Match (..), matchAll
    ) where

import Control.Monad
import Development.Shake
import Development.Shake.Classes
import GHC.Generics
import System.Directory.Extra

import Hadrian.Utilities

data Match = Test FilePattern | Not Match | And [Match] | Or [Match]
    deriving (Generic, Eq, Show, Typeable)

instance Binary   Match
instance Hashable Match
instance NFData   Match

-- | A 'Match' expression that always evaluates to 'True' (i.e. always matches).
matchAll :: Match
matchAll = And []

-- | Check if a file name matches a given 'Match' expression.
matches :: Match -> FilePath -> Bool
matches (Test p) f = p ?== f
matches (Not  m) f = not $ matches m f
matches (And ms) f = all (`matches` f) ms
matches (Or  ms) f = any (`matches` f) ms

-- | Given a 'Match' expression and a directory, recursively traverse it and all
-- its subdirectories to find and return all matching contents.
directoryContents :: Match -> FilePath -> Action [FilePath]
directoryContents expr dir = askOracle $ DirectoryContents (expr, dir)

newtype DirectoryContents = DirectoryContents (Match, FilePath)
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)

-- | This oracle answers 'directoryContents' queries and tracks the results.
directoryContentsOracle :: Rules ()
directoryContentsOracle = void $
    addOracle $ \(DirectoryContents (expr, dir)) -> liftIO $ map unifyPath .
        filter (matches expr) <$> listFilesInside (return . matches expr) dir