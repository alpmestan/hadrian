module Settings.Default (
    SourceArgs (..), sourceArgs, defaultBuilderArgs, defaultPackageArgs,
    defaultArgs, defaultLibraryWays, defaultRtsWays, defaultFlavour, defaultSplitObjects
    ) where

import Types.Flavour
import Types.Expression

data SourceArgs = SourceArgs
    { hsDefault  :: Args
    , hsLibrary  :: Args
    , hsCompiler :: Args
    , hsGhc      :: Args }

sourceArgs :: SourceArgs -> Args

defaultBuilderArgs, defaultPackageArgs, defaultArgs :: Args
defaultLibraryWays, defaultRtsWays :: Ways
defaultFlavour :: Flavour
defaultSplitObjects :: Predicate
