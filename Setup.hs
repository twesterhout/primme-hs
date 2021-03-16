module Main (main) where

import Control.Monad (unless)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
  ( InstallDirs (..),
    LocalBuildInfo (..),
    absoluteInstallDirs,
    localPkgDescr,
  )
import Distribution.Simple.Setup
import Distribution.Simple.Utils
  ( installMaybeExecutableFile,
    notice,
    rawSystemExit,
  )
import System.Directory (getCurrentDirectory)


main :: IO ()
main = defaultMainWithHooks $ autoconfUserHooks { postCopy = copyLibPrimme }

copyLib :: ConfigFlags -> LocalBuildInfo -> FilePath -> IO ()
copyLib flags localBuildInfo libPref =
  unless useSystem $ do
    notice verbosity $ "Installing PRIMME C library..."
    libDir <- (<> "/third_party/primme/lib") <$> getCurrentDirectory
    let f = "libprimme.a"
    installMaybeExecutableFile verbosity (libDir <> "/" <> f) (libPref <> "/" <> f)
  where
    verbosity = fromFlag $ configVerbosity flags
    useSystem = getCabalFlag "use_system_libprimme" flags

copyLibPrimme :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
copyLibPrimme _ flags packageDescription localBuildInfo = copyLib config localBuildInfo libPref
  where
    libPref = libdir . absoluteInstallDirs packageDescription localBuildInfo . fromFlag . copyDest $ flags
    config = configFlags localBuildInfo

getCabalFlag :: String -> ConfigFlags -> Bool
getCabalFlag name flags = fromMaybe False (lookupFlagAssignment (mkFlagName name') allFlags)
  where
    allFlags = configConfigurationsFlags flags
    name' = map toLower name
