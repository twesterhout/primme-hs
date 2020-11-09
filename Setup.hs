import Control.Monad (unless)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Distribution.PackageDescription
import Distribution.Simple
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
import Distribution.System
import qualified Distribution.Verbosity as Verbosity
import System.Directory (getCurrentDirectory)

main = defaultMainWithHooks hooks
  where
    hooks =
      simpleUserHooks
        { preConf = buildLibPrimme,
          confHook = \a f -> confHook simpleUserHooks a f >>= updateFinalDirs,
          preBuild = updateLocalDirs,
          postCopy = copyLibPrimme,
          postClean = cleanLibPrimme
        }

buildLibPrimme :: Args -> ConfigFlags -> IO HookedBuildInfo
buildLibPrimme _ flags = do
  let verbosity = fromFlag $ configVerbosity flags
      useSystem = getCabalFlag "use_system_libprimme" flags
  unless useSystem $ do
    notice verbosity "Building libprimme.a C library..."
    rawSystemExit verbosity "./build_primme.sh" []
  return emptyHookedBuildInfo

updateLocalDirs :: Args -> BuildFlags -> IO HookedBuildInfo
updateLocalDirs _ flags = do
  dir <- getCurrentDirectory
  let buildInfo =
        emptyBuildInfo
          { extraLibDirs = [dir <> "/third_party/primme/lib"],
            includeDirs = [dir <> "/third_party/primme/include"]
          }
  return (Just buildInfo, [])

updateFinalDirs :: LocalBuildInfo -> IO LocalBuildInfo
updateFinalDirs localBuildInfo =
  return localBuildInfo {localPkgDescr = packageDescription {library = Just lib'}}
  where
    packageDescription = localPkgDescr localBuildInfo
    lib = case library packageDescription of
      Just x -> x
      Nothing -> error "this should not have happened; did you remove the library target?"
    libBuild = libBuildInfo lib
    libPref = libdir $ absoluteInstallDirs packageDescription localBuildInfo NoCopyDest
    lib' = lib {libBuildInfo = libBuild {extraLibDirs = libPref : extraLibDirs libBuild}}

copyLib :: ConfigFlags -> LocalBuildInfo -> FilePath -> IO ()
copyLib flags _ libPref =
  unless useSystem $ do
    notice verbosity $ "Installing libprimme.a C library..."
    installMaybeExecutableFile verbosity ("third_party/primme/lib" <> libName) (libPref <> libName)
  where
    verbosity = fromFlag $ configVerbosity flags
    useSystem = getCabalFlag "use_system_libprimme" flags
    libName = "/libprimme.a"

copyLibPrimme :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
copyLibPrimme _ flags packageDescription localBuildInfo = copyLib config localBuildInfo libPref
  where
    libPref = libdir . absoluteInstallDirs packageDescription localBuildInfo . fromFlag . copyDest $ flags
    config = configFlags localBuildInfo

cleanLibPrimme :: Args -> CleanFlags -> PackageDescription -> () -> IO ()
cleanLibPrimme _ flags _ _ = rawSystemExit verbosity "./build_primme.sh" ["--clean"]
  where
    verbosity = fromFlag $ cleanVerbosity flags

getCabalFlag :: String -> ConfigFlags -> Bool
getCabalFlag name flags = fromMaybe False (lookupFlagAssignment (mkFlagName name') allFlags)
  where
    allFlags = configConfigurationsFlags flags
    name' = map toLower name
