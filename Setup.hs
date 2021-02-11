import Control.Monad (forM_, unless)
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
import System.Directory (getCurrentDirectory, listDirectory)

main = defaultMainWithHooks autoconfUserHooks

-- main = defaultMainWithHooks hooks
--   where
--     hooks =
--       simpleUserHooks
--         { preConf = buildLibPrimme,
--           confHook = \a f -> confHook simpleUserHooks a f >>= updateExtraDirs,
--           postConf = \_ _ _ _ -> return (),
--           preBuild = updateLocalDirs,
--           postCopy = copyLibPrimme,
--           postClean = cleanLibPrimme
--         }

buildLibPrimme :: Args -> ConfigFlags -> IO HookedBuildInfo
buildLibPrimme _ flags = do
  let verbosity = fromFlag $ configVerbosity flags
      useSystem = getCabalFlag "use_system_libprimme" flags
      useShared = getCabalFlag "use_shared_libs" flags
      useAccelerate = getCabalFlag "use_accelerate" flags
      useOpenBLAS = getCabalFlag "use_openblas" flags
      extraArgs =
        ["--verbose"] <> (if useShared then ["--shared"] else [])
          <> (if useOpenBLAS then ["--use-openblas"] else [])
          <> (if useAccelerate then ["--use-accelerate"] else [])
  dir <- getCurrentDirectory
  unless useSystem $ do
    notice verbosity "Building PRIMME C library..."
    rawSystemExit verbosity "bash" $ "build_primme.sh" : extraArgs
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

-- updateFinalDirs :: LocalBuildInfo -> IO LocalBuildInfo
-- updateFinalDirs localBuildInfo =
--   return localBuildInfo {localPkgDescr = packageDescription {library = Just lib'}}
--   where
--     packageDescription = localPkgDescr localBuildInfo
--     lib = case library packageDescription of
--       Just x -> x
--       Nothing -> error "this should not have happened; did you remove the library target?"
--     libBuild = libBuildInfo lib
--     libPref = libdir $ absoluteInstallDirs packageDescription localBuildInfo NoCopyDest
--     lib' = lib {libBuildInfo = libBuild {extraLibDirs = libPref : extraLibDirs libBuild}}

updateExtraDirs :: LocalBuildInfo -> IO LocalBuildInfo
updateExtraDirs localBuildInfo = do
  dir <- getCurrentDirectory
  let lib' =
        lib
          { libBuildInfo =
              libBuild
                { extraLibDirs = libPref : extraLibDirs libBuild
                }
          }
  return localBuildInfo {localPkgDescr = packageDescription {library = Just $ lib'}}
  where
    packageDescription = localPkgDescr localBuildInfo
    lib = case library packageDescription of
      Just x -> x
      Nothing -> error "this should not have happened; did you remove the library target?"
    libBuild = libBuildInfo lib
    libPref = libdir $ absoluteInstallDirs packageDescription localBuildInfo NoCopyDest

copyLib :: ConfigFlags -> LocalBuildInfo -> FilePath -> IO ()
copyLib flags localBuildInfo libPref =
  unless useSystem $ do
    notice verbosity $ "Installing PRIMME C library..."
    libDir <- (<> "/third_party/primme/lib") <$> getCurrentDirectory
    libraries <- listDirectory libDir
    forM_ libraries $ \f -> do
      installMaybeExecutableFile verbosity (libDir <> "/" <> f) (libPref <> "/" <> f)
  where
    verbosity = fromFlag $ configVerbosity flags
    useSystem = getCabalFlag "use_system_libprimme" flags
    useShared = getCabalFlag "use_shared_libs" flags
    Platform _ os = hostPlatform localBuildInfo
    extension = case os of
      Windows -> if useShared then "dll" else "a"
      OSX -> if useShared then "dylib" else "a"
      _ -> if useShared then "so" else "a"
    libName = "/libprimme." <> extension

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
