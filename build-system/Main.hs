import           Control.Applicative hiding ((*>))
import           Control.Monad
import           Data.List
import           Haskell
import           Control.Lens hiding (Action, (<.>))
import           Data.Maybe
import qualified Data.Set as S
import           Data.Traversable (for)
import           Development.Shake
import           Development.Shake.FilePath
import           Qt
import           Shake.Configure

-- | CXX compiler
cxx :: String
cxx = "/usr/bin/g++"

-- | CXX compiler flags
cxxflags :: [String]
cxxflags = concat 
  [ ["-std=c++11", "-Wall", "-Werror"]
  , map ("-I" ++) includeDirs
  ]

-- | Include directories
includeDirs :: [String]
includeDirs =
  [ "cpp"
  , builddir </> "stubs"
  ]

-- | Build directory
builddir :: String
builddir = "_build"

-- | Flags to use when compiling haskell sources
ghcflags :: [String]
ghcflags = flags ++ concatMap (\x -> ["-package",x]) packages
  where flags = ["-hide-all-packages"]
        packages =
          [ "lens"
          , "base"
          , "vplan"
          , "aeson"
          , "semigroups"
          , "groups"
          , "void"
          , "bifunctors"
          , "bytestring"
          ]

-- | Search the include path for the given file name.
resolveInclude :: [FilePath] -> FilePath -> Action (Maybe FilePath)
resolveInclude [] _ = return Nothing
resolveInclude (p:ps) f = do
  e <- doesFileExist $ p </> f
  if e 
    then return $ Just $ p </> f
    else resolveInclude ps f

-- | Drop the first N directories from a given path.
dropDirectoryN :: Int -> FilePath -> FilePath
dropDirectoryN n = joinPath . drop n . splitPath

-- | Convert a haskell module name to a file name.
moduleToFileName :: String -> String
moduleToFileName = (<.> "hs") . map r
  where r '.' = '/'
        r x = x

-- | Run the C++ compiler, collecting all flags from the given configuration.
runCXX :: Config -> FilePath -> FilePath -> Action ()
runCXX config source out = do
      need [source]
      flags <- withPackages config getCompilerFlags
      system' cxx $ cxxflags ++ flags ++ ["-o", out, "-c", source]

-- | This function drops the build prefix from a file name. The build prefix is "build/*".
dropBuildPrefix :: FilePath -> FilePath
dropBuildPrefix = dropDirectory1 . dropDirectory1

-- | Find all required object files
findObjects :: Action [FilePath]
findObjects = do
  cppSources <- getDirectoryFiles "cpp" ["//*.cpp"]
  hsSources <- getDirectoryFiles "hs" ["//*.hs"]
  cppHeaders <- getDirectoryFiles "cpp" ["//*.hpp"]
  return $ concat
    [ [ builddir </> "objects/cpp" </> s -<.> "cpp.o" | s <- cppSources, listToMaybe s /= Just '.']
    , [ builddir </> "objects/hs" </> s  -<.> "hs.o"  | s <- hsSources, listToMaybe s /= Just '.']
    , [ builddir </> "objects/moc" </> s -<.> "moc.o" | s <- cppHeaders, listToMaybe s /= Just '.']
    ]

main :: IO ()
main = shakeArgs shakeOptions $ do
    config <- findPackages (builddir </> "info/configure") [qt5core, qt5gui, qt5quick, qt5qml, haskell]

    [builddir ++ "/objects/hs//*.hs.o", builddir ++ "/interfaces/hs//*.hi", builddir ++ "/stubs/hs//*_stub.h"] *>> \[outO, _, _] -> do
      let getGHC x = fromMaybe (error "GHC not found") $ x ^? variables . ix "ghc"
      let source = dropDirectoryN 2 outO -<.> ""
      need [source]
      need =<< readFileLines (builddir </> "info/deps" </> source <.> "deps")      
      join $ withPackage config haskell $ (. getGHC) $ flip system' $ concat
        [ ghcflags
        , ["-c", source]
        , ["-hidir", builddir </> "interfaces/hs"]
        , ["-stubdir", builddir </> "stubs/hs"]
        , ["-o", outO]
        ]

    builddir ++ "/info/deps/hs/*.hs.dep" *> \out -> do
      let source = dropDirectoryN 3 out -<.> ""
      imports <- map (("hs" </>) . moduleToFileName) . parseImports <$> readFileLines source
      filterM doesFileExist imports >>= writeFileChanged out . unlines

    builddir ++ "/info/deps//*.deps" *> \out -> do
      dep <- readFileLines (replaceExtension out "dep")
      deps <- traverse (\x -> readFileLines (builddir </> "info/deps" </> x <.> "deps")) dep
      writeFileChanged out $ unlines $ S.toList $ S.fromList $ dep ++ concat deps

    builddir ++ "/info/deps/hs//*_stub.h.dep" *> \out -> do
      let source = builddir </> "stubs" </> dropDirectoryN 3 out -<.> ""
      need [source]
      writeFile' out ""

    [builddir ++ "/info/deps/cpp//*.cpp.dep", builddir ++ "/info/deps/cpp//*.hpp.dep"] **> \out -> do
      src <- readFileLines $ dropDirectoryN 3 $ dropExtension out
      let incs = [init y | x <- src, Just y <- [stripPrefix "#include \"" x]]
      let curdir = takeDirectory $ dropDirectoryN 3 out
      incs' <- for incs $ fmap . fromMaybe <*> resolveInclude [curdir]
      writeFileChanged out $ unlines incs'

    builddir ++ "/generated/moc//*.moc.cpp" *> \out -> do
      let source = "cpp" </> dropExtension (dropDirectoryN 3 out) -<.> "hpp"
      need [source]
      join $ withPackage config qt5core $ qt5moc source $ ["-o", out] ++ map ("-I" ++) includeDirs

    builddir ++ "/objects/cpp//*.cpp.o" *> \out -> do
      let source = "cpp" </> dropDirectoryN 3 out -<.> ""
      deps <- readFileLines (builddir </> "info/deps" </> source <.> "deps")
      for deps (fmap . fromMaybe <*> resolveInclude includeDirs) >>= need
      runCXX config source out

    builddir ++ "/objects/moc//*.moc.o" *> \out -> do
      let source = builddir </> "generated/moc" </> dropDirectoryN 3 out -<.> "cpp"
      runCXX config source out

    builddir ++ "/bin/vplan-qml" *> \out -> do
      objs <- findObjects
      need objs
      ldflags <- withPackages config getLinkerFlags
      system' "ghc" $ concat
        [ ["-no-hs-main"]
        , ["-o", out]
        , ["-optl -lstdc++"]
        , ghcflags 
        , objs
        , map ("-optl" ++) ldflags
        ]
    return ()
  
