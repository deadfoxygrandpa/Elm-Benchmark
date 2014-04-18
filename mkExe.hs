{-# OPTIONS_GHC -Wall #-}

import           Control.Monad
import           System.Exit
import           System.FilePath
import           System.Process
import           System.Environment
import           System.IO (hClose, openTempFile)
import           System.Directory

import           Elm.Internal.Paths as Elm
import qualified Paths_ElmBenchmark as ElmBenchmark

catToFile :: [FilePath] -> FilePath -> IO ()
catToFile files outfile = do
	writeFile outfile ""
	forM_ files $ \filename -> do
	   	contents <- readFile filename
	   	appendFile outfile contents

buildJS :: ExitCode -> FilePath -> FilePath -> IO ()
buildJS code infile outfile = case code of
  ExitFailure _ -> do
    putStrLn "Something went wrong during compilation"
    exitWith code
  ExitSuccess -> do
    putStrLn "Making exe"
    prescript    <- ElmBenchmark.getDataFileName "prescript.js"
    postscript   <- ElmBenchmark.getDataFileName "postscript.js"
    catToFile [ prescript
              , Elm.runtime
              , "build" </> replaceExtension infile "js"
              , postscript ]
              outfile

buildJSFromJS :: FilePath -> FilePath -> IO ()
buildJSFromJS infile outfile = do
  putStrLn "Making exe"
  prescript    <- ElmBenchmark.getDataFileName "prescript.js"
  postscript   <- ElmBenchmark.getDataFileName "postscript.js"
  catToFile [ prescript
            , Elm.runtime
            , infile
            , postscript ]
            outfile

compile :: FilePath -> IO ExitCode
compile infile = do
  rawSystem "elm" ["-mo", infile]

compileScripts :: FilePath -> FilePath -> IO ExitCode
compileScripts infile scripts = do
  rawSystem "elm" ["-mo", "--scripts=" ++ scripts, infile]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [infile, outfile] ->
      case takeExtension infile of
        ".elm" -> do code <- compile infile
                     buildJS code infile outfile
        ".js"  -> buildJSFromJS infile outfile
        _ -> putStrLn $ "Expected input file and output file arguments, but got " ++ show (length args) ++ " args."
    [infile, scripts, outfile] -> do
      code <- compileScripts infile scripts
      buildJS code infile outfile
    _ -> putStrLn $ "Expected input file and output file arguments, but got " ++ show (length args) ++ " args."
