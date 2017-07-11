-- A simple, CPSA specific make system

module Make3 (cpsa, shapes, sas, annos, params, cleanse, get, set,
              build, clean, roots) where

{- Place a copy of this source file in the directory used to store
CPSA problem statements, edit it to suit your needs, and load it into
a Haskell interpreter.

Normally, just the build and the clean command are used.  It's the
build command that you usually modify.

To analyze a problem in prob.scm, type:

*Make> cpsa "prob"

If successful, the analysis is in the file prob.xhtml, which can be
viewed with a standards-compliant browser.

For a shapes only version of the analysis, type:

*Make> shapes "prob"

If successful, the shapes are in the file prob_shapes.xhtml.

*Make> sas "prob"

If successful, the shape analysis sentences are in the file
prob_sas.text.

When the protocol is annotated with rely-guarantee formulas, type:

*Make> annos "prob"

If successful, the annotated shapes are in the file prob_annotations.xhtml.

*Make> params "prob"

If successful, roles with parameter descriptions are in prob_parameters.txt.

To remove the files generated from source files, type:

*Make> cleanse "prob"

To see the command-line options used by CPSA, type:

*Make> get

To change the command-line options used by CPSA to "-b 15", type:

*Make> set "-b 15"

To analyze all source files in the directory, type:

*Make> build

To remove the files generated from source files in the directory, type:

*Make> clean

-}

import Control.Monad (mapM_)
import Data.List (sort)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Exit (ExitCode (..))
import System.Process (system)
import System.IO (putStrLn)
import System.IO.Unsafe (unsafePerformIO)
import System.FilePath (FilePath, splitExtension)
import System.Directory (removeFile, doesFileExist, getModificationTime,
                         getCurrentDirectory, getDirectoryContents)

-- Flags for CPSA

initialCpsaFlags :: String
initialCpsaFlags = "+RTS -M512m -RTS"

graphFlags :: String
graphFlags = ""
-- To enable zooming, use:
-- graphFlags = " -z"

-- A mutable location for CPSA flags
cpsaFlags :: IORef String
cpsaFlags = unsafePerformIO $ newIORef initialCpsaFlags

-- Get the CPSA flags
get :: IO String
get =
    readIORef cpsaFlags

-- Set the CPSA flags
set :: String -> IO ()
set flags =
    writeIORef cpsaFlags flags

-- Transformation rules

data Rule = Rule
    { prog :: String,           -- program to run
      inputExt :: String,       -- input file name extension
      outputExt :: String }     -- output file name extension

-- Graph Rule

graph :: FilePath -> IO ()
graph root =
    make graphRule root         -- make graph using given rule

graphRule :: Rule
graphRule =
    Rule { prog = "cpsa3graph" ++ graphFlags,
           inputExt = cpsaExt,
           outputExt = graphExt }

-- CPSA Rule

cpsa :: FilePath -> IO ()
cpsa root =
    do
      cpsaBasic root
      shapes root
      graph root

-- CPSA using Basic rule

cpsaBasic :: FilePath -> IO ()
cpsaBasic root =
    do
      flags <- get               -- get CPSA flags
      make (cpsaBasicRule flags) root -- make CPSA output using given rule

cpsaBasicRule :: String -> Rule
cpsaBasicRule flags =
    Rule { prog = "cpsa3 " ++ flags,
           inputExt = sourceBasicExt,
           outputExt = cpsaExt }

-- Shapes Rule

shapes :: FilePath -> IO ()
shapes root =
    do
      cpsaBasic root            -- Run CPSA if need be
      make shapesRule root
      graph $ root ++ shapesRoot

shapesRule :: Rule
shapesRule =
    Rule { prog = "cpsa3shapes",
           inputExt = cpsaExt,
           outputExt = shapesRoot ++ cpsaExt }

-- Sas Rule

sas :: FilePath -> IO ()
sas root =
    do
      cpsaBasic root            -- Run CPSA if need be
      make sasRule root

sasRule :: Rule
sasRule =
    Rule { prog = "cpsa3sas",
           inputExt = cpsaExt,
           outputExt = sasExt }

-- Annotations Rule

annos :: FilePath -> IO ()
annos root =
    do
      cpsa root                 -- Run CPSA and make shapes
      make annosRule root
      graph $ root ++ annosRoot

annosRule :: Rule
annosRule =
    Rule { prog = "cpsa3annotations",
           inputExt = shapesRoot ++ cpsaExt,
           outputExt = annosRoot ++ cpsaExt }

-- Parameters Rule

params :: FilePath -> IO ()
params root =
     do
       make cpsaparametersRule root

cpsaparametersRule :: Rule
cpsaparametersRule =
    Rule { prog = "cpsa3parameters",
    	   inputExt = sourceBasicExt,
	   outputExt = paramsRoot ++ cpsaExt }

-- Clean generated files

cleanse :: FilePath -> IO ()
cleanse root =
    do
      rm $ root ++ cpsaExt
      rm $ root ++ graphExt
      rm $ root ++ shapesRoot ++ cpsaExt
      rm $ root ++ shapesRoot ++ graphExt
      rm $ root ++ sasExt
      rm $ root ++ annosRoot ++ cpsaExt
      rm $ root ++ annosRoot ++ graphExt
      rm $ root ++ paramsRoot ++ cpsaExt

-- File Extensions

sourceBasicExt :: String
sourceBasicExt = ".scm"

cpsaExt :: String
cpsaExt = ".txt"

shapesRoot :: String
shapesRoot = "_shapes"

sasExt :: String
sasExt = "_sas.text"

annosRoot :: String
annosRoot = "_annotations"

paramsRoot :: String
paramsRoot = "_parameters"

graphExt :: String
graphExt = ".xhtml"

-- Rule Interpreters

-- Make output for root using rule
make :: Rule -> FilePath -> IO ()
make rule root =
    do
      let input = root ++ inputExt rule
      let output = root ++ outputExt rule
      done <- made input output
      case done of
        True -> return ()       -- Nothing to do
        False -> run (prog rule) input output

-- See if an output file is up-to-date
made :: FilePath -> FilePath -> IO Bool
made input output =
    do
      src <- doesFileExist input
      dst <- doesFileExist output
      case src && dst of
        False -> return False
        True ->
            do
              src <- getModificationTime input
              dst <- getModificationTime output
              return $ src < dst

-- Run a program with input and output from files

-- Print the command before running it.  Delete the output when the
-- command fails.
run :: String -> FilePath -> FilePath -> IO ()
run prog input output =
    do
      let cmd = prog ++ " -o " ++ output ++ " " ++ input
      putStrLn cmd
      code <- system cmd
      case code of
        ExitSuccess -> return ()
        ExitFailure _ ->
            do
              --rm output
              fail "Command failed"

-- Remove a file

-- Prints the command when there is a file to be deleted.
rm :: FilePath -> IO ()
rm output =
    do
      exists <- doesFileExist output
      case exists of
        False -> return ()      -- File doesn't exist
        True ->
            do                  -- Print command before removal
              putStrLn $ "rm " ++ output
              removeFile output

-- Return the roots of the CPSA source files in the current directory.

roots :: [String] -> IO [FilePath]
roots exts =
    do
      dir <- getCurrentDirectory
      files <- getDirectoryContents dir
      let roots = [ root |
                    file <- files,
                    let (root, ext) = splitExtension file,
                    elem ext exts ] -- Filter for source files
      return $ sort roots

-- Build the shapes for all the source files in the current directory.

build :: IO ()
build =
    do
      probs <- roots [sourceBasicExt]
      mapM_ cpsa probs

-- Clean files generated for all the source files in the current directory.

clean :: IO ()
clean =
    do
      probs <- roots [sourceBasicExt]
      mapM_ cleanse probs
