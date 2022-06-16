-- Provides a common entry point for programs based on the CPSA library.

-- Copyright (c) 2009 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module CPSA.Lib.Entry (start, usage, abort, success, Options(..),
                       defaultOptions, algOptions, algInterp,
                       filterOptions, filterInterp, readSExpr,
                       gentlyReadSExpr, outputHandle, writeSExpr,
                       writeLnSExpr, cpsaVersion, comment,
                       writeComment, tryIO) where

import Numeric
import Control.Exception (try)
import System.IO
import System.IO.Error (ioeGetErrorString)
import System.Environment
import System.Console.GetOpt
import System.Exit
import Data.Version
import Paths_cpsa
import qualified CPSA.Basic.Algebra
import CPSA.Lib.SExpr
import CPSA.Lib.Printer

-- Returns the input S-expression and an interpretation of the command
-- line options.
start :: [OptDescr a] -> ([a] -> IO b) -> IO (PosHandle, b)
start options interp =
    do
      argv <- getArgs
      (flags, files) <- opts options argv
      opts <- interp flags
      p <- openInput options files
      return (p, opts)

opts :: [OptDescr a] -> [String] -> IO ([a], [String])
opts options argv =
    case getOpt RequireOrder options argv of
      (o, n, []) -> return (o, n)
      (_, _, errs) ->
          do
            msg <- usage options errs
            abort msg

openInput ::  [OptDescr a] -> [String] -> IO PosHandle
openInput _ [file] =
    do                          -- Input from named file
      input <- openFile file ReadMode
      posHandle file input
openInput _ [] =
    posHandle "" stdin          -- Input from the standard input
openInput options _ =
    do
      msg <- usage options ["too many input files\n"]
      abort msg

usage :: [OptDescr a] -> [String] -> IO String  -- Return usage string
usage options errs =
    do
      name <- getProgName
      datadir <- getDataDir
      let header = "Usage: " ++ name ++ " [OPTIONS] [FILE]"
      let footer = "\nDocumentation directory: " ++ datadir
      return (concat errs ++ usageInfo header options ++ footer)

-- CPSA options and default values

data Options = Options {
      optFile :: Maybe FilePath, -- Nothing specifies standard output
      optAlg :: String,          -- Name of the algebra
      optAnalyze :: Bool,        -- False when only expanding macros
      optDoAnalyze :: Bool,      -- False when only printing inputs
      optNoIsoChk :: Bool, -- True when not performing isomorphism checks
      optCheckNoncesFirst :: Bool, -- True when checking nonces first
      optTryOldStrandsFirst :: Bool, -- True when visiting old strands first
      optTryYoungNodesFirst :: Bool, -- True when visiting young nodes first
      optGoalsSat :: Bool , -- True when goals satisfied stops tree expansion
      optLimit :: Int,          -- Step count limit
      optBound :: Int,          -- Strand cound bound
      optDepth :: Int,          -- Tree depth bound
      optMargin :: Int,         -- Output line length
      optIndent :: Int }        -- Pretty printing indent
    deriving Show

defaultOptions :: Options
defaultOptions = Options {
  optFile = Nothing,
  optAlg = CPSA.Basic.Algebra.name,
  optAnalyze = True,
  optDoAnalyze = True,
  optNoIsoChk = False,
  optCheckNoncesFirst = False,
  optTryOldStrandsFirst = False,
  optTryYoungNodesFirst = False,
  optGoalsSat = False,
  optLimit = 2000,
  optBound = 12,
  optDepth = 0,
  optMargin = 72,
  optIndent = 2 }

-- Command line option flags
data Flag
    = Help                      -- Help
    | Info                      -- Version information
    | Algebra String            -- Algebra
    | Algebras                  -- Show algebras
    | Margin String             -- Output line length
    | Output String             -- Output file name
      deriving Show

algOptions :: String -> [OptDescr Flag]
algOptions defaultAlgebra =
    [ Option ['o'] ["output"]  (ReqArg Output "FILE")  "output FILE",
      Option ['m'] ["margin"]  (ReqArg Margin "INT")
      ("set output margin (default " ++ show (optMargin defaultOptions) ++ ")"),
      Option ['a'] ["algebra"]  (ReqArg Algebra "STRING")
      ("algebra (default " ++ defaultAlgebra ++ ")"),
      Option ['s'] ["show-algebras"] (NoArg Algebras)  "show algebras",
      Option ['h'] ["help"]    (NoArg Help)          "show help message",
      Option ['v'] ["version"] (NoArg Info)          "show version number" ]

data Params = Params
    { file :: Maybe FilePath,   -- Nothing specifies standard output
      alg :: String,            -- Name of the algebra
      margin :: Int }           -- Output line length
    deriving Show

-- Interpret option flags
algInterp :: String -> [String] -> [Flag] -> IO (Maybe FilePath, String, Int)
algInterp alg algs flags =
    loop flags (Params { file = Nothing, alg = alg,
                         margin = optMargin defaultOptions })
    where
      loop [] (Params { file = file, alg = alg, margin = margin}) =
          return (file, alg, margin)
      loop (Output name : flags)  params
          | file params == Nothing =
              loop flags $ params { file = Just name }
      loop (Margin value : flags) params =
          case readDec value of
            [(margin, "")] ->
                loop flags $ params { margin = margin }
            _ ->
                do
                  msg <- usage (algOptions alg) ["Bad value for margin\n"]
                  abort msg
      loop (Algebra name : flags) params
          | elem name algs = loop flags $ params { alg = name }
          | otherwise =
              abort ("Algebra " ++ name ++ " not one of\n" ++ unlines algs)
      loop (Algebras : _) _ =
          success $ unlines algs
      loop (Info : _) _ =
          success cpsaVersion
      loop (Help : _) _ =
          do                    -- Show help then exit with success
            msg <- usage (algOptions alg) []
            success msg
      loop _ _ =
           do                   -- Show help then exit with failure
             msg <- usage (algOptions alg) ["Bad option combination\n"]
             abort msg

filterOptions :: [OptDescr Flag]
filterOptions =
    [ Option ['o'] ["output"]  (ReqArg Output "FILE")  "output FILE",
      Option ['m'] ["margin"]  (ReqArg Margin "INT")
      ("set output margin (default " ++ show (optMargin defaultOptions) ++ ")"),
      Option ['h'] ["help"]    (NoArg Help)          "show help message",
      Option ['v'] ["version"] (NoArg Info)          "show version number" ]

-- Interpret option flags
filterInterp :: [Flag] -> IO (Maybe FilePath, Int)
filterInterp flags =
    loop flags Nothing (optMargin defaultOptions)
    where
      loop [] file margin =
          return (file, margin)
      loop (Output name : flags) Nothing margin =
          loop flags (Just name) margin
      loop (Margin value : flags) file _ =
          case readDec value of
            [(margin, "")] ->
                loop flags file margin
            _ ->
                do
                  msg <- usage filterOptions ["Bad value for margin\n"]
                  abort msg
      loop (Info : _) _ _ =
          success cpsaVersion
      loop (Help : _) _ _ =
          do                    -- Show help then exit with success
            msg <- usage filterOptions []
            success msg
      loop _ _ _ =
           do                   -- Show help then exit with failure
             msg <- usage filterOptions ["Bad option combination\n"]
             abort msg

-- Contruct the output handle specified by the command line.
outputHandle :: Maybe FilePath -> IO (Handle)
outputHandle output =
    case output of
      Nothing -> return stdout
      Just file -> openFile file WriteMode

abort :: String -> IO a         -- Print message then exit with failure
abort msg =
    do
      hPutStrLn stderr msg
      exitFailure

success :: String -> IO a       -- Print message then exit with success
success msg =
    do
      hPutStrLn stderr msg
      exitWith ExitSuccess

-- S-expression pretty print parameters
indent :: Int
indent = optIndent defaultOptions

writeSExpr :: Handle -> Int -> SExpr a -> IO ()
writeSExpr h margin sexpr =
    hPutStrLn h (pp margin indent sexpr)

writeLnSExpr :: Handle -> Int -> SExpr a -> IO ()
writeLnSExpr h margin sexpr =
    do
      hPutStrLn h ""
      writeSExpr h margin sexpr

cpsaVersion :: String
cpsaVersion = showString "CPSA " (showVersion version)

comment :: String -> SExpr ()
comment msg =
    L () [S () "comment", stringSExpr msg]

writeComment :: Handle -> Int -> String -> IO ()
writeComment h margin msg =
    writeSExpr h margin (comment msg)

-- Read an S-expression, and fail on error
readSExpr :: PosHandle -> IO (Maybe (SExpr Pos))
readSExpr p =
    do
      x <- tryIO (load p)
      case x of
        Right x ->
            return x
        Left err ->
            abort err

-- Read an S-expression, and gently fail on error by printing the
-- error message to standard error and returning EOF.
gentlyReadSExpr :: PosHandle -> IO (Maybe (SExpr Pos))
gentlyReadSExpr p =
    do
      x <- tryIO (load p)
      case x of
        Right x ->
            return x
        Left err ->
            do
              hPutStrLn stderr err
              return Nothing

-- Exception handling for IO errors
tryIO :: IO a -> IO (Either String a)
tryIO x =
    do
      y <- try x
      case y of
        Right x ->
            return (Right x)
        Left err ->
            return (Left (ioeGetErrorString err))
