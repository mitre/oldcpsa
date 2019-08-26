--  Copies important files from the CPSA data directory to the current
--  directory.

-- Copyright (c) 2019 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module Main (main) where

import System.IO
import System.Environment
import System.Console.GetOpt
import System.Exit
import System.Directory (copyFile, doesFileExist)
import Data.List (elemIndex)
import Data.Version
import Paths_cpsa

main :: IO ()
main =
  do
    argv <- getArgs
    (flags, _) <- opts options argv
    interp flags
    copy ["init/Makefile", "cpsa.mk", "Make.hs", "init/template.lisp"]

-- Copy the file only if it does not already exist

copy :: [String] -> IO ()
copy [] = return ()
copy (f : fs) =
  do
    let b = basename f
    exist <- doesFileExist b
    case exist of
      True ->
        hPutStrLn stderr ("File " ++ b ++ " exists, not overwriting")
      False ->
        do
          fp <- getDataFileName f
          copyFile fp b
          hPutStrLn stderr ("Created " ++ b)
    copy fs

basename :: String -> String
basename f =
  case elemIndex '/' f of
    Nothing -> f
    Just i -> basename (drop (i + 1) f)

-- Command-line options

data Flag
    = Help                      -- Help
    | Info                      -- Version information
      deriving Show

options :: [OptDescr Flag]
options =
    [ Option ['h'] ["help"]     (NoArg Help)           "show help message",
      Option ['v'] ["version"]  (NoArg Info)           "show version number" ]

opts :: [OptDescr a] -> [String] -> IO ([a], [String])
opts options argv =
    case getOpt RequireOrder options argv of
      (o, n, []) -> return (o, n)
      (_, _, errs) ->
          do
            msg <- use options errs
            abort msg

use :: [OptDescr a] -> [String] -> IO String  -- Return usage string
use options errs =
    do
      name <- getProgName
      datadir <- getDataDir
      let header = "Usage: " ++ name ++ " [OPTIONS]"
      let footer = "\nDocumentation directory: " ++ datadir
      return (concat errs ++ usageInfo header options ++ footer)

-- Interpret option flags

interp :: [Flag] -> IO ()
interp flags =
    loop flags
    where
      loop [] = return ()
      loop (Info : _) =
          success cpsaVersion
      loop (Help : _) =
          do                    -- Show help then exit with success
            msg <- use options []
            success msg

-- From CPSA.Lib.Entry

cpsaVersion :: String
cpsaVersion = showString "CPSA " (showVersion version)

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
