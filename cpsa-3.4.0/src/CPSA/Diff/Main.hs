--  Compares CPSA output files S-expression by S-expression, and
--  prints the first skeleton that differs.

-- Copyright (c) 2010 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module Main (main) where

import Numeric
import System.IO
import System.Environment
import System.Console.GetOpt
import System.Exit
import Paths_cpsa
import CPSA.Lib.SExpr
import CPSA.Lib.Printer
import CPSA.Lib.Entry

main :: IO ()
main =
    do
      argv <- getArgs
      (flags, files) <- opts options argv
      params <- interp flags
      case files of
        [oldName, newName] ->
            do
              oldFile <- openFile oldName ReadMode
              old <- posHandle oldName oldFile
              newFile <- openFile newName ReadMode
              new <- posHandle newName newFile
              go params old new
        _ ->
            do
              msg <- use options []
              abort msg

go :: Params -> PosHandle -> PosHandle -> IO ()
go params old new =
    do
      o <- readSkel old
      n <- readSkel new
      case (o, n) of
        (Nothing, Nothing) -> return ()
        (Just o, Just n) | o == n -> go params old new
        _ -> showDiff params o n

showDiff :: Params -> Maybe (SExpr Pos) -> Maybe (SExpr Pos) -> IO ()
showDiff params o n =
    do
      out <- outputHandle (file params)
      case o of
        Just o ->
            do
              hPutStrLn out ("<<< " ++ show (annotation o))
              writeSkel params out o
              hPutStrLn out "<<<"
        _ ->
            return ()
      case n of
        Just n ->
            do
              hPutStrLn out (">>> " ++ show (annotation n))
              writeSkel params out n
              hPutStrLn out ">>>"
        _ ->
            return ()
      hClose out
      exitFailure

writeSkel :: Params -> Handle -> SExpr a -> IO ()
writeSkel params h sexpr =
    do
      hPutStrLn h $ pp (margin params) indent sexpr

-- Read the next skeleton
readSkel :: PosHandle -> IO (Maybe (SExpr Pos))
readSkel p =
    do
      input <- gentlyReadSExpr p
      case input of
        Nothing -> return input
        Just x ->
            case x of
              L _ (S _ "defskeleton" : _) ->
                  return input
              _ ->
                  readSkel p

-- Runtime parameters

data Params = Params
    { file :: Maybe FilePath,   -- Nothing specifies standard output
      margin :: Int }           -- Output line length
    deriving Show

indent :: Int
indent = optIndent defaultOptions

-- Command line option flags
data Flag
    = Help                      -- Help
    | Info                      -- Version information
    | Margin String             -- Output line length
    | Output String             -- Output file name
      deriving Show

defaultMargin :: Int
defaultMargin = optMargin defaultOptions

options :: [OptDescr Flag]
options =
    [ Option ['o'] ["output"]   (ReqArg Output "FILE") "output FILE",
      Option ['m'] ["margin"]   (ReqArg Margin "INT")
      ("set output margin (default " ++ show defaultMargin ++ ")"),
      Option ['h'] ["help"]     (NoArg Help)           "show help message",
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
      let header = "Usage: " ++ name ++ " [OPTIONS] OLD-FILE NEW-FILE"
      let footer = "\nDocumentation directory: " ++ datadir
      return (concat errs ++ usageInfo header options ++ footer)

-- Interpret option flags
interp :: [Flag] -> IO Params
interp flags =
    loop flags (Params { file = Nothing, -- By default, no output file
                         margin = defaultMargin })
    where
      loop [] params = return params
      loop (Output name : flags) params
          | file params == Nothing =
              loop flags $ params { file = Just name }
      loop (Margin value : flags) params =
          case readDec value of
            [(margin, "")] ->
                loop flags $ params { margin = margin }
            _ ->
                do
                  msg <- use options ["Bad value for margin\n"]
                  abort msg
      loop (Info : _) _ =
          success cpsaVersion
      loop (Help : _) _ =
          do                    -- Show help then exit with success
            msg <- use options []
            success msg
      loop _ _ =
           do                   -- Show help then exit with failure
             msg <- use options ["Bad option combination\n"]
             abort msg
