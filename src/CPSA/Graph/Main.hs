-- Generate views of a sequence of preskeletons

-- Copyright (c) 2009 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module Main (main) where

import Numeric
import System.IO
import System.Console.GetOpt
import CPSA.Lib.SExpr (PosHandle, SExpr, Pos)
import CPSA.Lib.Entry
import CPSA.Graph.Config
import CPSA.Graph.Loader
import CPSA.Graph.CompactView
import CPSA.Graph.ExpandedView
import CPSA.Graph.LaTeXView

-- Runtime parameters

data Params = Params
    { file :: Maybe FilePath,   -- Nothing specifies standard output
      format :: Format,         -- Output format
      prefix :: Bool,           -- Use prefix notation?
      purgeTraces :: Bool,      -- Purge traces?
      scripted :: Bool,         -- Use scripting?
      comp :: Bool,          -- Use compact images?
      margin :: Int }           -- Output line length
    deriving Show

data Format = XHTML | TreelessXHTML | SVG | LaTeX deriving Show

main :: IO ()
main =
    do
      (p, params) <- start options interp
      case format params of
        TreelessXHTML -> treeless p params
        LaTeX -> latex p params
        _ -> loadAll p params

-- Load all preskeletons before generating any output.
-- Don't use these graphing methods if your input is large.
loadAll :: PosHandle -> Params -> IO ()
loadAll p params =
    do
      preskels <- tryIO (loadDefs p)
      case preskels of
        Left err -> abort (show err)
        Right (cmts, preskels) ->
            do
              h <- outputHandle (file params)
              hPutStrLn h "<?xml version=\"1.0\"?>"
              hPutStrLn h ("<!-- " ++ cpsaVersion ++ " -->")
              let conf = config (prefix params) (purgeTraces params)
                         (scripted params) (comp params)
              case format params of
                XHTML -> expandedView h conf
                         (margin params) cmts preskels
                SVG -> compactView h conf preskels
                _ -> error "Bad case in main"

-- Load comments and preskeletons
loadDefs :: PosHandle -> IO ([SExpr Pos], [Preskel])
loadDefs h =
    do
      (cmts, k, s) <- loadFirst h
      ks <- loop [k] s
      return (cmts, ks)
    where
      loop ks s =
          do
            n <- loadNext s
            case n of
              Nothing ->        -- EOF
                  return $ reverse ks
              Just (k, s) ->
                  loop (k:ks) s

-- XHTML graphing for very large files.
-- Treeless loads one S-expression at a time, processes it, prints the
-- results, and makes the S-expression available for garbage
-- collection before reading the next S-expression.
treeless :: PosHandle -> Params -> IO ()
treeless p params =
    do
      preskel <- tryIO (loadFirst p)
      case preskel of
        Left err -> abort (show err)
        Right (cmts, preskel, state) ->
            do
              h <- outputHandle (file params)
              hPutStrLn h "<?xml version=\"1.0\"?>"
              hPutStrLn h ("<!-- " ++ cpsaVersion ++ " -->")
              let conf = config (prefix params) (purgeTraces params)
                         (scripted params) False
              ans <- tryIO (treelessView h conf (margin params)
                                         cmts preskel state)
              case ans of
                Left err -> abort (show err)
                Right () -> return ()

-- LaTeX graphing.
latex :: PosHandle -> Params -> IO ()
latex p params =
    do
      preskel <- tryIO (loadFirst p)
      case preskel of
        Left err -> abort (show err)
        Right (cmts, preskel, state) ->
            do
              h <- outputHandle (file params)
              hPutStrLn h "\\documentclass[12pt]{article}"
              hPutStrLn h ("% " ++ cpsaVersion)
              let conf = config (prefix params) (purgeTraces params)
                         (scripted params) False
              let pp = printer conf
              ans <- tryIO (latexView h (margin params) pp cmts preskel state)
              case ans of
                Left err -> abort (show err)
                Right () -> return ()

-- Command line option flags
data Flag
    = Help                      -- Help
    | Info                      -- Version information
    | Expanded                  -- Select expanded format in XHTML
    | Treeless                  -- Select treeless expanded format in XHTML
    | Scripted                  -- Enable scripting
    | Compact                   -- Select compact format in SVG
    | Text                      -- Select text format in LaTeX
    | Margin String             -- Output line length
    | InfixFlag                 -- Select output notation
    | PurgeFlag                 -- Enable purging of traces
    | Output String             -- Output file name
      deriving Show

defaultMargin :: Int
defaultMargin = optMargin defaultOptions

options :: [OptDescr Flag]
options =
    [ Option ['o'] ["output"]   (ReqArg Output "FILE") "output FILE",
      Option ['x'] ["expanded"] (NoArg Expanded)
      "use expanded format (default)",
      Option ['z'] ["zoom"]     (NoArg Scripted)
      "enable diagram scaling",
      Option ['t'] ["treeless"] (NoArg Treeless)
      "use treeless expanded format",
      Option ['c'] ["compact"]  (NoArg Compact)        "use compact format",
      Option ['l'] ["latex"]    (NoArg Text)           "use LaTeX format",
      Option ['m'] ["margin"]   (ReqArg Margin "INT")
      ("set output margin (default " ++ show defaultMargin ++ ")"),
      Option ['i'] ["infix"]    (NoArg InfixFlag) "output uses infix notation",
      Option ['s'] ["show-traces"] (NoArg PurgeFlag)   "show traces",
      Option ['h'] ["help"]     (NoArg Help)           "show help message",
      Option ['v'] ["version"]  (NoArg Info)           "show version number" ]

-- Interpret option flags
interp :: [Flag] -> IO Params
interp flags =
    loop flags (Params { file = Nothing, -- By default, no output file
                         format = XHTML, -- and use expanded format
                         prefix = True,
                         purgeTraces = True,
                         scripted = False,
                         comp = False,
                         margin = defaultMargin })
    where
      loop [] params = return params
      loop (Output name : flags) params
          | file params == Nothing =
              loop flags $ params { file = Just name }
      loop (Expanded : flags) params =
          loop flags $ params { format = XHTML }
      loop (Treeless : flags) params =
          loop flags $ params { format = TreelessXHTML }
      loop (Scripted : flags) params =
          loop flags $ params { scripted = True }
      loop (Compact : flags) params =
          loop flags $ params { comp = True }
      loop (Text : flags) params =
          loop flags $ params { format = LaTeX }
      loop (InfixFlag : flags) params =
          loop flags $ params { prefix = False }
      loop (PurgeFlag : flags) params =
          loop flags $ params { purgeTraces = False }
      loop (Margin value : flags) params =
          case readDec value of
            [(margin, "")] ->
                loop flags $ params { margin = margin }
            _ ->
                do
                  msg <- usage options ["Bad value for margin\n"]
                  abort msg
      loop (Info : _) _ =
          success cpsaVersion
      loop (Help : _) _ =
          do                    -- Show help then exit with success
            msg <- usage options []
            success msg
      loop _ _ =
           do                   -- Show help then exit with failure
             msg <- usage options ["Bad option combination\n"]
             abort msg

-- Default configuration.  The lengths are in points, however the more
-- natural choice is a font relative unit of length such as ems,
-- however FireFox doesn't support these units yet.
config :: Bool -> Bool -> Bool -> Bool -> Config
config prefix purge scripts compact =
    Config { units = "pt",
             font = font,
             stroke = 0.08 * font,
             dash = 0.50 * font,
             gap = 0.20 * font,
             tx = 4.16 * font * factor,
             ty = 6.25 * font * (if compact then 0.4 else 1.0),
             ta = 1.75 * font * factor,
             td = 1.16 * font * (if compact then 0.4 else 1.0),
             dx = 8.33 * font * factor,
             dy = 6.25 * font * factor,
             mx = 3.33 * font,
             my = 3.33 * font,
             br = 0.50 * font,
             compact = False,
             notation = if prefix then Prefix else Infix,
             purge = purge,
             scripts = scripts }
    where
      font = 12
      factor = if compact then 0.7 else 1.0
