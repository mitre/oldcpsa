-- Main routine for the CPSA solver.  Provides command line processing

-- Copyright (c) 2009 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module Main (main) where

import Numeric
import Control.Monad (forM_)
import System.IO
import System.Environment
import System.Console.GetOpt
import CPSA.Lib.SExpr
import CPSA.Lib.Entry
import CPSA.Lib.Algebra
import CPSA.Lib.Loader
import CPSA.Lib.Expand
import CPSA.Lib.Reduction
import qualified CPSA.Basic.Algebra
import qualified CPSA.DiffieHellman.Algebra

-- Default limit on the number of steps used to solve one problem.
defaultStepLimit :: Int
defaultStepLimit = optLimit defaultOptions

-- Default limit on the number of strands is a skeleton.
defaultStrandBound :: Int
defaultStrandBound = optBound defaultOptions

-- Default limit on the number of depths is a skeleton.
defaultDepthBound :: Int
defaultDepthBound = optDepth defaultOptions

-- Default algebra
defaultAlgebra :: String
defaultAlgebra = optAlg defaultOptions

-- Entry point
main :: IO ()
main =
    do
      argv <- getArgs
      (flags, files) <- opts options argv
      -- Handle help and version options before input is read
      _ <- interp algs defaultOptions flags
      p <- openInput files
      sexprs <- readSExprs p
      let herald = getHerald sexprs
      alist <- case herald of
                 Nothing -> return []
                 Just sexpr -> getAlist sexpr
      heraldFlags <- getAlistOpts alist
      forM_ heraldFlags checkHeraldFlag
      opts <- interp algs defaultOptions heraldFlags
      opts <- interp algs opts flags
      sexprs <- tryIO (expand sexprs) -- Expand macros
      case sexprs of
        Left err -> abort err
        Right sexprs ->
            if optAnalyze opts then
                select files herald opts sexprs
            else
                prettyPrint opts sexprs

opts :: [OptDescr a] -> [String] -> IO ([a], [String])
opts options argv =
    case getOpt RequireOrder options argv of
      (o, n, []) -> return (o, n)
      (_, _, errs) ->
          do
            msg <- usage options errs
            abort msg

openInput :: [String] -> IO PosHandle
openInput [file] =
    do                          -- Input from named file
      input <- openFile file ReadMode
      posHandle file input
openInput [] =
    posHandle "" stdin          -- Input from the standard input
openInput _ =
    do
      msg <- usage options ["too many input files\n"]
      abort msg

-- Algebra specific section

-- Algebra names
algs :: [String]
algs = [CPSA.Basic.Algebra.name, CPSA.DiffieHellman.Algebra.name]

-- Select the algebra and go.
select :: [String] -> Maybe (SExpr Pos) -> Options -> [SExpr Pos] -> IO ()
select files herald opts sexprs =
    case optAlg opts of
      name | name == CPSA.Basic.Algebra.name ->
               go name CPSA.Basic.Algebra.origin
                  files herald opts sexprs
           | name == CPSA.DiffieHellman.Algebra.name ->
               go name CPSA.DiffieHellman.Algebra.origin
                  files herald opts sexprs
           | otherwise ->
               abort ("Bad algebra: " ++ name)

{--
-- name the algebra and go.
select :: [String] -> Maybe (SExpr Pos) -> Options -> [SExpr Pos] -> IO ()
select files herald opts sexprs =
    case optAlg opts of
      name | name == CPSA.Algebra.Basic.name ->
               go name (CPSA.Algebra.Algebra.origin CPSA.Algebra.Basic.origin)
                  files herald opts sexprs
           | name == CPSA.Algebra.DiffieHellman.name ->
               go name (CPSA.Algebra.Algebra.origin CPSA.Algebra.DiffieHellman.origin)
                  files herald opts sexprs
           | otherwise ->
               abort ("Bad algebra: " ++ name)
--}

-- Load protocols and preskeletons and print run time information
go :: Algebra t p g s e c => String -> g -> [String] ->
      Maybe (SExpr Pos) -> Options -> [SExpr Pos] -> IO ()
go name origin files herald opts sexprs =
    do
      preskels <- tryIO (loadSExprs name origin sexprs)
      case preskels of          -- Load protocols and preskeletons
        Left err -> abort err
        Right preskels ->
            do
              h <- outputHandle (optFile opts)
              let m = optMargin opts
              -- Print herald
              case herald of
                Nothing -> return ()
                Just sexpr ->
                    do
                      writeSExpr h m sexpr
                      hPutStrLn h ""
              -- Print run time information
              writeComment h m cpsaVersion
              case files of
                [file] -> writeComment h m $ "All input read from " ++ file
                _ -> writeComment h m "All input read"
              case optNoIsoChk opts of
                True -> writeComment h m "Isomorphism checking disabled"
                False -> return ()
              case optLimit opts /= defaultStepLimit of
                True -> writeComment h m $
                        "Step count limited to " ++ show (optLimit opts)
                False -> return ()
              case optBound opts /= defaultStrandBound of
                True -> writeComment h m $
                        "Strand count bounded at " ++ show (optBound opts)
                False -> return ()
              case optDepth opts /= defaultDepthBound of
                True -> writeComment h m $
                        "Tree depth bounded at " ++ show (optDepth opts)
                False -> return ()
              case optCheckNoncesFirst opts of
                True -> writeComment h m "Nonces checked first"
                False -> return ()
              case optTryOldStrandsFirst opts of
                True -> writeComment h m "Old strands tried first"
                False -> return ()
              case optTryYoungNodesFirst opts of
                True -> writeComment h m "Younger nodes tried first"
                False -> return ()
              case optGoalsSat opts of
                True -> writeComment h m "Stop when goals satisfied"
                False -> return ()
              -- Analyze
              solve opts h preskels 0

-- Just pretty the expanded macros
prettyPrint :: Options -> [SExpr a] -> IO ()
prettyPrint opts sexprs =
    do
      let m = optMargin opts
      h <- outputHandle (optFile opts)
      writeComment h m cpsaVersion
      writeComment h m "Expanded macros"
      mapM_ (writeLnSExpr h m) sexprs
      hClose h
      return ()

-- Command line option flags
data Flag
    = Output String             -- Output file name
    | Limit String              -- Step count limit
    | Bound String              -- Strand count bound
    | Depth String              -- Tree depth bound
    | Margin String             -- Output line length
    | Expand                    -- Expand macros only
    | NoAnalyze                 -- Don't analyze, just echo outputs
    | NoIsoChk                  -- Disable isomorphism checks
    | CheckNoncesFirst          -- Check nonces first
    | TryOldStrandsFirst        -- Try old strands first
    | TryYoungNodesFirst        -- Try young nodes first
    | GoalsSat                  -- Stop when goals are satisfied
    | Algebra String            -- Algebra
    | Algebras                  -- Show algebras
    | Help                      -- Help
    | Info                      -- Version information
      deriving Show

options :: [OptDescr Flag]
options =
    [ Option ['o'] ["output"]   (ReqArg Output "FILE")  "output FILE",
      Option ['l'] ["limit"]    (ReqArg Limit "INT")
      ("step count limit (default " ++ show defaultStepLimit ++ ")"),
      Option ['b'] ["bound"]    (ReqArg Bound "INT")
      ("strand count bound (default " ++ show defaultStrandBound ++ ")"),
      Option ['d'] ["depth"]    (ReqArg Depth "INT")
      ("tree depth bound (default unbounded)"),
      Option ['m'] ["margin"]   (ReqArg Margin "INT")
      ("set output margin (default " ++ show (optMargin defaultOptions) ++ ")"),
      Option ['e'] ["expand"]   (NoArg Expand)
      "expand macros only; don't load or analyze",
      Option ['z'] ["noanalyze"] (NoArg NoAnalyze) "load but don't analyze",
      Option ['n'] ["noisochk"] (NoArg NoIsoChk)
      "disable isomorphism checks",
      Option ['c'] ["check-nonces"] (NoArg CheckNoncesFirst)
      "check nonces first",
      Option ['t'] ["try-old-strands"] (NoArg TryOldStrandsFirst)
      "try old strands first",
      Option ['r'] ["reverse-nodes"] (NoArg TryYoungNodesFirst)
      "try younger nodes first",
      Option ['g'] ["goals-sat"] (NoArg GoalsSat)
      "Stop when goals are satisfied",
      Option ['a'] ["algebra"]  (ReqArg Algebra "STRING")
      ("algebra (default " ++ defaultAlgebra ++ ")"),
      Option ['s'] ["show-algebras"] (NoArg Algebras)  "show algebras",
      Option ['h'] ["help"]     (NoArg Help)      "show help message",
      Option ['v'] ["version"]  (NoArg Info)      "show version number" ]

-- Interpret option flags
interp :: [String] -> Options -> [Flag] -> IO Options
interp algs opts flags =
    loop flags opts
    where
      loop [] opts = return opts
      loop (Output name : flags) opts
          | optFile opts == Nothing =
              loop flags $ opts { optFile = Just name }
      loop (Limit value : flags) opts =
          case readDec value of
            [(limit, "")] ->
                loop flags $ opts { optLimit = limit }
            _ ->
                do
                  msg <- usage options ["Bad value for step limit\n"]
                  abort msg
      loop (Bound value : flags) opts =
          case readDec value of
            [(bound, "")] ->
                loop flags $ opts { optBound = bound }
            _ ->
                do
                  msg <- usage options ["Bad value for strand bound\n"]
                  abort msg
      loop (Depth value : flags) opts =
          case readDec value of
            [(depth, "")] ->
                loop flags $ opts { optDepth = depth }
            _ ->
                do
                  msg <- usage options ["Bad value for depth bound\n"]
                  abort msg
      loop (Margin value : flags) opts =
          case readDec value of
            [(margin, "")] ->
                loop flags $ opts { optMargin = margin }
            _ ->
                do
                  msg <- usage options ["Bad value for margin\n"]
                  abort msg
      loop (Expand : flags) opts =
          loop flags $ opts { optAnalyze = False }
      loop (NoAnalyze : flags) opts =
          loop flags $ opts { optDoAnalyze = False }
      loop (NoIsoChk : flags) opts =
          loop flags $ opts { optNoIsoChk = True }
      loop (CheckNoncesFirst : flags) opts =
          loop flags $ opts { optCheckNoncesFirst = True }
      loop (TryOldStrandsFirst : flags) opts =
          loop flags $ opts { optTryOldStrandsFirst = True }
      loop (TryYoungNodesFirst : flags) opts =
          loop flags $ opts { optTryYoungNodesFirst = True }
      loop (GoalsSat : flags) opts =
          loop flags $ opts { optGoalsSat = True }
      loop (Algebra name : flags) opts
          | elem name algs = loop flags $ opts { optAlg = name }
          | otherwise =
              abort ("Algebra " ++ name ++ " not one of\n" ++ unlines algs)
      loop (Algebras : _) _ =
          success $ unlines algs
      loop (Help : _) _ =
          do                    -- Show help then exit with success
            msg <- usage options []
            success msg
      loop (Info : _) _ =
          success cpsaVersion
      loop _ _ =
           do                   -- Show help then exit with failure
             msg <- usage options ["Bad option combination\n"]
             abort msg

-- Herald form

-- Get a herald form.  It must be the first form while skipping comments.
getHerald :: [SExpr Pos] -> Maybe (SExpr Pos)
getHerald (x@(L _ (S _ "herald" : _)) : _) = Just x
getHerald ((L _ (S _ "comment" : _)) : sexprs) = getHerald sexprs
getHerald _ = Nothing

-- Get the association list of a herald form
getAlist :: SExpr Pos -> IO [SExpr Pos]
getAlist (L _ (S _ "herald" : S _ _ : alist)) = return alist
getAlist (L _ (S _ "herald" : Q _ _ : alist)) = return alist
getAlist _ =
    do
      msg <- usage options ["Bad herald form\n"]
      abort msg

getAlistOpts :: [SExpr Pos] -> IO [Flag]
getAlistOpts alist =
    loop alist []
    where
      loop [] flags =
          return $ reverse flags
      loop ((L _ (S _ key : vals)) : alist) flags =
          do
            flag <- flagOf key vals
            case flag of
              Nothing -> loop alist flags -- Ignore unrecognized keys
              Just flag -> loop alist (flag : flags)
      loop _ _ =
          do
             msg <- usage options ["Bad herald form\n"]
             abort msg

flagOf :: String -> [SExpr Pos] -> IO (Maybe Flag)
flagOf key vals =
    do
      let opt = findOpt key
      case opt of
        Nothing -> return Nothing
        Just opt ->
            do
              flag <- interpAssoc key opt vals
              return $ Just flag

findOpt :: String -> Maybe (OptDescr Flag)
findOpt key =
    loop options
    where
      loop [] = Nothing
      loop (opt@(Option _ keys _ _) : options)
           | elem key keys = Just opt
           | otherwise = loop options

interpAssoc :: String -> OptDescr Flag -> [SExpr Pos] -> IO Flag
interpAssoc _ (Option _ _ (NoArg flag) _) [] =
    return flag
interpAssoc _ (Option _ _ (ReqArg flag _) _) [S _ val] =
    return $ flag val
interpAssoc _ (Option _ _ (ReqArg flag _) _) [Q _ val] =
    return $ flag val
interpAssoc _ (Option _ _ (ReqArg flag _) _) [N _ val] =
    return $ flag $ show val
interpAssoc _ (Option _ _ (OptArg flag _) _) [] =
    return $ flag Nothing
interpAssoc _ (Option _ _ (OptArg flag _) _) [S _ val] =
    return $ flag $ Just val
interpAssoc _ (Option _ _ (OptArg flag _) _) [Q _ val] =
    return $ flag $ Just val
interpAssoc _ (Option _ _ (OptArg flag _) _) [N _ val] =
    return $ flag $ Just $ show val
interpAssoc key _ _ =
    do
      msg <- usage options ["Bad herald form at " ++ key ++ "\n"]
      abort msg

checkHeraldFlag :: Flag -> IO ()
checkHeraldFlag (Output _) =
    abort "output option not allowed in herald"
checkHeraldFlag Help =
    abort "help option not allowed in herald"
checkHeraldFlag Info =
    abort "version option not allowed in herald"
checkHeraldFlag Algebras =
    abort "show algebras help option not allowed in herald"
checkHeraldFlag _ =
    return ()
