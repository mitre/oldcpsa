-- Convert macros into LaTeX macros

-- Copyright (c) 2020 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module Main (main) where

import Data.List
import System.Environment
import System.IO
import System.Console.GetOpt
import Control.Monad
import CPSA.Lib.SExpr
import CPSA.Lib.Expand
import CPSA.Lib.Entry hiding (defaultOptions, Options, optFile)

main :: IO ()
main =
  do
    argv <- getArgs
    (flags, files) <- opts options argv
    opts <- interp defaultOptions flags
    p <- openInput files
    sexprs <- readSExprs p
    macros <- macroExtract sexprs
    let filtered = macroFilter macros
    outputResults opts (reverse $ map translateMacro filtered)

-- Command line option flags
data Flag
    = Output String
    | Help
    | Info
      deriving Show

data Options = Options {
  optFile :: Maybe FilePath
  }
  deriving Show

defaultOptions :: Options
defaultOptions = Options {
  optFile = Nothing
  }

options :: [OptDescr Flag]
options =
  [ Option ['o'] ["output"]  (ReqArg Output "FILE") "output FILE",
    Option ['h'] ["help"]    (NoArg Help)           "show help message",
    Option ['v'] ["version"] (NoArg Info)           "show version number" ]

opts :: [OptDescr a] -> [String] -> IO ([a], [String])
opts options argv =
    case getOpt RequireOrder options argv of
      (o, n, []) -> return (o, n)
      (_, _, errs) ->
          do
            msg <- usage options errs
            abort msg

-- Interpret option flags
interp :: Options -> [Flag] -> IO Options
interp opts flags =
  loop flags opts
  where
    loop [] opts = return opts
    loop (Output name : flags) opts
      | optFile opts == Nothing =
          loop flags $ opts { optFile = Just name }
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

-- Write results to specified output location
outputResults :: Options -> [String] -> IO ()
outputResults opts rs =
  do
    h <- outputHandle (optFile opts)
    mapM_ (hPutStrLn h) rs
    hClose h
    return ()

-- The mechanisms for translating from CPSA to LaTeX are below

-- Extract the macros from a list of SExprs
macroExtract :: [SExpr Pos] -> IO [Macro]
macroExtract sexprs = do
  (mList, _) <- foldM (expandSExpr bound) ([], []) sexprs
  return mList

-- Get rid of splices
macroRemoveSplices :: [Macro] -> [Macro]
macroRemoveSplices [] = []
macroRemoveSplices (m:ms) =
  case (getMacroBody m) of
    L _ (S _ "^":_) -> macroRemoveSplices ms
    _ -> m:(macroRemoveSplices ms)

-- Get rid of defprotocols
macroRemoveDefprotocols :: [Macro] -> [Macro]
macroRemoveDefprotocols [] = []
macroRemoveDefprotocols (m:ms) =
  case (getMacroBody m) of
    L _ (S _ "defprotocol":_) -> macroRemoveDefprotocols ms
    _ -> m:(macroRemoveDefprotocols ms)

-- Get rid of defroles
macroRemoveDefroles :: [Macro] -> [Macro]
macroRemoveDefroles [] = []
macroRemoveDefroles (m:ms) =
  case (getMacroBody m) of
    L _ (S _ "defrole":_) -> macroRemoveDefroles ms
    _ -> m:(macroRemoveDefroles ms)

-- Get rid of rules
macroRemoveRules :: [Macro] -> [Macro]
macroRemoveRules [] = []
macroRemoveRules (m:ms) =
  case (getMacroBody m) of
    L _ (S _ "defrule":_) -> macroRemoveRules ms
    _ -> m:(macroRemoveRules ms)

-- Get rid of goals
macroRemoveGoals ::  [Macro] -> [Macro]
macroRemoveGoals [] = []
macroRemoveGoals (m:ms) =
  case (getMacroBody m) of
    L _ (S _ "defgoal":_) -> macroRemoveGoals ms
    _ -> m:(macroRemoveGoals ms)

-- Composition of the preceding three functions
macroFilter :: [Macro] -> [Macro]
macroFilter = macroRemoveDefroles .
              macroRemoveDefprotocols .
              macroRemoveGoals .
              macroRemoveRules .
              macroRemoveSplices

-- Given a macro, return the count of its args
macroArgsCount :: Macro -> Int
macroArgsCount = length . getMacroArgs

-- Translate an SExpr into a String and a final translation
-- environment given some initial environment
translateSExpr :: [(String, String)] -> SExpr a -> (String, [(String, String)])
translateSExpr e (S _ s) =
  case (lookup s e) of
    Nothing -> let t = (translateSymbol s) in (t, (s,t):e)
    Just t -> (t, e)
translateSExpr e (Q _ s) = let t = translateQuotedStr s in (t, (s,t):e)
translateSExpr e (N _ n) = (translateInt n, e)
translateSExpr e (L _ []) = ("", e)
translateSExpr e (L _ (S _ "cat":ys)) =
  (intercalate "\\cat " xs, e1)
  where
    (xs, e1) = translateSExprFold e ys
translateSExpr e (L p (S q "enc":ys)) =
  ("\\enc{" ++ s1 ++ "}{" ++ s2 ++ "}", e2)
  where
    (s2, e1) = translateSExpr e (last ys)
    (s1, e2) = translateSExpr e1 (L p (S q "cat":init ys))
translateSExpr e (L p (S q "hash":ys)) =
  ("\\hash{" ++ s ++ "}", e')
  where
    (s, e') = translateSExpr e (L p (S q "cat":ys))
translateSExpr e (L _ [S _ "pubk", y1, y2]) =
  case translateSExprFold e [y1, y2] of
    ([s1, s2], e1) ->
      ("\\pubktwo{" ++ s1 ++ "}{" ++ s2 ++ "}", e1)
    _ ->
      error "translateSExpr: bad fold for pubktwo"
translateSExpr e (L _ [S _ "privk", y1, y2]) =
  case translateSExprFold e [y1, y2] of
    ([s1, s2], e1) ->
      ("\\privktwo{" ++ s1 ++ "}{" ++ s2 ++ "}", e1)
    _ ->
      error "translateSExpr: bad fold for privktwo"
translateSExpr e (L _ ys) =
  (concat xs, e1)
  where
    (xs, e1) = translateSExprFold e ys

translateSExprFold :: [(String, String)] -> [SExpr a] ->
                      ([String], [(String, String)])
translateSExprFold e ys = (xs, e')
  where
    (xs, e') = foldr f ([], e) ys
    f y (acc, env) = (x:acc, e'')
      where (x, e'') = translateSExpr env y

-- Translate a CPSA macro into an equivalent LaTeX macro
translateMacro :: Macro -> String
translateMacro m =
  let name = toMathMode $ getMacroName m
      args = getMacroArgs m
      argc = macroArgsCount m
      body = getMacroBody m
      env = zip args ["{#" ++ (show i) ++ "}" | i <- [(1::Int)..]]
      prelude = "\\newcommand{\\"
      l = "}["
      r = "]{"
  in prelude++name++l++(show argc)++r++(fst $ translateSExpr env body)++"}"

-- Symbol to String translation rules
translateSymbol :: String -> String
translateSymbol s = "\\" ++ (toMathMode s)

-- Quoted string to String translation rules
translateQuotedStr :: String -> String
translateQuotedStr s = "\\tag{" ++ s' ++ "}"
  where s' = toMathModeSpaced s

-- Delete chars LaTeX math mode doesn't like
toMathMode :: String -> String
toMathMode s = [c | c<-s, not (c `elem` badChars)]
  where badChars = " -"

-- Int to String translation rules
translateInt :: Int -> String
translateInt n = show n

-- Replace chars LaTeX math mode doesn't like with "\\ "
toMathModeSpaced :: String -> String
toMathModeSpaced s = intercalate "\\ " (f s)
  where badChars = " -"
        f :: String -> [String]
        f s = case dropWhile (`elem` badChars) s of
          "" -> []
          s' -> w : f s''
            where (w, s'') = break (`elem` badChars) s'
