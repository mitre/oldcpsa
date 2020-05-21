-- Identify unsatisfied skeletons

-- Copyright (c) 2009 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module Main (main) where

import Numeric
import System.IO
import System.Console.GetOpt
import CPSA.Lib.SExpr
import CPSA.Lib.Printer (pp)
import CPSA.Lib.Entry

-- Runtime parameters

data Params = Params
    { file :: Maybe FilePath,   -- Nothing specifies standard output
      margin :: Int }           -- Output line length
    deriving Show

defaultIndent :: Int
defaultIndent = optIndent defaultOptions

defaultMargin :: Int
defaultMargin = optMargin defaultOptions

main :: IO ()
main =
    do
      (p, params) <- start options interp
      h <- outputHandle $ file params
      go (writeCpsaLn (pp (margin params) defaultIndent) h) p
      hClose h

writeCpsaLn :: (SExpr a -> String) -> Handle -> SExpr a -> IO ()
writeCpsaLn printer h sexpr =
    hPutStrLn h $ printer sexpr

-- Command line option flags
data Flag
    = Help                      -- Help
    | Info                      -- Version information
    | Margin String             -- Output line length
    | Output String             -- Output file name
      deriving Show

options :: [OptDescr Flag]
options =
    [ Option ['o'] ["output"]   (ReqArg Output "FILE") "output FILE",
      Option ['m'] ["margin"]   (ReqArg Margin "INT")
      ("set output margin (default " ++ show defaultMargin ++ ")"),
      Option ['h'] ["help"]     (NoArg Help)           "show help message",
      Option ['v'] ["version"]  (NoArg Info)           "show version number" ]

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

-- Entry point for finding unsatisfied skeletons
go :: (SExpr () -> IO ()) -> PosHandle -> IO ()
go pp p =
  pov pp p

-- Look for a point of view that contains a goal.
pov :: (SExpr () -> IO ()) -> PosHandle -> IO ()
pov pp p =
  do
    x <- gentlyReadSExpr p
    case x of
      Nothing ->
        return ()
      Just sexpr ->
        case sexpr of
          L _ (S _ "defskeleton" : _ : xs) ->
            case label xs of    -- Get laber
              Nothing ->
                abort "Skeleton without a label"
              Just lab ->       -- See if it has goals
                if hasKey "goals" xs then
                  skel pp p [lab] -- Has a goal
                else
                  prot pp p     -- Else skip this problem
          _ ->
            pov pp p

-- Get the label number of a skeleton
label :: [SExpr a] -> Maybe Int
label xs =
  case assoc "label" xs of
    [N _ lab] ->
      Just lab
    _ ->
      Nothing

-- Search for next protocol definition
prot :: (SExpr () -> IO ()) -> PosHandle -> IO ()
prot pp p =
  do
    x <- gentlyReadSExpr p
    case x of
      Nothing ->
        return ()
      Just sexpr ->
        case sexpr of
          L _ (S _ "defprotocol" : _ ) ->
            pov pp p
          _ ->
            prot pp p

-- Look for skeleton that are unsatisfied.
skel :: (SExpr () -> IO ()) -> PosHandle -> [Int] -> IO ()
skel pp p sats =
  do
    x <- gentlyReadSExpr p
    case x of
      Nothing ->
        return ()
      Just sexpr ->
        case sexpr of
          L _ (S _ "defskeleton" : _ : xs) ->
            if unsat xs then    -- If skeleton is unsatisfied
              case label xs of
                Nothing ->
                  abort "Skeleton without a label"
                Just lab ->     -- Then add its label to the list
                  skel pp p (lab : sats)
            else                -- Skeleton okay
              skel pp p sats
          _ ->                  -- Not a skeleton
            do                  -- Print results
              pp (L () (map (N ()) (reverse sats)))
              pov pp p          -- And then search for POV

-- Is skeleton unsatisfied?
unsat :: [SExpr a] -> Bool
unsat xs =                      -- Satisfies entry has at least
  hasKey "no" $ assoc "satisfies" xs -- one no

-- Lookup value in alist, appending values with the same key
assoc :: String -> [SExpr a] -> [SExpr a]
assoc key alist =
    concat [ rest | L _ (S _ head : rest) <- alist, key == head ]

-- See if alist has a key
hasKey :: String -> [SExpr a] -> Bool
hasKey key alist =
    any f alist
    where
      f (L _ (S _ head : _)) = head == key
      f _ = False
