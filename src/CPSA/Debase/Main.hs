-- Pretty print the input

-- Copyright (c) 2009 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module Main (main) where

import System.IO
import CPSA.Lib.SExpr
import CPSA.Lib.Printer (pp)
import CPSA.Lib.Entry

-- Runtime parameters

defaultIndent :: Int
defaultIndent = optIndent defaultOptions

main :: IO ()
main =
    do
      (p, (output, margin)) <- start filterOptions filterInterp
      h <- outputHandle output
      go (writeCpsaLn (pp margin defaultIndent) h) p
      hClose h

writeCpsaLn :: (SExpr () -> String) -> Handle -> SExpr a -> IO ()
writeCpsaLn printer h sexpr =
    do
      hPutStrLn h $ printer $ transform $ strip sexpr
      hPutStrLn h ""

go :: (SExpr Pos -> IO ()) -> PosHandle -> IO ()
go f p =
    loop
    where
      loop =
          do
            x <- gentlyReadSExpr p
            case x of
              Nothing ->
                  return ()
              Just sexpr ->
                  do
                    f sexpr
                    loop

strip :: SExpr a -> SExpr ()
strip (S _ s) = S () s
strip (Q _ s) = Q () s
strip (N _ n) = N () n
strip (L _ l) = L () (map strip l)

-- | Make a symbol.
sym :: String -> SExpr ()
sym s = S () s

{-

-- | Make a quoted string.
str :: String -> SExpr ()
str s = Q () s

-- | Make a number.
num :: Int -> SExpr ()
num n = N () n
-}

-- | Make a list of S-expressions.
lst :: [SExpr ()] -> SExpr ()
lst xs = L () xs

transform :: SExpr () -> SExpr ()
transform (L () (S () "defprotocol" : name : alg : rest)) =
  lst (sym "defprotocol" : name : alg : map role rest)
transform (L () (S () "defskeleton" : xs)) =
  skel xs
transform x = x

role :: SExpr () -> SExpr ()
role (L () (S () "defrole" : name : L () (S () "vars" : v) : rest)) =
  let (decl, bases) = vars v in
    lst (sym "defrole" : name : lst (sym "vars" : decl) :
         map (subst bases) rest)
role x = x

skel :: [SExpr ()] -> SExpr ()
skel (p@(S _ _) : L () (S () "vars" : v) : rest) =
  let (decl, _) = vars v in
    lst (sym "defskeleton" : p : lst (sym "vars" : decl) : rest)
skel xs = lst (sym "defskeleton" : xs)

vars :: [SExpr ()] -> ([SExpr ()], [String])
vars [] = ([], [])
vars (L () x : xs)
  | symStr (last x) == "base" =
      let (xs', ss) = vars xs in
        (lst (butLast x ++ [sym "expr"]) : xs',
         map symStr (butLast x) ++ ss)
  | otherwise =
      let (xs', ss) = vars xs in
        (lst x : xs', ss)
vars _ = error "vars: bad input"

symStr :: SExpr a -> String
symStr (S _ s) = s
symStr _ = error "symStr: not a symbol"

butLast :: [a] -> [a]
butLast [] = []
butLast [_] = []
butLast (x : xs) = x : butLast xs

subst :: [String] -> SExpr () -> SExpr ()
subst bases (S () s)
  | elem s bases = lst [sym "exp", lst [sym "gen"], sym s]
subst bases (L () (x : xs)) = L () (x : map (subst bases) xs)
subst _ x = x
