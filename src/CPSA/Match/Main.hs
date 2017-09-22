-- Runs match, unify, or absent code

-- This is a test routine for the DH algebra.  It runs test of the
-- matcher, the unifier, and the absence substitution code.  The input
-- syntax is:
--
-- (MODE (VARS) TESTS)
--
-- where mode is one of match, unify, or absent, VARS are some
-- variable declarations, and TESTS is a list of tests.  A test is a
-- list of pairs of terms.  For mode match, the output is a list of
-- substitutions that cause the left hand sides of a test to match a
-- right hand side.  Unify is like match except unification is
-- performed.  Absent invokes the absenceSubst primitive in the
-- algebra interface.

-- Copyright (c) 2017 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module Main (main) where

import System.IO
import CPSA.Lib.SExpr (PosHandle, SExpr, Pos)
import CPSA.Lib.Entry
import CPSA.Match.Match

main :: IO ()
main =
    do
      (p, (output, margin)) <- start filterOptions filterInterp
      h <- outputHandle output
      writeComment h margin cpsaVersion
      writeComment h margin "Match"
      go (step h margin) p
      hClose h

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

step :: Handle -> Int -> SExpr Pos -> IO ()
step output margin sexpr =
    do
      writeLnSExpr output margin sexpr
      x <- tryIO (testMatcher sexpr)
      case x of
        Right sexpr ->
          do
            writeLnSExpr output margin sexpr
            return ()
        Left err ->
            abort (show err)
