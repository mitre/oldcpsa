-- Add annotations to skeletons

-- This module simply maps the function annotations to S-expressions in the
-- file.

-- Copyright (c) 2009 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module Main (main) where

import System.IO
import CPSA.Lib.SExpr
import CPSA.Lib.Algebra
import CPSA.Lib.Entry
import CPSA.Annotations.Annotations
import qualified CPSA.Basic.Algebra
import qualified CPSA.DiffieHellman.Algebra

-- Algebra names
algs :: [String]
algs = [CPSA.Basic.Algebra.name, CPSA.DiffieHellman.Algebra.name]

main :: IO ()
main =
    do
      let options = algOptions CPSA.Basic.Algebra.name
      let interp = algInterp CPSA.Basic.Algebra.name algs
      (p, (output, alg, margin)) <- start options interp
      h <- outputHandle output
      writeComment h margin cpsaVersion
      writeComment h margin "Annotated skeletons"
      case () of
        _ | alg == CPSA.Basic.Algebra.name ->
              go (step h alg CPSA.Basic.Algebra.origin margin) p []
          | alg == CPSA.DiffieHellman.Algebra.name ->
              go (step h alg CPSA.DiffieHellman.Algebra.origin margin) p []
          | otherwise ->
               abort ("Bad algebra: " ++ alg)
      hClose h

go :: (a -> SExpr Pos -> IO a) -> PosHandle -> a -> IO ()
go f p a =
    loop a
    where
      loop a =
          do
            x <- readSExpr p
            case x of
              Nothing ->
                  return ()
              Just sexpr ->
                  do
                    a <- f a sexpr
                    loop a

step :: Algebra t p g s e c => Handle ->
        String -> g -> Int -> [Prot t g] ->
        SExpr Pos -> IO [Prot t g]
step output name origin margin ps sexpr =
    do
      x <- tryIO (annotations name origin ps sexpr)
      case x of
        Right (ps, sexpr) ->
            do
              writeLnSExpr output margin sexpr
              return ps
        Left err ->
            abort (show err)
