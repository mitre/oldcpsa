-- Extract the shapes from a CPSA run

-- This module simply maps the function shapes to S-expressions in the
-- file.

-- Copyright (c) 2009 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module Main (main) where

import System.IO
import CPSA.Lib.CPSA (PosHandle, SExpr, Pos)
import CPSA.Lib.Entry
import CPSA.Shapes.Shapes

main :: IO ()
main =
    do
      (p, (output, margin)) <- start filterOptions filterInterp
      h <- outputHandle output
      writeComment h margin cpsaVersion
      writeComment h margin "Extracted shapes"
      go (step h margin) p empty
      hClose h

go :: (a -> SExpr Pos -> IO a) -> PosHandle -> a -> IO ()
go f p a =
    loop a
    where
      loop a =
          do
            x <- gentlyReadSExpr p
            case x of
              Nothing ->
                  return ()
              Just sexpr ->
                  do
                    a <- f a sexpr
                    loop a

step :: Handle -> Int -> Map -> SExpr Pos -> IO Map
step output margin acc sexpr =
    do
      x <- tryIO (shape acc sexpr)
      case x of
        Right (acc, sexpr) ->
            case sexpr of
              Nothing ->
                  return acc
              Just sexpr ->
                  do
                    writeLnSEexpr output margin sexpr
                    return acc
        Left err ->
            abort (show err)
