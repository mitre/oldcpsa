-- Summarize CPSA output as a formula in coherent logic

-- Copyright (c) 2011 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module Main (main) where

import System.IO
import CPSA.Lib.SExpr
import CPSA.Lib.Algebra
import CPSA.Lib.Entry
import CPSA.SAS.SAS
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
      -- Handle the herald
      x <- readSExpr p
      case x of
        Nothing -> abort "Empty input"
        Just (L _ (S _ "herald" : _ : xs)) ->
             selAlg p h (getName xs alg) margin Nothing
        Just x -> selAlg p h alg margin (Just x)

-- Select algebra and then continue with next
selAlg :: PosHandle -> Handle -> String -> Int -> Maybe (SExpr Pos) -> IO ()
selAlg p h alg margin sexpr =
    case () of
      _ | alg == CPSA.Basic.Algebra.name ->
           next p h margin sexpr
                    (step h alg CPSA.Basic.Algebra.origin margin)
        | alg == CPSA.DiffieHellman.Algebra.name ->
           next p h margin sexpr
                    (step h alg CPSA.DiffieHellman.Algebra.origin margin)
          | otherwise ->
               abort ("Bad algebra: " ++ alg)

-- Continue and ignore header if one was found
next :: PosHandle -> Handle -> Int ->
        Maybe (SExpr Pos) -> (State t g c -> Maybe (SExpr Pos)
                              -> IO (State t g c)) -> IO ()
next p h margin Nothing f = -- Found header
    do
      writeComment h margin cpsaVersion
      writeComment h margin "Coherent logic"
      go f p ([], [])
      hClose h
next p h margin sexpr f =   -- No header found
    do
      writeComment h margin cpsaVersion
      writeComment h margin "Coherent logic"
      st <- f ([], []) sexpr -- Process first S-expression.
      go f p st
      hClose h

go :: (a -> Maybe (SExpr Pos) -> IO a) -> PosHandle -> a -> IO ()
go f p a =
    loop a
    where
      loop a =
          do
            x <- readSExpr p
            case x of
              Nothing ->
                  do
                    _ <- f a x
                    return ()
              Just _ ->
                  do
                    a <- f a x
                    loop a

step :: Algebra t p g s e c => Handle -> String ->
        g -> Int -> State t g c ->
        Maybe (SExpr Pos) -> IO (State t g c)
step output _ _ margin state
         (Just sexpr@(L _ (S _ "comment" : _))) =
         do
           writeLnSExpr output margin sexpr
           return state
step output name origin margin state sexpr =
    do
      x <- tryIO (sas name origin state sexpr)
      case x of
        Left err ->
            abort err
        Right (acc, Nothing) ->
            after output margin acc sexpr
        Right (acc, Just x) ->
            do
              writeLnSExpr output margin x
              after output margin acc sexpr

after :: Handle -> Int -> State t g c ->
         Maybe (SExpr Pos) -> IO (State t g c)
after output margin state (Just sexpr@(L _ (S _ "defprotocol" : _))) =
    do
      writeLnSExpr output margin sexpr
      return state
after _ _ state _ =
    return state

getName :: [SExpr a] -> String -> String
getName xs name =
    case assoc "algebra" xs of
      [S _ nom] -> nom
      _ -> name

-- Lookup value in alist, appending values with the same key
assoc :: String -> [SExpr a] -> [SExpr a]
assoc key alist =
    concat [ rest | L _ (S _ head : rest) <- alist, key == head ]
