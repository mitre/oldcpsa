-- Copyright (c) 2017 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module CPSA.Match.Match (testMatcher) where

import CPSA.Lib.SExpr
import CPSA.Lib.Algebra
import qualified CPSA.DiffieHellman.Algebra

testMatcher :: SExpr Pos -> IO (SExpr ())
testMatcher x = go CPSA.DiffieHellman.Algebra.origin x

go :: Algebra t p g s e c => g -> SExpr Pos -> IO (SExpr ())
go g (L _ (S _ "match" : L _ vs : xs)) =
  do
    (g, vs) <- loadVars g vs
    a <- mapM (loadMatch vs) xs
    return $ L () $ map (showEnv vs) (matchTerms a (g, emptyEnv))
go g (L _ (S _ "unify" : L _ vs : xs)) =
  do
    (g, vs) <- loadVars g vs
    a <- mapM (loadMatch vs) xs
    return $ L () $ map (showSubst vs) (unifyTerms a (g, emptySubst))
go g (L _ (S _ "absent" : L _ vs : xs)) =
  do
    (g, vs) <- loadVars g vs
    a <- mapM (loadMatch vs) xs
    return $ L () $ map (showSubst vs) (absenceSubst g a)
go _ x = fail (shows (annotation x) "Malformed input!")

loadMatch :: (Algebra t p g s e c, Monad m) =>
              [t] -> SExpr Pos -> m (t, t)
loadMatch vs (L _ [r, n]) =
  do
    r <- loadTerm vs False r
    n <- loadTerm vs False n
    return (r, n)
loadMatch _ x = fail (shows (annotation x) "Malformed match")

matchTerms :: Algebra t p g s e c => [(t, t)] -> (g, e) -> [(g, e)]
matchTerms [] e = [e]
matchTerms ((t0, t1) : ts) e =
  do
    e <- match t0 t1 e
    matchTerms ts e

showEnv :: Algebra t p g s e c => [t] -> (g, e) -> SExpr ()
showEnv vs (_, e) =
  L () $ displaySubst c (substitution e)
  where
    c = addToContext emptyContext vs

unifyTerms :: Algebra t p g s e c => [(t, t)] -> (g, s) -> [(g, s)]
unifyTerms [] e = [e]
unifyTerms ((t0, t1) : ts) e =
  do
    e <- unify t0 t1 e
    unifyTerms ts e

showSubst :: Algebra t p g s e c => [t] -> (g, s) -> SExpr ()
showSubst vs (_, s) =
  L () $ displaySubst c s
  where
    c = addToContext emptyContext vs
