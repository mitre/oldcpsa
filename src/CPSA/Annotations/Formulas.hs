-- Formulas used as annotations

-- Copyright (c) 2009 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module CPSA.Annotations.Formulas (Formula, truth, true, says, implies,
    freeVars, finstantiate, mapAccumLM, loadDecls, loadFormula,
    displayFormula) where

import Control.Monad
import qualified Data.Set as S
import CPSA.Lib.Utilities
import CPSA.Lib.SExpr
import CPSA.Lib.Algebra

-- A CPSA formula is first-order logic augmented with the says modal
-- operator.  True is And [], and false is Or [].

-- The formulas are constructed with the property that that every
-- bound variable is quantified once, and no free variable is also
-- quantified within a subformula.  During the construction, each
-- variable bound by a forall or exists quantifier is freshly
-- generated.

data Formula t
    = Predicate String [FTerm t]
    | Not (Formula t)
    | And [Formula t]
    | Or [Formula t]
    | Implies [Formula t] -- Null list not allowed
    | Says t (Formula t)
    | Forall [t] (Formula t)
    | Exists [t] (Formula t)
      deriving (Eq, Show)

-- A formula term is an algebra term or an application of a function
-- symbol to list of formula terms.

data FTerm t
    = AlgTerm t
    | Application String [FTerm t]
      deriving (Eq, Show)

true :: Formula t
true = And []

truth :: Eq t => Formula t -> Bool
truth f = f == true

says :: Eq t => t -> Formula t -> Formula t
says t f
    | truth f = true
    | otherwise = Says t f

implies :: Eq t => [Formula t] -> Formula t -> Formula t
implies antecedents consequence
    | truth consequence = true
    | null simp = consequence
    | otherwise = Implies (simp ++ [consequence])
    where
      simp = filter (not . truth) antecedents

-- Free variables in a formula

freeVars :: Algebra t p g s e c => Formula t -> S.Set t
freeVars f = freeVarsForm S.empty f

freeVarsForm :: Algebra t p g s e c => S.Set t -> Formula t -> S.Set t
freeVarsForm s (Predicate _ ts) = foldl freeVarsFTerm s ts
freeVarsForm s (Not f) = freeVarsForm s f
freeVarsForm s (And fs) = foldl freeVarsForm s fs
freeVarsForm s (Or fs) = foldl freeVarsForm s fs
freeVarsForm s (Implies fs) = foldl freeVarsForm s fs
freeVarsForm s (Says t f) = freeVarsForm (freeVarsTerm s t) f
freeVarsForm s (Forall ts f) = foldl (flip S.delete) (freeVarsForm s f) ts
freeVarsForm s (Exists ts f) = foldl (flip S.delete) (freeVarsForm s f) ts

freeVarsFTerm :: Algebra t p g s e c => S.Set t -> FTerm t -> S.Set t
freeVarsFTerm s (AlgTerm t) = freeVarsTerm s t
freeVarsFTerm s (Application _ ts) = foldl freeVarsFTerm s ts

freeVarsTerm :: Algebra t p g s e c => S.Set t -> t -> S.Set t
freeVarsTerm s t = foldVars (flip S.insert) s t

-- Formula instantiation

-- The implementations assumes the environment maps no bound
-- variables.

finstantiate :: Algebra t p g s e c => e -> Formula t -> Formula t
finstantiate env (Predicate sym ts) =
    Predicate sym (map (tinstantiate env) ts)
finstantiate env (Not f) =
    Not (finstantiate env f)
finstantiate env (And fs) =
    And (map (finstantiate env) fs)
finstantiate env (Or fs) =
    Or (map (finstantiate env) fs)
finstantiate env (Implies fs) =
    Implies (map (finstantiate env) fs)
finstantiate env (Says t f) =
    Says (instantiate env t) (finstantiate env f)
finstantiate env (Forall ts f) =
    Forall ts (finstantiate env f)
finstantiate env (Exists ts f) =
    Exists ts (finstantiate env f)

tinstantiate :: Algebra t p g s e c => e -> FTerm t -> FTerm t
tinstantiate env (AlgTerm t) =
    AlgTerm (instantiate env t)
tinstantiate env (Application sym ts) =
    Application sym (map (tinstantiate env) ts)

-- The follow translations are used on input and output so as to
-- reduce the number of cases used internally.

-- f == (or)
-- t == (and)

-- The follow translation is used on input to get rid of the form

-- (iff a b) == (and (implies a b) (implies b a))

-- Keywords used in the S-Expression representation of formulas

-- The following keywords may not be used as variables, function
-- symbols, or predicate symbols.
keywords :: [String]
keywords = ["forall", "exists", "says", "not",
            "or", "and", "implies", "iff"]

-- Load a formula.  The vars list is the list of variables that are in
-- scope.  The variable generator for the algebra is threaded through
-- the loading process so that every bound variable is quantified
-- once, and no free variable is bound within a formula.  Use
-- loadDecls to ensure vars are not keywords.
loadFormula :: (Algebra t p g s e c, MonadFail m) => [t] -> g ->
               SExpr Pos -> m (g, Formula t)
loadFormula vars gen (L _ (S _ pred : xs)) -- Atomic formula
    | notElem pred keywords =
        do
          fs <- loadFTerms vars xs
          return (gen, Predicate pred fs)
loadFormula vars gen (L _ (S _ "or" : xs)) = -- Disjunction
    do
      (gen', ts) <- mapAccumLM (loadFormula vars) gen xs
      return (gen', Or ts)
loadFormula vars gen (L _ (S _ "and" : xs)) = -- Conjunction
    do
      (gen', ts) <- mapAccumLM (loadFormula vars) gen xs
      return (gen', And ts)
loadFormula vars gen (L _ [S _ "not", x]) = -- Negation
    do
      (gen', t) <- loadFormula vars gen x
      return (gen', Not t)
loadFormula vars gen (L _ (S _ "implies" : x : xs)) = -- Implication
    do
      (gen', ts) <- mapAccumLM (loadFormula vars) gen (x : xs)
      return (gen', Implies ts)
loadFormula vars gen (L pos [S pos' "iff", x, y]) = -- If and only if
    loadFormula vars gen (L pos [S pos' "and",
                                 L pos [S pos' "implies", x, y],
                                 L pos [S pos' "implies", y, x]])
loadFormula vars gen (L _ [S _ "says", x, y]) = -- Says modal operator
    do
      t <- loadTerm vars False x
      (gen', f) <- loadFormula vars gen y
      return (gen', Says t f)
loadFormula vars gen (L _ [S _ "forall", L _ xs, x]) = -- Universal
    loadQuantified Forall vars gen xs x                -- quantification
loadFormula vars gen (L _ [S _ "exists", L _ xs, x]) = -- Existential
    loadQuantified Exists vars gen xs x                -- quantification
loadFormula _ _ x =
    fail (shows (annotation x) "Malformed formula")

loadFTerms :: (Algebra t p g s e c, MonadFail m) => [t] ->
              [SExpr Pos] -> m [FTerm t]
loadFTerms vars xs =
    foldM f [] (reverse xs)
    where
      f acc x =
          case loadFTerm vars x of
            Return t -> return (t : acc)
            Fail msg -> fail msg

-- Load a formula term
loadFTerm :: Algebra t p g s e c => [t] -> SExpr Pos ->
             ReturnFail (FTerm t)
loadFTerm vars x =
    case loadTerm vars False x of
      Return t -> return (AlgTerm t)
      Fail msg ->               -- If x is not an algebra term
          case x of             -- see if it's an application
            L _ (S pos fun : xs)
                | elem fun keywords ->
                    Fail (shows pos $
                           "Keyword " ++ fun ++ " used as a function")
                | otherwise ->
                 case mapM (loadFTerm vars) xs of
                   Return ts -> Return (Application fun ts)
                   Fail _ -> Fail msg
            _ -> Fail msg

-- A monad version of map accumulation from the left
mapAccumLM :: Monad m => (a -> b -> m (a, c)) -> a -> [b] -> m (a, [c])
mapAccumLM _ z [] =
    return (z, [])
mapAccumLM f z (x : xs) =
    do
      (z', y) <- f z x
      (z'', ys) <- mapAccumLM f z' xs
      return (z'', y : ys)

-- Load a quantified formula.  Note that loadVars returns variables in
-- reverse order, so list append is the correct way to extend the list
-- of variables that are in scope.
loadQuantified :: (Algebra t p g s e c, MonadFail m) =>
                  ([t] -> Formula t -> Formula t) ->
                  [t] -> g -> [SExpr Pos] -> SExpr Pos ->
                  m (g, Formula t)
loadQuantified build vars gen decls body =
    do
      (gen', vars') <- loadDecls gen decls
      (gen'', f) <- loadFormula (vars' ++ vars) gen' body
      return (gen'', build (reverse vars') f)

loadDecls :: (Algebra t p g s e c, MonadFail m) => g ->
             [SExpr Pos] -> m (g, [t])
loadDecls gen decls =
    do
      mapM_ checkDecl decls     -- Check for keywords
      loadVars gen decls

-- Fail if a variable is used as a keyword
checkDecl :: MonadFail m => SExpr Pos -> m ()
checkDecl (L _ (S pos sym : _))
    | elem sym keywords =
        fail (shows pos "Keyword " ++ sym ++ " used as a variable")
checkDecl _ = return ()

-- Display a formula
displayFormula :: Algebra t p g s e c => [t] ->
                  Formula t -> SExpr ()
displayFormula vars form =
    displayForm ctx' form
    where                       -- Add quantified vars last so they are
      ctx = addToContext emptyContext vars -- most like to be renamed.
      ctx' = addToContext ctx (quantified form)

-- Collect the quantified variables
quantified :: Algebra t p g s e c => Formula t -> [t]
quantified (Predicate _ _) = []
quantified (Not f) = quantified f
quantified (And fs) = concatMap quantified fs
quantified (Or fs) = concatMap quantified fs
quantified (Implies fs) = concatMap quantified fs
quantified (Says _ f) = quantified f
quantified (Forall ts f) = ts ++ quantified f
quantified (Exists ts f) = ts ++ quantified f

displayForm :: Algebra t p g s e c => c -> Formula t -> SExpr ()
displayForm ctx (Predicate sym ts) =
    L () (S () sym : map (displayFTerm ctx) ts)
displayForm ctx (Not f) =
    L () [S () "not", displayForm ctx f]
displayForm ctx (And fs) =
    L () (S () "and" : map (displayForm ctx) fs)
displayForm ctx (Or fs) =
    L () (S () "or" : map (displayForm ctx) fs)
displayForm ctx (Implies fs) =
    L () (S () "implies" : map (displayForm ctx) fs)
displayForm ctx (Says t f) =
    L () [S () "says", displayTerm ctx t, displayForm ctx f]
displayForm ctx (Forall ts f) =
    L () [S () "forall", L () (displayVars ctx ts), displayForm ctx f]
displayForm ctx (Exists ts f) =
    L () [S () "exists", L () (displayVars ctx ts), displayForm ctx f]

displayFTerm :: Algebra t p g s e c => c -> FTerm t -> SExpr ()
displayFTerm ctx (AlgTerm t) =
    displayTerm ctx t
displayFTerm ctx (Application sym ts) =
    L () (S () sym : map (displayFTerm ctx) ts)
