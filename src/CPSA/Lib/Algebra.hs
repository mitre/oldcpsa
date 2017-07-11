-- Defines the interface to CPSA algebras.

-- Copyright (c) 2009 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module CPSA.Lib.Algebra where

import Data.Set (Set)
import CPSA.Lib.SExpr

-- Algebras

-- This class collects all the types that make up an algebra.
-- Non-algebra specific types should use this type class in their
-- context, and not any of the others in this module.

class (Term t, Place t p, Gen t g, Subst t g s,
       Env t g s e, Context t g s e c) =>
    Algebra t p g s e c

-- Terms

class (Ord t, Show t) => Term t where

  -- These predicates succeed only on terms in the message algebra.
    isVar :: t -> Bool          -- Is term a variable in the message algebra?
    isAcquiredVar :: t -> Bool  -- Is term an acquired variable?
    isAtom :: t -> Bool       -- Is the sort of this term a base sort?

    -- Extension for security goals, a predicate that succeeds only
    -- when applied to node variables.
    isNodeVar :: t -> Bool

    -- Does a variable occur in a term?
    occursIn :: t -> t -> Bool

    -- Check to see if a list of terms is well-formed.
    termsWellFormed :: [t] -> Bool

    -- Fold a function through a term applying it to each variable in
    -- the term.
    foldVars :: (a -> t -> a) -> a -> t -> a

    -- Fold a function through a term applying it to each term carried
    -- by the term.
    foldCarriedTerms :: (a -> t -> a) -> a -> t -> a

    -- Is a term carried by another term?  In other words, does the
    -- possession of the appropriate keys allow the term to be
    -- extracted from the other term?
    carriedBy :: t -> t -> Bool

    -- Is atom a constituent of a term?  In other words, is atom among
    -- the set of atoms required to construct the term?
    constituent :: t -> t -> Bool

    -- Returns the key used to decrypt an encryption term, otherwise
    -- Nothing.
    decryptionKey :: t -> Maybe t

    -- decompose outpred avoid returns minimum sets required to
    -- determine if a term is penetrator derivable, where outpred is a
    -- set of previously sent messages and avoid is an avoidance set.
    -- An atom in the avoidance set cannot be guess by the penetrator,
    -- except when it is exposed in the sent terms.
    decompose :: Set t -> Set t -> (Set t, Set t)

    -- buildable outpred avoid term is true when the penetrator can
    -- derive the term given a minimum previously sent message set and
    -- an avoidance set, as computed using the decompose function.
    buildable :: Set t -> Set t -> t -> Bool

    -- encryptions term returns a list of encryptions carried by the
    -- term, each with the keys used to prepare it, with duplicates
    -- eliminated.  Encryptions that occur in other encryption are
    -- later in the list.
    encryptions :: t -> [(t,[t])]

    -- Assume (ts, a) is the output of decomposable, that is ts is a
    -- set of encriptions and a is the reduced avoidance set.
    -- escapeSet ts a ct = Nothing if buildable ts a ct = True.
    -- Otherwise, escapeSet ts a ct = Just es, where es is the escape
    -- set.
    escapeSet :: Set t -> Set t -> t -> Maybe (Set t)

    -- Given a list of variables, load a term from an S-expression.
    loadTerm :: Monad m => [t] -> Bool -> SExpr Pos -> m t

    -- Determines if an element is "numeric", meaning that it is
    -- part of a numeric sort.
    isNum :: t -> Bool

    -- Finds all (outermost) numeric subterms occurring in a given term
    subNums :: t -> Set t

    -- Given a term t, return the first variable in t.
    forceVar :: t -> t

    -- Determines whether a given target is derivable, given a set
    -- of known messages and an avoidance set.
    derivable :: Set t -> Set t -> t -> Bool

    -- Returns the indicator in a given variable of a term.
    indicator :: t -> t -> Maybe Int

-- The place at which a term occurs in another term

class (Term t, Eq p, Show p) => Place t p | t -> p, p -> t where
    -- places variable source returns a list of places at which the
    -- variable occurs in the term.
    places :: t -> t -> [p]
    -- carriedPlaces target source returns a list of places at which
    -- the target is carried in the term.
    carriedPlaces :: t -> t -> [p]
    carriedRelPlaces :: t -> t -> Set t -> [p]
    -- replace variable place source returns the term that results
    -- from replacing the variable at the give place in the source
    -- term.  The sort of the variable must match the one used to
    -- create the place.
    replace :: t -> p -> t -> t
    -- ancestors source place extracts the terms in the source that
    -- contain the term at the given place.
    ancestors :: t -> p -> [t]
    -- placeIsPrefixOf place place' returns true if place is a prefix
    -- of place'.
    placeIsPrefixOf :: p -> p -> Bool
    -- placeStripPrefix prefix place drops a prefix from a place or
    -- return Nothing when prefix is not a prefix of place.
    placeStripPrefix :: p -> p -> Maybe p

-- Generation of terms with fresh variables.

class (Term t, Show g) => Gen t g | t -> g, g -> t where
    -- The starting generator.
    origin :: g

    -- Given a generator, generate a clone of a term in which each
    -- variable has been replaced by a variable that has never been
    -- generated by the generator.
    clone :: g -> t -> (g, t)

    -- Given a generator and a single numeric term, create a generic
    -- representative of values derivable from that term.  Produces
    -- a gen because this generates a variable.
    genericize :: g -> t -> (g, t)

    -- Given a generator and a single term, create a generic
    -- representative of values relevant to that term
    -- genericVersion :: g -> t -> (g, t)

    -- Given a generator, load a list of variables or return an error
    -- message.  Each element of the list is an identifier and a sort.
    -- The varibles are returned in the reverse order.
    loadVars :: Monad m => g -> [SExpr Pos] -> m (g, [t])

-- Substitutions

-- A substitution is a map from variables to terms.  Two terms unify
-- if there is a substitution, that when applied to both terms,
-- produces the same term.
class (Term t, Gen t g, Ord s, Show s) => Subst t g s | t -> s, s -> t where
    emptySubst :: s
    destroyer :: t -> Maybe s
    substitute :: s -> t -> t
    unify :: t -> t -> (g, s) -> [(g, s)]
    compose :: s -> s -> s

-- Environments

-- An environment is a partial map from variables to terms.  It is
-- used to relate the variables in a role to ones in an instance of
-- the role, and for other tasks involving matching.
class (Term t, Gen t g, Subst t g s, Ord e, Show e) => Env t g s e
    | t -> e, e -> t where
    emptyEnv :: e
    instantiate :: e -> t -> t
    match :: t -> t -> (g, e) -> [(g, e)]
    -- Can environment be refined so it is the identity when applied
    -- to some terms?
    identityEnvFor :: (g, e) -> [t] -> [(g, e)]
    -- Cast an environment into a substitution
    substitution :: e -> s
    -- Provide a concrete representation of an environment as an
    -- association list.  The first argument is a list of variables
    -- that make up the domain of the environment.
    reify :: [t] -> e -> [(t, t)]
    -- Is match a one-to-one variable-to-variable map?  This function
    -- is used while testing if two skeletons are isomorphic.
    matchRenaming :: (g, e) -> Bool
    -- Extensions for security goals: node match.  To succeed, the
    -- term must be a variable of sort node.
    nodeMatch :: t -> (Int, Int) -> (g, e) -> [(g, e)]
    -- Node lookup.  If t is a node variable and is bound to a pair in
    -- e, then return the pair, otherwise return nothing.
    nodeLookup :: e -> t -> Maybe (Int, Int)

-- Display contexts--maps from variables to their printed representation.

class (Term t, Gen t g, Subst t g s, Env t g s e, Show c) => Context t g s e c
    | t -> c, c -> t where
    emptyContext :: c             -- The initial context
    addToContext :: c -> [t] -> c -- Add to context from some terms
    displayVars :: c -> [t] -> [SExpr ()]
    displayTerm :: c -> t -> SExpr ()
    displayEnv :: c -> c -> e -> [SExpr ()]
    -- A substitution display routine is required due to the fact that
    -- the sort of some variables in the substitition might not be
    -- known.  For the purposes of displaying the substitition, enough
    -- sort information can be inferred.  For displaySubst, there may
    -- be variables in the substitution that are not in the context.
    displaySubst :: c -> s -> [SExpr ()]
