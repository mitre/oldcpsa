-- Basic Crypto Algebra implementation

-- The module implements a many-sorted algebra, but is used as an
-- order-sorted algebra.  It exports a name, and the origin used to
-- generate variables.

-- To support security goals, the message algebra has been augmented
-- with support for variables of sort node and pairs of integers.  The
-- constructor D is used as N is taken for numbers in S-Expressions.

-- Copyright (c) 2009 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

{-# LANGUAGE MultiParamTypeClasses #-}

module CPSA.Basic.Algebra (name, origin) where

import Control.Monad (foldM)
import qualified Data.List as L
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Char (isDigit)
import qualified CPSA.Lib.CPSA as C
import CPSA.Lib.CPSA (SExpr(..), Pos, annotation)

name :: String
name = "basic"

-- An identifier is a variable without any information about its sort

newtype Id = Id (Integer, String) deriving Show

-- The integer distinguishes an identifier, the string is for printing.

instance Eq Id where
    (Id (x, _)) == (Id (x', _)) = x == x'

instance Ord Id where
    compare (Id (x, _)) (Id (x', _)) = compare x x'

idName :: Id -> String
idName (Id (_, name)) = name

-- Counter used for generating fresh identifiers.

newtype Gen = Gen (Integer) deriving Show

origin :: Gen
origin = Gen (0)

freshId :: Gen -> String -> (Gen, Id)
freshId (Gen (i)) name = (Gen (i + 1), Id (i, name))

cloneId :: Gen -> Id -> (Gen, Id)
cloneId gen x = freshId gen (idName x)

-- The Basic Crypto Order-Sorted Signature is

-- Sorts: mesg, text, data, name, skey, akey, and string
--
-- Subsorts: text, data, name, skey, akey < mesg
--
-- Operations:
--   cat : mesg X mesg -> mesg               Pairing
--   enc : mesg X mesg -> mesg               Encryption
--   hash : mesg -> mesg                     Hashing
--   string : mesg                           Tag constants
--   ltk : name X name -> skey               Long term shared key
--   pubk : name -> akey                     Public key of principal
--   pubk : string X name -> akey            Tagged public key of principal
--   invk : akey -> akey                     Inverse of asymmetric key

-- Variables of sort string are forbidden.

-- The implementation exploits the isomorphism between order-sorted
-- algebras and many-sorted algebras by adding inclusion operations to
-- produce an equivalent Basic Crypto Many-Sorted Signature.  There is
-- an inclusion operation for each subsort of mesg.

-- Sorts: mesg, text, data, name, skey, akey, and string
--
-- Operations:
--   cat : mesg X mesg -> mesg               Pairing
--   enc : mesg X mesg -> mesg               Encryption
--   hash : mesg -> mesg                     Hashing
--   string : mesg                           Tag constants
--   ltk : name X name -> skey               Long term shared key
--   pubk : name -> akey                     Public key of principal
--   pubk : string X name -> akey            Tagged public key of principal
--   invk : akey -> akey                     Inverse of asymmetric key
--   text : text -> mesg                     Sort text inclusion
--   data : data -> mesg                     Sort date inclusion
--   name : name -> mesg                     Sort name inclusion
--   skey : skey -> mesg                     Sort skey inclusion
--   akey : akey -> mesg                     Sort akey inclusion

-- In both algebras, invk(invk(t)) = t for all t of sort akey

-- Operations other than the tag constant constructor
data Symbol
    = Text                      -- Text
    | Data                      -- Another text-like item
    | Name                      -- Principal
    | Skey                      -- Symmetric key
    | Ltk                       -- Long term shared symmetric key
    | Akey                      -- Asymmetric key
    | Invk                      -- Inverse of asymmetric key
    | Pubk                      -- Public asymmetric key of a principal
    | Cat                       -- Term concatenation (Pairing really)
    | Enc                       -- Encryption
    | Hash                      -- Hashing
      deriving (Show, Eq, Ord, Enum, Bounded)

-- A Basic Crypto Algebra Term

data Term
    = I !Id
    | C !String                 -- Tag constants
    | F !Symbol ![Term]
    | D !Id                     -- Node variable
    | P (Int, Int)              -- Node constant
      deriving (Show, Eq, Ord)

-- In this algebra (F Invk [F Invk [t]]) == t is an axiom

-- Basic terms are introduced by defining a function used to decide if
-- a term is well-formed.  The context of an occurrence of an identifier
-- determines its sort.  A term that contains just an identifier and its
-- sort information is called a variable.  The sort of a variable is
-- one of mesg, text, data, name, skey, and akey.

-- Terms that represent algebra variables.
isVar :: Term -> Bool
isVar (I _) = True           -- Sort: mesg
isVar (F s [I _]) =
    -- Sorts: text, data, name, skey, and akey
    s == Text || s == Data || s == Name || s == Skey || s == Akey
isVar _ = False

-- Note that isVar of (D _) is false.
isNodeVar :: Term -> Bool
isNodeVar (D _) = True
isNodeVar _ = False

subNums :: Term -> Set Term
subNums _ = S.empty

indicator :: Term -> Term -> Maybe Int
indicator _ _ = Nothing

forceVar :: Term -> Term
forceVar (I x) = (I x)
forceVar (C x) = (C x) -- no variable!
forceVar (F Text y) = (F Text y)
forceVar (F Data y) = (F Data y)
forceVar (F Name y) = (F Name y)
forceVar (F Skey [(I x)]) = (F Skey [(I x)])
forceVar (F Skey [(F Ltk ns)]) = forceVar (head ns)
forceVar (F Akey [(I x)]) = (F Akey [(I x)])
forceVar (F Akey [F Invk ns]) = forceVar (head ns)
forceVar (F Akey [F Pubk ns]) = forceVar (head ns)
forceVar (F _ ns) = forceVar (head ns) -- for Cat, Enc.
forceVar (D x) = (D x)
forceVar (P x) = (P x) -- no variable!

-- Extract the identifier from a variable
varId :: Term -> Id
varId (I x) = x
varId (F Text [I x]) = x
varId (F Data [I x]) = x
varId (F Name [I x]) = x
varId (F Skey [I x]) = x
varId (F Akey [I x]) = x
varId (D x) = x
varId _ = C.assertError "Algebra.varId: term not a variable with its sort"

isAcquiredVar :: Term -> Bool
isAcquiredVar (I _) = True
isAcquiredVar _ = False

-- A list of terms are well-formed if each one has the correct
-- structure and every occurrence of an identifier in a term has the
-- same sort.  Variable environments are used to check the sort
-- condition.  It maps an identifier to a variable that contains the
-- identifier.

-- termsWellFormed u ensures all terms in u use each identifier at the
-- same sort, and makes sure every term has the correct structure.
termsWellFormed :: [Term] -> Bool
termsWellFormed u =
    loop emptyVarEnv u
    where
      loop _ [] = True
      loop env (t : u) =
          case termWellFormed env t of
            Nothing -> False
            Just env' -> loop env' u

newtype VarEnv = VarEnv (Map Id Term) deriving Show

emptyVarEnv :: VarEnv
emptyVarEnv = VarEnv M.empty

-- A term is well-formed if it is in the language defined by the
-- following grammar, and variables occur at positions within the term
-- in a way that allows each variable to be assigned one sort.
--
-- The start symbol is T, for all well-formed terms.  Terminal symbol
-- I is for variables and terminal symbol C is for quoted strings.
-- The non-terminal symbols are B, K, and T.  Symbol B is for base
-- sorted terms, and K is for asymmetric keys.
--
--     T ::= I | C | B | cat(T, T) | enc(T, T) | hash(T)
--
--     B ::= text(I) | data(I) | name(I) | skey(I)
--        |  skey(I) | skey(ltk(I, I)) | akey(K)
--
--     K ::= I | pubk(I) | invk(I) | invk(pubk(I))

-- termWellFormed checks the structure and sort condition.
termWellFormed :: VarEnv -> Term -> Maybe VarEnv
termWellFormed xts t@(I x) =
    extendVarEnv xts x t        -- Mesg variable
termWellFormed xts t@(F Text [I x]) =
    extendVarEnv xts x t        -- Text variable
termWellFormed xts t@(F Data [I x]) =
    extendVarEnv xts x t        -- Data variable
termWellFormed xts t@(F Name [I x]) =
    extendVarEnv xts x t        -- Name variable
termWellFormed xts t@(F Skey [I x]) =
    extendVarEnv xts x t        -- Symmetric key variable
termWellFormed xts (F Skey [F Ltk [I x, I y]]) =
    -- Long term shared symmetric key
    doubleTermWellFormed xts (F Name [I x]) (F Name [I y])
termWellFormed xts (F Akey [t]) = -- Asymmetric key terms
    case t of
      I x -> extendVarEnv xts x (F Akey [I x])
      F Invk [I x] -> extendVarEnv xts x (F Akey [I x])
      F Pubk [I x] -> extendVarEnv xts x (F Name [I x])
      F Pubk [C _, I x] -> extendVarEnv xts x (F Name [I x])
      F Invk [F Pubk [I x]] -> extendVarEnv xts x (F Name [I x])
      F Invk [F Pubk [C _, I x]] -> extendVarEnv xts x (F Name [I x])
      _ -> Nothing
termWellFormed xts (C _) =
    Just xts                    -- Tags
termWellFormed xts (F Cat [t0, t1]) =
    doubleTermWellFormed xts t0 t1  -- Concatenation
termWellFormed xts (F Enc [t0, t1]) =
    doubleTermWellFormed xts t0 t1  -- Encryption
termWellFormed xts (F Hash [t])     =
    termWellFormed xts t            -- Hashing
termWellFormed _ _ = Nothing

-- Extend when sorts agree
extendVarEnv :: VarEnv -> Id -> Term -> Maybe VarEnv
extendVarEnv (VarEnv env) x t =
    case M.lookup x env of
      Nothing -> Just $ VarEnv $ M.insert x t env
      Just t' -> if t == t' then Just (VarEnv env) else Nothing

doubleTermWellFormed :: VarEnv -> Term -> Term -> Maybe VarEnv
doubleTermWellFormed xts t0 t1 =
    do
      xts <- termWellFormed xts t0
      termWellFormed xts t1

-- Is the sort of the term a base sort?
isAtom :: Term -> Bool
isAtom (I _) = False
isAtom (C _) = False
isAtom (F s _) =
    s == Text || s == Data || s == Name || s == Skey || s == Akey
isAtom (D _) = False
isAtom (P _) = False

-- Is the term numeric?
isNum :: Term -> Bool
isNum _ = False

-- Does a variable occur in a term?
occursIn :: Term -> Term -> Bool
occursIn t t' | isVar t =       -- Does a variable occur in a term?
  subterm (I $ varId t) t'
occursIn t _ =
  C.assertError $ "Algebra.occursIn: Bad variable " ++ show t

subterm :: Term -> Term -> Bool
subterm t t' | t == t' =
  True
subterm t (F _ u) =
  any (subterm t) u
subterm _ _ =
  False

-- Fold f through a term applying it to each variable in the term.
foldVars :: (a -> Term -> a) -> a -> Term -> a
foldVars f acc t@(I _) = f acc t          -- Mesg variable
foldVars f acc t@(F Text [I _]) = f acc t -- Text variable
foldVars f acc t@(F Data [I _]) = f acc t -- Data variable
foldVars f acc t@(F Name [I _]) = f acc t -- Name variable
foldVars f acc t@(F Skey [I _]) = f acc t -- Symmetric keys
foldVars f acc (F Skey [F Ltk [I x, I y]]) =
    f (f acc (F Name [I x])) (F Name [I y])
foldVars f acc t@(F Akey [I _]) = f acc t -- Asymmetric keys
foldVars f acc (F Akey [F Invk [I x]]) = f acc (F Akey [I x])
foldVars f acc (F Akey [F Pubk [I x]]) = f acc (F Name [I x])
foldVars f acc (F Akey [F Pubk [C _, I x]]) = f acc (F Name [I x])
foldVars f acc (F Akey [F Invk [F Pubk [I x]]]) = f acc (F Name [I x])
foldVars f acc (F Akey [F Invk [F Pubk [C _, I x]]]) = f acc (F Name [I x])
foldVars _ acc (C _) = acc        -- Tags
foldVars f acc (F Cat [t0, t1]) = -- Concatenation
    foldVars f (foldVars f acc t0) t1
foldVars f acc (F Enc [t0, t1]) = -- Encryption
    foldVars f (foldVars f acc t0) t1
foldVars f acc (F Hash [t])     = -- Hashing
    foldVars f acc t
foldVars f acc t@(D _) = f acc t          -- Node variable
foldVars _ _ t = C.assertError $ "Algebra.foldVars: Bad term " ++ show t

-- Fold f through a term applying it to each term that is carried by the term.
foldCarriedTerms :: (a -> Term -> a) -> a -> Term -> a
foldCarriedTerms f acc t@(F Cat [t0, t1]) = -- Concatenation
    foldCarriedTerms f (foldCarriedTerms f (f acc t) t0) t1
foldCarriedTerms f acc t@(F Enc [t0, _]) = -- Encryption
    foldCarriedTerms f (f acc t) t0
foldCarriedTerms f acc t = f acc t     -- atoms and tags

-- Is a term carried by another term?
carriedBy :: Term -> Term -> Bool
carriedBy t t' =
    t == t' ||
      case t' of
        F Cat [t0, t1] -> carriedBy t t0 || carriedBy t t1
        F Enc [t0, _] -> carriedBy t t0
        _ -> False

-- Is atom a constituent of a term?  In other words, is atom among
-- the set of atoms required to construct the term?
constituent :: Term -> Term -> Bool
constituent t t' | isAtom t =
  subterm t t'
constituent t _ =
  C.assertError $ "Algebra.constituent: Bad atom " ++ show t

-- The key used to decrypt an encrypted term, otherwise Nothing.
decryptionKey :: Term -> Maybe Term
decryptionKey (F Enc [_, t]) = Just (inv t)
decryptionKey _ = Nothing

buildable :: Set Term -> Set Term -> Term -> Bool
buildable knowns unguessable term =
    ba term
    where
      ba (I _) = True           -- A mesg sorted variable is always buildable
      ba (C _) = True           -- and so is a tag
      ba (F Cat [t0, t1]) =
          ba t0 && ba t1
      ba t@(F Enc [t0, t1]) =
          S.member t knowns || ba t0 && ba t1
      ba t@(F Hash [t1]) =
          S.member t knowns || ba t1
      ba t = isAtom t && not (S.member t unguessable)

-- Penetrator derivable predicate and checking for unrealized skeletons.

derivable :: Set Term -> Set Term -> Term -> Bool
derivable avoid sent term =
    let (knowns, unknowns) = decompose sent avoid in
    buildable knowns unknowns term

-- Compute the decomposition given some known terms and some unguessable
-- atoms.  The code is quite tricky.  It iterates until the known
-- terms don't change.  The known terms ends up with all the
-- encryptions that are known.
decompose :: Set Term -> Set Term -> (Set Term, Set Term)
decompose knowns unguessable =
    loop unguessable knowns S.empty []
    where
      loop unguessable knowns old []
          | old == knowns = (knowns, unguessable) -- Done
          | otherwise = loop unguessable knowns knowns (S.elems knowns)
      loop unguessable knowns old (t@(F Cat _) : todo) =
          loop unguessable (decat t (S.delete t knowns)) old todo
      loop unguessable knowns old ((F Enc [t0, t1]) : todo)
          | buildable knowns unguessable (inv t1) = -- Add plaintext
              loop unguessable (decat t0 knowns) old todo
          | otherwise = loop unguessable knowns old todo
      loop unguessable knowns old ((F Hash [_]) : todo) =
          loop unguessable knowns old todo -- Hash can't be decomposed
      loop unguessable knowns old (t : todo) =
          loop (S.delete t unguessable) (S.delete t knowns) old todo
      -- Decat
      decat (F Cat [t0, t1]) s = decat t1 (decat t0 s)
      decat t s = S.insert t s

-- Inverts an asymmetric key
inv :: Term -> Term
inv (F Akey [F Invk [t]]) = F Akey [t]
inv (F Akey [t]) = F Akey [F Invk [t]]
inv (I _) = error "Algebra.inv: Cannot invert a variable of sort mesg"
inv t = t

-- Extracts every encryption that is carried by a term along with its
-- encryption key.  Note that a hash is treated as a kind of
-- encryption in which the term that is hashed is the encryption key.
encryptions :: Term -> [(Term, [Term])]
encryptions t =
    reverse $ loop t []
    where
      loop (F Cat [t, t']) acc =
          loop t' (loop t acc)
      loop t@(F Enc [t', t'']) acc =
          loop t' (adjoin (t, [t'']) acc)
      loop t@(F Hash [t']) acc =
          adjoin (t, [t']) acc
      loop _ acc = acc
      adjoin x xs
          | x `elem` xs = xs
          | otherwise = x : xs

escapeSet :: Set Term -> Set Term -> Term -> Maybe (Set Term)
escapeSet ts a ct =
    if buildable ts a ct then
        Nothing
    else
        Just $ S.filter f ts
        where
          f (F Enc [t, key]) =
              carriedBy ct t &&
              not (buildable ts a (inv key))
          f _ = False

instance C.Term Term where
    derivable = derivable
    isNum = isNum
    subNums = subNums
    indicator = indicator
    forceVar = forceVar
    isVar = isVar
    isAcquiredVar = isAcquiredVar
    isAtom = isAtom
    isNodeVar = isNodeVar
    termsWellFormed = termsWellFormed
    occursIn = occursIn
    foldVars = foldVars
    foldCarriedTerms = foldCarriedTerms
    carriedBy = carriedBy
    constituent = constituent
    decryptionKey = decryptionKey
    decompose = decompose
    buildable = buildable
    encryptions = encryptions
    escapeSet = escapeSet
    loadTerm = loadTerm2

-- Places

-- A place names a one subterm within a term.  It is a list of
-- integers giving a path through a term to that named subterm.  Each
-- integer in the list identifies the subterm in a function
-- application on the path to the named subterm.  The integer is the
-- index of the subterm in the application's list of terms.

newtype Place = Place [Int] deriving (Eq, Show)

-- Returns the places a variable occurs within another term.
places :: Term -> Term -> [Place]
places target source =
    f [] [] source
    where
      f paths path source
          | target == source = Place (reverse path) : paths
      f paths path (F _ u) =
          g paths path 0 u
      f paths _ _ = paths
      g paths _ _ [] = paths
      g paths path i (t : u) =
          g (f paths (i: path) t) path (i + 1) u

-- Replace a variable within a term at a given place.
replace :: Term -> Place -> Term -> Term
replace target (Place ints) source =
    loop ints source
    where
      loop [] _ = target
      loop (i : path) (F s u) =
          if length u <= i then
            C.assertError ("Bogus i " ++ show i)
          else
            F s (C.replaceNth (loop path (u !! i)) i u)
      loop _ _ = C.assertError "Algebra.replace: Bad path to variable"

-- Returns the places a term is carried by another term.
carriedPlaces :: Term -> Term -> [Place]
carriedPlaces target source =
    f [] [] source
    where
      f paths path source
          | target == source = Place (reverse path) : paths
      f paths path (F Cat [t, t']) =
          f (f paths  (0 : path) t) (1 : path) t'
      f paths path (F Enc [t, _]) =
          f paths (0 : path) t
      f paths _ _ = paths

-- Return the ancestors of the term at the given place.
ancestors :: Term -> Place -> [Term]
ancestors source (Place ints) =
    loop [] ints source
    where
      loop ts [] _ = ts
      loop ts (i: path) t@(F _ u) =
          loop (t : ts) path (u !! i)
      loop _ _ _ = C.assertError "Algebra.ancestors: Bad path to term"

prefix :: Place -> Place -> Bool
prefix (Place l) (Place l') = L.isPrefixOf l l'

strip :: Place -> Place -> Maybe Place
strip (Place l) (Place l') =
  loop l l'
  where
    loop [] l' = Just $ Place l'
    loop (i : l) (i' : l') | i == i' = loop l l'
    loop _ _ = Nothing

instance C.Place Term Place where
    places = places
    carriedPlaces = carriedPlaces
    carriedRelPlaces x y _ = carriedPlaces x y
    replace = replace
    ancestors = ancestors
    placeIsPrefixOf = prefix
    placeStripPrefix = strip

genericize :: Gen -> Term -> (Gen, Term)
genericize g t = (g, t)

-- Rename the identifiers in a term.  Gen keeps the state of the
-- renamer.  (Question: should alist be replaced by a Map?)
clone :: Gen -> Term -> (Gen, Term)
clone gen t =
    (gen', t')
    where
      (_, gen', t') = cloneTerm ([], gen) t
      cloneTerm (alist, gen) t =
          case t of             -- The association list maps
            I x ->              -- identifiers to identifier.
                case lookup x alist of
                  Just y -> (alist, gen, I y)
                  Nothing ->
                      let (gen', y) = cloneId gen x in
                      ((x, y) : alist, gen', I y)
            C c -> (alist, gen, C c)
            F sym u ->
                let (alist', gen', u') =
                        foldl cloneTermList (alist, gen, []) u in
                (alist', gen', F sym $ reverse u')
            D x ->              -- identifiers to identifier.
                case lookup x alist of
                  Just y -> (alist, gen, D y)
                  Nothing ->
                      let (gen', y) = cloneId gen x in
                      ((x, y) : alist, gen', D y)
            P p -> (alist, gen, P p)
      cloneTermList (alist, gen, u) t =
          let (alist', gen', t') = cloneTerm (alist, gen) t in
          (alist', gen', t' : u)

instance C.Gen Term Gen where
    origin = origin
    genericize = genericize
--    genericVersion = genericize
    clone = clone
    loadVars = loadVars

-- Functions used in both unification and matching

type IdMap = Map Id Term

emptyIdMap :: IdMap
emptyIdMap = M.empty

-- Apply a substitution to a term
idSubst :: IdMap -> Term -> Term
idSubst subst (I x) =
    M.findWithDefault (I x) x subst
idSubst _ t@(C _) = t
idSubst subst (F Invk [t]) =
    case idSubst subst t of
      F Invk [t] -> t           -- Apply axiom
      t -> F Invk [t]
idSubst subst (F s u) =
    F s (map (idSubst subst) u)
idSubst subst (D x) =
    M.findWithDefault (D x) x subst
idSubst _ t@(P _) = t

-- Unification and substitution

newtype Subst = Subst IdMap deriving (Eq, Ord, Show)

emptySubst :: Subst
emptySubst = Subst emptyIdMap

-- Apply a substitution created by unification
substitute :: Subst -> Term -> Term
substitute (Subst s) t =
    idSubst s t

-- Composition of substitutions

-- substitute (compose s0 s1) t = substitute s0 (substitute s1 t)

-- 1. apply s0 to range of s1 to obtain s2;
-- 2. remove bindings is s0 where domains of s0 and s1 overlap to form s3;
-- 3. remove trivial bindings from s2 to form s4; and
-- 4. take the union of s4 and s3.

compose :: Subst -> Subst -> Subst
compose (Subst s0) (Subst s1) =
    let s2 = M.map (substitute (Subst s0)) s1        -- Step 1
        s4 = M.filterWithKey nonTrivialBinding s2 in -- Step 3
    Subst (M.union s4 s0)       -- Steps 2 and 4, union is left-biased

nonTrivialBinding :: Id -> Term -> Bool
nonTrivialBinding x (I y) = x /= y
nonTrivialBinding _ _ = True

-- During unification, variables determined to be equal are collected
-- into an equivalence class.  Multiple lookups of each variable in
-- the internal representation of a substitution finds the canonical
-- representive of the class.  The chase function finds the current
-- canonical representitive.

-- Get the canonical representative of equivalent identifiers making use
-- of this algebra's axiom.
chase :: Subst -> Term -> Term
chase (Subst s) (I x) =
    case M.lookup x s of
      Nothing -> I x
      Just t -> chase (Subst s) t
chase (Subst s) (D x) =
    case M.lookup x s of
      Nothing -> D x
      Just t -> chase (Subst s) t
chase s (F Invk [t]) = chaseInvk s t
chase _ t = t

chaseInvk :: Subst -> Term -> Term
chaseInvk (Subst s) (I x) =
    case M.lookup x s of
      Nothing -> F Invk [I x]
      Just t -> chaseInvk (Subst s) t
chaseInvk s (F Invk [t]) = chase s t
chaseInvk _ t = F Invk [t]

-- Does x occur in t?
occurs :: Id -> Term -> Bool
occurs x (I y) = x == y
occurs _ (C _) = False
occurs x (F _ u) = any (occurs x) u
occurs x (D y) = x == y
occurs _ (P _) = False

unifyChase :: Term -> Term -> Subst -> Maybe Subst
unifyChase t t' s = unifyTerms (chase s t) (chase s t') s

unifyTerms :: Term -> Term -> Subst -> Maybe Subst
unifyTerms (I x) (I y) (Subst s)
    | x == y = Just (Subst s)
    | otherwise = Just (Subst $ M.insert x (I y) s)
unifyTerms (I x) t (Subst s)
    | occurs x t = Nothing
    | otherwise = Just (Subst $ M.insert x t s)
unifyTerms t (I x) s = unifyTerms (I x) t s
unifyTerms (C c) (C c') s
    | c == c' = Just s
    | otherwise = Nothing
unifyTerms (F Invk [I x]) (F Pubk [I y]) s =
    unifyTerms (I x) (F Invk [F Pubk [I y]]) s
unifyTerms (F Invk [I x]) (F Pubk [C c, I y]) s =
    unifyTerms (I x) (F Invk [F Pubk [C c, I y]]) s
unifyTerms (F Pubk [I x]) (F Invk [I y]) s =
    unifyTerms (I y) (F Invk [F Pubk [I x]]) s
unifyTerms (F Pubk [C c, I x]) (F Invk [I y]) s =
    unifyTerms (I y) (F Invk [F Pubk [C c, I x]]) s
unifyTerms (F sym u) (F sym' u') s
    | sym == sym' = unifyTermLists u u' s
    | otherwise = Nothing
unifyTerms (D x) (D y) (Subst s)
    | x == y = Just (Subst s)
    | otherwise = Just (Subst $ M.insert x (D y) s)
unifyTerms (D x) (P p) (Subst s) =
    Just (Subst $ M.insert x (P p) s)
unifyTerms t (D x) s = unifyTerms (D x) t s
unifyTerms (P p) (P p') s
    | p == p' = Just s
    | otherwise = Nothing
unifyTerms _ _ _ = Nothing

unifyTermLists :: [Term] -> [Term] -> Subst -> Maybe Subst
unifyTermLists [] [] s = Just s
unifyTermLists (t : u) (t' : u') s =
    maybe Nothing (unifyTermLists u u') (unifyChase t t' s)
unifyTermLists _ _ _ = Nothing

-- The exported unifier converts the internal representation of a
-- substitution into the external form using chaseMap.

unify :: Term -> Term -> Subst -> Maybe Subst
unify t t' s =
    do
      s <- unifyChase t t' s
      return $ chaseMap s

-- Apply the chasing version of substitution to the range of s.

chaseMap :: Subst -> Subst
chaseMap (Subst s) =
    Subst $ M.map (substChase (Subst s)) s

-- A chasing version of substitution.

substChase :: Subst -> Term -> Term
substChase subst t =
    case chase subst t of
      t@(I _) -> t
      t@(C _) -> t
      F Invk [t] ->
          case substChase subst t of
            F Invk [t] -> t           -- Apply axiom
            t -> F Invk [t]
      F s u ->
          F s (map (substChase subst) u)
      t@(D _) -> t
      t@(P _) -> t

destroyer :: Term -> Maybe Subst
destroyer _ = Nothing

instance C.Subst Term Gen Subst where
   emptySubst = emptySubst
   destroyer = destroyer
   substitute = substitute
   unify t t' (g, s) =
       maybe [] (\s -> [(g, s)]) $ unify t t' s
   compose = compose

-- Matching and instantiation

newtype Env = Env IdMap deriving (Eq, Ord, Show)

-- An environment may contain an explicit identity mapping, whereas a
-- substitution is erroneous if it has one.

emptyEnv :: Env
emptyEnv = Env emptyIdMap

-- Apply a substitution created my matching
instantiate :: Env -> Term -> Term
instantiate (Env r) t = idSubst r t

-- Matching

-- The matcher has the property that when pattern P and term T match
-- then instantiate (match P T emptyEnv) P = T.
match ::  Term -> Term -> Env -> Maybe Env
match (I x) t (Env r) =
    case M.lookup x r of
      Nothing -> Just $ Env $ M.insert x t r
      Just t' -> if t == t' then Just $ Env r else Nothing
match (C c) (C c') r = if c == c' then Just r else Nothing
match (F s u) (F s' u') r
    | s == s' = matchLists u u' r
match (F Invk [t]) t' r =
    match t (F Invk [t']) r
match (D x) t (Env r) =
    case M.lookup x r of
      Nothing -> Just $ Env $ M.insert x t r
      Just t' -> if t == t' then Just $ Env r else Nothing
match (P p) (P p') r = if p == p' then Just r else Nothing
match _ _ _ = Nothing

matchLists :: [Term] -> [Term] -> Env -> Maybe Env
matchLists [] [] r = Just r
matchLists (t : u) (t' : u') r =
    maybe Nothing (matchLists u u') (match t t' r)
matchLists _ _ _ = Nothing

-- Does every varible in ts not occur in the domain of e?
-- Trivial bindings in e are ignored.
identityEnvFor :: Env -> [Term] -> Bool
identityEnvFor (Env r) ts =
    all (allId $ flip S.notMember dom) ts
    where
      dom = M.foldrWithKey f S.empty r -- The domain of r
      f x (I y) dom
          | x == y = dom        -- Ignore trivial bindings
          | otherwise = S.insert x dom
      f x _ dom = S.insert x dom

allId :: (Id -> Bool) -> Term -> Bool
allId f (I x) = f x
allId _ (C _) = True
allId f (F _ u) = all (allId f) u
allId f (D x) = f x
allId _ (P _) = True

-- Cast an environment into a substitution by filtering out trivial
-- bindings.

substitution :: Env -> Subst
substitution (Env r) =
    Subst $ M.filterWithKey nonTrivialBinding r

-- Add type information to an environment, and return it as a list of
-- associations.

reify :: [Term] -> Env -> [(Term, Term)]
reify domain (Env env) =
    map (loop domain) $ M.assocs env
    where
      loop [] (x, _) =
          C.assertError $ "Algebra.reify: variable missing from domain " ++ idName x
      loop (I x : _) (y, t)
          | x == y = (I x, t)
      loop (F Text [I x] : _) (y, t)
          | x == y = (F Text [I x], F Text [t])
      loop (F Data [I x] : _) (y, t)
          | x == y = (F Data [I x], F Data [t])
      loop (F Name [I x] : _) (y, t)
          | x == y = (F Name [I x], F Name [t])
      loop (F Skey [I x] : _) (y, t)
          | x == y = (F Skey [I x], F Skey [t])
      loop (F Akey [I x] : _) (y, t)
          | x == y = (F Akey [I x], F Akey [t])
      loop (D x : _) (y, t)
          | x == y = (D x, t)
      loop (_ : domain) pair = loop domain pair

-- Ensure the range of an environment contains only variables and that
-- the environment is injective.
matchRenaming :: Env -> Bool
matchRenaming (Env e) =
    loop S.empty $ M.elems e
    where
      loop _ [] = True
      loop s (I x:e) =
          not (S.member x s) && loop (S.insert x s) e
      loop _ _ = False

nodeMatch ::  Term -> (Int, Int) -> Env -> Maybe Env
nodeMatch t p env = match t (P p) env

nodeLookup :: Env -> Term -> Maybe (Int, Int)
nodeLookup env t =
  case instantiate env t of
    P p -> Just p
    _ -> Nothing

instance C.Env Term Gen Subst Env where
   emptyEnv = emptyEnv
   instantiate = instantiate
   match t t' (g, e) =
       maybe [] (\e -> [(g, e)]) $ match t t' e
   identityEnvFor (g, e) ts =
       if identityEnvFor e ts then
           [(g, e)]
       else
           []
   substitution = substitution
   reify = reify
   matchRenaming (_, e) = matchRenaming e
   nodeMatch t t' (g, e) =
       maybe [] (\e -> [(g, e)]) $ nodeMatch t t' e
   nodeLookup = nodeLookup

-- Term specific loading functions

loadVars :: Monad m => Gen -> [SExpr Pos] -> m (Gen, [Term])
loadVars gen sexprs =
    do
      pairs <- mapM loadVarPair sexprs
      (g, vars) <- foldM loadVar (gen, []) (concat pairs)
      return (g, reverse vars)

loadVarPair :: Monad m => SExpr Pos -> m [(SExpr Pos, SExpr Pos)]
loadVarPair (L _ (x:xs)) =
    let (t:vs) = reverse (x:xs) in
    return [(v,t) | v <- reverse vs]
loadVarPair x = fail (shows (annotation x) "Bad variable declaration")

loadVar :: Monad m => (Gen, [Term]) -> (SExpr Pos, SExpr Pos) ->
           m (Gen, [Term])
loadVar (gen, vars) (S pos name, S pos' sort) =
    case loadLookup pos vars name of
      Right _ ->
          fail (shows pos "Duplicate variable declaration for " ++ name)
      Left _ ->
          do
            let (gen', x) = freshId gen name
            p <- mkVar x
            return (gen', p : vars)
    where
      mkVar x =
          case sort of
            "mesg" -> return (I x)
            "text" -> return $ F Text [I x]
            "data" -> return $ F Data [I x]
            "name" -> return $ F Name [I x]
            "skey" -> return $ F Skey [I x]
            "akey" -> return $ F Akey [I x]
            "node" -> return (D x)
            _ -> fail (shows pos' "Sort " ++ sort ++ " not recognized")
loadVar _ (x,_) = fail (shows (annotation x) "Malformed vars declaration")

loadLookup :: Pos -> [Term] -> String -> Either String Term
loadLookup pos [] name = Left (shows pos $ "Identifier " ++ name ++ " unknown")
loadLookup pos (t : u) name =
    let name' = idName (varId t) in
    if name' == name then Right t else loadLookup pos u name

loadLookupName :: Monad m => Pos -> [Term] -> String -> m Term
loadLookupName pos vars name =
    either fail f (loadLookup pos vars name)
    where
      f t@(F Name [I _]) = return t
      f _ = fail (shows pos $ "Expecting " ++ name ++ " to be a name")

loadLookupAkey :: Monad m => Pos -> [Term] -> String -> m Term
loadLookupAkey pos vars name =
    either fail f (loadLookup pos vars name)
    where
      f t@(F Akey [I _]) = return t
      f _ = fail (shows pos $ "Expecting " ++ name ++ " to be an akey")

loadTerm2 :: Monad m => [Term] -> Bool -> SExpr Pos -> m Term
loadTerm2 vars _ pos = loadTerm vars pos

-- Load term and check that it is well-formed.
loadTerm :: Monad m => [Term] -> SExpr Pos -> m Term
loadTerm vars (S pos s) =
    either fail return (loadLookup pos vars s)
loadTerm _ (Q _ t) =
    return (C t)
loadTerm vars (L pos (S _ s : l)) =
    case lookup s loadDispatch of
      Nothing -> fail (shows pos "Keyword " ++ s ++ " unknown")
      Just f -> f pos vars l
loadTerm _ x = fail (shows (annotation x) "Malformed term")

type LoadFunction m = Pos -> [Term] -> [SExpr Pos] -> m Term

loadDispatch :: Monad m => [(String, LoadFunction m)]
loadDispatch =
    [("pubk", loadPubk)
    ,("privk", loadPrivk)
    ,("invk", loadInvk)
    ,("ltk", loadLtk)
    ,("cat", loadCat)
    ,("enc", loadEnc)
    ,("hash", loadHash)
    ]

-- Atom constructors: pubk privk invk ltk

loadPubk :: Monad m => LoadFunction m
loadPubk _ vars [S pos s] =
    do
      t <- loadLookupName pos vars s
      return $ F Akey [F Pubk [I $ varId t]]
loadPubk _ vars [Q _ c, S pos s] =
    do
      t <- loadLookupName pos vars s
      return $ F Akey [F Pubk [C c, I $ varId t]]
loadPubk pos _ _ = fail (shows pos "Malformed pubk")

loadPrivk :: Monad m => LoadFunction m
loadPrivk _ vars [S pos s] =
    do
      t <- loadLookupName pos vars s
      return $ F Akey [F Invk [F Pubk [I $ varId t]]]
loadPrivk _ vars [Q _ c, S pos s] =
    do
      t <- loadLookupName pos vars s
      return $ F Akey [F Invk [F Pubk [C c, I $ varId t]]]
loadPrivk pos _ _ = fail (shows pos "Malformed privk")

loadInvk :: Monad m => LoadFunction m
loadInvk _ vars [S pos s] =
    do
      t <- loadLookupAkey pos vars s
      return $ F Akey [F Invk [I $ varId t]]
loadInvk pos _ _ = fail (shows pos "Malformed invk")

loadLtk :: Monad m => LoadFunction m
loadLtk _ vars [S pos s, S pos' s'] =
    do
      t <- loadLookupName pos vars s
      t' <- loadLookupName pos' vars s'
      return $ F Skey [F Ltk [I $ varId t, I $ varId t']]
loadLtk pos _ _ = fail (shows pos "Malformed ltk")

-- Term constructors: cat enc

loadCat :: Monad m => LoadFunction m
loadCat _ vars (l : ls) =
    do
      ts <- mapM (loadTerm vars) (l : ls)
      return $ foldr1 (\a b -> F Cat [a, b]) ts
loadCat pos _ _ = fail (shows pos "Malformed cat")

loadEnc :: Monad m => LoadFunction m
loadEnc pos vars (l : l' : ls) =
    do
      let (butLast, last) = splitLast l (l' : ls)
      t <- loadCat pos vars butLast
      t' <- loadTerm vars last
      return $ F Enc [t, t']
loadEnc pos _ _ = fail (shows pos "Malformed enc")

splitLast :: a -> [a] -> ([a], a)
splitLast x xs =
    loop [] x xs
    where
      loop z x [] = (reverse z, x)
      loop z x (y : ys) = loop (x : z) y ys

loadHash :: Monad m => LoadFunction m
loadHash _ vars (l : ls) =
   do
     ts <- mapM (loadTerm vars) (l : ls)
     return $ F Hash [foldr1 (\a b -> F Cat [a, b]) ts]
loadHash pos _ _ = fail (shows pos "Malformed hash")

-- Term specific displaying functions

newtype Context = Context [(Id, String)] deriving Show

displayVars :: Context -> [Term] -> [SExpr ()]
displayVars _ [] = []
displayVars ctx vars =
    let (v,t):pairs = map (displayVar ctx) vars in
    loop t [v] pairs
    where
      loop t vs [] = [L () (reverse (t:vs))]
      loop t vs ((v',t'):xs)
          | t == t' = loop t (v':vs) xs
          | otherwise = L () (reverse (t:vs)):loop t' [v'] xs

displayVar :: Context -> Term -> (SExpr (), SExpr ())
displayVar ctx (I x) = displaySortId "mesg" ctx x
displayVar ctx (F Text [I x]) = displaySortId "text" ctx x
displayVar ctx (F Data [I x]) = displaySortId "data" ctx x
displayVar ctx (F Name [I x]) = displaySortId "name" ctx x
displayVar ctx (F Skey [I x]) = displaySortId "skey" ctx x
displayVar ctx (F Akey [I x]) = displaySortId "akey" ctx x
displayVar ctx (D x) = displaySortId "node" ctx x
displayVar _ _ =
    C.assertError "Algebra.displayVar: term not a variable with its sort"

displaySortId :: String -> Context -> Id -> (SExpr (), SExpr ())
displaySortId sort ctx x = (displayId ctx x, S () sort)

displayId :: Context -> Id -> SExpr ()
displayId (Context ctx) x =
    case lookup x ctx of
      Nothing ->
          let msg = idName x ++ " in a display context" in
          C.assertError $ "Algebra.displayId: Cannot find variable " ++ msg
      Just name -> S () name

displayTerm :: Context -> Term -> SExpr ()
displayTerm ctx (I x) = displayId ctx x
displayTerm ctx (F Text [I x]) = displayId ctx x
displayTerm ctx (F Data [I x]) = displayId ctx x
displayTerm ctx (F Name [I x]) = displayId ctx x
displayTerm ctx (F Skey [I x]) = displayId ctx x
displayTerm ctx (F Skey [F Ltk [I x, I y]]) =
    L () [S () "ltk", displayId ctx x, displayId ctx y]
displayTerm ctx (F Akey [t]) =
    case t of
      I x -> displayId ctx x
      F Invk [I x] -> L () [S () "invk", displayId ctx x]
      F Pubk [I x] -> L () [S () "pubk", displayId ctx x]
      F Pubk [C c, I x] -> L () [S () "pubk", Q () c, displayId ctx x]
      F Invk [F Pubk [I x]] -> L () [S () "privk", displayId ctx x]
      F Invk [F Pubk [C c, I x]] ->
          L () [S () "privk", Q () c, displayId ctx x]
      _ -> C.assertError ("Algebra.displayAkey: Bad term " ++ show t)
displayTerm _ (C t) = Q () t
displayTerm ctx (F Cat [t0, t1]) =
    L () (S () "cat" : displayTerm ctx t0 : displayList ctx t1)
displayTerm ctx (F Enc [t0, t1]) =
    L () (S () "enc" : displayEnc ctx t0 t1)
displayTerm ctx (F Hash [t]) =
    L () (S () "hash" : displayList ctx t)
displayTerm ctx (D x) = displayId ctx x
displayTerm _ (P (z, i)) = L () [N () z, N () i]
displayTerm _ t = C.assertError ("Algebra.displayTerm: Bad term " ++ show t)

displayList :: Context -> Term -> [SExpr ()]
displayList ctx (F Cat [t0, t1]) = displayTerm ctx t0 : displayList ctx t1
displayList ctx t = [displayTerm ctx t]

displayEnc :: Context -> Term -> Term -> [SExpr ()]
displayEnc ctx (F Cat [t0, t1]) t = displayTerm ctx t0 : displayEnc ctx t1 t
displayEnc ctx t0 t1 = [displayTerm ctx t0, displayTerm ctx t1]

displayEnv :: Context -> Context -> Env -> [SExpr ()]
displayEnv ctx ctx' (Env r) =
    map (\(x, t) -> L () [displayTerm ctx x, displayTerm ctx' t]) r'
    where
      r' = map (\(x, t) -> (I x, inferSort t)) $ M.assocs r

-- displaySubst c s displays a substitution s in context c, where some
-- variables that occur in s might not be in c.  Enough sort
-- inference is performed so as to allow the extension of the context.
displaySubst :: Context -> Subst -> [SExpr ()]
displaySubst ctx s@(Subst r) =
    map (\(x, t) -> L () [displayTerm ctx' x, displayTerm ctx' t]) r'
    where
      r' = map (\(x, t) -> (I x, inferSort (substitute s t))) $ M.assocs r
      ctx' = foldl (\ctx (x, t) -> addToContext ctx [x, t]) ctx r'

inferSort :: Term -> Term
inferSort t@(F Invk _) = F Akey [t]
inferSort t@(F Pubk _) = F Akey [t]
inferSort t@(F Ltk _) = F Skey [t]
inferSort t = t

emptyContext :: Context
emptyContext = Context []

-- Generate names for output renaming as necessary.
-- Assumes the input is a list of term that are well-formed
addToContext :: Context -> [Term] -> Context
addToContext ctx u =
    foldl (foldVars varContext) ctx u

varContext :: Context -> Term -> Context
varContext ctx t =
    let x = varId t
        name = rootName $ idName x in
    if hasId ctx x then
        ctx
    else
        if hasName ctx name then
            extendContext ctx x (genName ctx name)
        else
            extendContext ctx x name

hasId :: Context -> Id -> Bool
hasId (Context ctx) id =
    maybe False (const True) (lookup id ctx)

hasName :: Context -> String -> Bool
hasName (Context ctx) name =
    maybe False (const True) (L.find ((name ==) . snd) ctx)

extendContext :: Context -> Id -> String -> Context
extendContext (Context ctx) x name =
    Context $ (x, name) : ctx

genName :: Context -> String -> String
genName ctx name =
    loop 0
    where
      root = '-' : reverse name
      loop :: Int -> String
      loop n =
          let name' = revapp root (show n) in
          if hasName ctx name' then
              loop (n + 1)
          else
              name'
      revapp [] s = s
      revapp (c : cs) s = revapp cs (c : s)

rootName :: String -> String
rootName name =
    noHyphen 0 name
    where
      noHyphen _ [] = name
      noHyphen i (c : s)
          | c == '-' = hyphen i (i + 1) s
          | otherwise = noHyphen (i + 1) s
      hyphen i _ [] = rootName $ take i name
      hyphen i j (c : s)
          | isDigit c  = hyphen i (j + 1) s
          | otherwise = noHyphen j (c : s)

instance C.Context Term Gen Subst Env Context where
    emptyContext = emptyContext
    addToContext = addToContext
    displayVars = displayVars
    displayTerm = displayTerm
    displayEnv = displayEnv
    displaySubst = displaySubst

instance C.Algebra Term Place Gen Subst Env Context
