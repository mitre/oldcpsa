-- Makes the characteristic skeleton of a security goal

-- Copyright (c) 2015 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module CPSA.Lib.Characteristic (Conj, characteristic) where

import Control.Monad
import qualified Data.List as L
import CPSA.Lib.SExpr
import CPSA.Lib.Algebra
import CPSA.Lib.Declaration
import CPSA.Lib.Protocol
import CPSA.Lib.Strand

{--
import System.IO.Unsafe
z :: Show a => a -> b -> b
z x y = unsafePerformIO (print x >> return y)
--}

type Conj t = [(Pos, AForm t)]

-- Entry point.  Takes a position, a protocol, a variable generator, a
-- goal, and a skeleton comment and makes a skeleton or fails.  This
-- function extracts the anecedent and univesally quantified variable.
characteristic :: (Algebra t p g s e c, Monad m) => Pos -> Prot t g ->
                  [Goal t] -> g -> Conj t -> [SExpr ()] -> m (Preskel t g s e)
characteristic pos prot goals g antec comment =
  equalsForm pos prot goals g antec comment

-- Checks for equals in an antecedent and fails if it finds one.  One
-- could use unification to solve the equality, and then apply the
-- result to the remaining parts of the formula.
equalsForm :: (Algebra t p g s e c, Monad m) => Pos -> Prot t g ->
              [Goal t] -> g -> Conj t -> [SExpr ()] -> m (Preskel t g s e)
equalsForm pos _ _ _ as _ | any isEquals as =
  fail (shows pos "Equals not allowed in antecedent")
equalsForm pos prot goals g as comment =
  splitForm pos prot goals g as comment

isEquals :: (Pos, AForm t) -> Bool
isEquals (_, Equals _ _) = True
isEquals _ = False

-- Split the formula into instance formulas and skeleton formulas.
-- The instance formulas are used to generate the skeleton's
-- instances, and the skeleton formulas generate the rest.  Make the
-- instances, and then make the rest.
splitForm :: (Algebra t p g s e c, Monad m) => Pos -> Prot t g ->
             [Goal t] -> g -> Conj t -> [SExpr ()] -> m (Preskel t g s e)
splitForm pos prot goals g as comment =
  do
    (nmap, g, insts) <- mkInsts g is
    mkSkel pos prot goals nmap g insts ks comment
  where                         -- is is the instance formulas and
    (is, ks) = L.partition instForm as -- ks is the skeleton formulas

-- Instance formulas are length predicates and parameter predicates.
instForm :: (Pos, AForm t) -> Bool
instForm (_, Length _ _ _) = True
instForm (_, Param _ _ _ _ _) = True
instForm _ = False

-- Make the instances from the instance predicates

mkInsts :: (Algebra t p g s e c, Monad m) => g -> Conj t ->
           m ([(t, Sid)], g, [Instance t e])
mkInsts g as =
  do
    srl <- strdRoleLength as    -- Compute role-length of each strand
    (g, insts) <- foldInsts g as srl -- Construct instances
    let strdMap = zip (map fst srl) [0..] -- Construct strand map
    return (strdMap, g, insts) -- Construct node map for later use

-- Computes a map from strands to roles and lengths
strdRoleLength :: (Algebra t p g s e c, Monad m) =>
                  Conj t -> m [(t, (Role t, Int))]
strdRoleLength as =
  foldM f [] as
  where
    f srl (pos, Length r z h) =
      case lookup z srl of
        Nothing -> return ((z, (r, h)) : srl)
        Just (r', h')
          | rname r' /= rname r ->
            fail (shows pos
                  "Strand occurs in more than one role length atom")
          | h <= h' -> return srl -- Use original binding
          | otherwise -> return ((z, (r, h)) : srl)
    f srl _ = return srl

-- Construct instances
foldInsts :: (Algebra t p g s e c, Monad m) =>
             g -> Conj t -> [(t, (Role t, Int))] ->
             m (g, [Instance t e])
foldInsts g _ [] = return (g, [])
foldInsts g as ((z, (r, h)) : srs) =
  do
    (g, inst) <- mkInst g as z r h
    (g, insts) <- foldInsts g as srs
    return (g, inst : insts)

-- Construct an instance by extracting maplets from the parameter
-- predicates associated with the strand.
mkInst :: (Algebra t p g s e c, Monad m) =>
          g -> Conj t -> t -> Role t -> Int -> m (g, Instance t e)
mkInst g as z r h =
  do
    (g, env) <- foldM (mkMaplet r z) (g, emptyEnv) as
    return (mkInstance g r env h)

-- Add match from a maplet
mkMaplet :: (Algebra t p g s e c, Monad m) =>
            Role t -> t -> (g, e) ->
            (Pos, AForm t) -> m (g, e)
mkMaplet role z env (pos, Param r v _ z' t)
  | z == z' =
    if rname role == rname r then -- Ensure role matches the one
      case match v t env of       -- used to create instance
        env : _ -> return env
        [] -> fail (shows pos "Domain does not match range")
    else
      fail (shows pos
            "Role in parameter pred differs from role position pred")
mkMaplet _ _ env _ = return env

-- Use this lookup when lookup must succeed, that is when loader makes
-- the check.
nMapLookup :: Eq t => (t, Int) -> [(t, Sid)] -> Node
nMapLookup (z, i) nmap =
  case lookup z nmap of
    Just s -> (s, i)
    Nothing -> error "Characteristic.nMapLookup: Bad lookup"

-- Create a skeleton given a list of instances

mkSkel :: (Algebra t p g s e c, Monad m) => Pos -> Prot t g ->
          [Goal t] -> [(t, Sid)] -> g -> [Instance t e] ->
          Conj t -> [SExpr ()] -> m (Preskel t g s e)
mkSkel pos p goals nmap g insts as comment =
  do
    let o = foldr (mkPrec nmap) [] as
    let nr = foldr mkNon [] as
    let ar = foldr mkPnon [] as
    let ur = foldr mkUniq [] as
    let gr = foldr mkUgen [] as
    let decls = mkDcls nr ar ur gr
    let fs = foldr (mkFact nmap) [] as
    let prios = []
    let k = mkPreskel g p goals insts o [] decls fs comment prios Nothing []
    mapM_ (checkUniqAt nmap k) as
    case termsWellFormed $ (termsInDlist decls) ++ kterms k of
      False -> fail (shows pos "Terms in skeleton not well formed")
      True -> return ()
    case verbosePreskelWellFormed k of
      Right () -> return k
      Left msg -> fail $ shows pos
                  $ showString "Skeleton not well formed: " msg
  where
    termsInDlist olist = concat $ map dterms (concatMap snd olist)

mkPrec :: Eq t => [(t, Sid)] ->
          (Pos, AForm t) -> [Pair] -> [Pair]
mkPrec nmap (_, Prec n n') o =
  (nMapLookup n nmap, nMapLookup n' nmap) : o
mkPrec _ _ o = o

mkDcls :: [t] -> [t] -> [t] -> [t] -> SkelDeclList t
mkDcls nr ar ur gr =
  [("non-orig", map simpleDInst nr), ("pen-non-orig", map simpleDInst ar),
   ("uniq-orig", map simpleDInst ur), ("uniq-gen", map simpleDInst gr)]
  where
    simpleDInst t = declInst [t] []

mkNon :: (Pos, AForm t) -> [t] -> [t]
mkNon (_, Non t) ts = t : ts
mkNon _ ts = ts

mkPnon :: (Pos, AForm t) -> [t] -> [t]
mkPnon (_, Pnon t) ts = t : ts
mkPnon _ ts = ts

mkUniq :: (Pos, AForm t) -> [t] -> [t]
mkUniq (_, Uniq t) ts = t : ts
mkUniq (_, UniqAt t _) ts = t : ts
mkUniq _ ts = ts

mkUgen :: (Pos, AForm t) -> [t] -> [t]
mkUgen (_, Ugen t) ts = t : ts
mkUgen (_, UgenAt t _) ts = t : ts
mkUgen _ ts = ts

mkFact :: Eq t => [(t, Sid)] -> (Pos, AForm t) -> [Fact t] -> [Fact t]
mkFact nmap (_, AFact name fs) ts =
  Fact name (map f fs) : ts
  where
    f t =
      case lookup t nmap of
        Just s -> FSid s
        Nothing -> FTerm t
mkFact _ _ ts = ts

checkUniqAt :: (Algebra t p g s e c, Monad m) => [(t, Sid)] ->
               Preskel t g s e -> (Pos, AForm t) -> m ()
checkUniqAt nmap k (pos, UniqAt t n) =
  case lookup t $ korig k of
    Nothing -> fail (shows pos "Atom not unique at node")
    Just ns
      | elem (nMapLookup n nmap) ns -> return ()
      | otherwise -> fail (shows pos "Atom not unique at node")
checkUniqAt _ _ _ = return ()
