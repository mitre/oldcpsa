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
import CPSA.Lib.Goal
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

-- Instance formulas are role predicates, parameter predicates, and
-- strand prec.
instForm :: (Pos, AForm t) -> Bool
instForm (_, RolePred _ _ _) = True
instForm (_, ParamPred _ _ _ _) = True
instForm (_, StrPrec _ _) = True
instForm _ = False

-- Make the instances from the instance predicates

mkInsts :: (Algebra t p g s e c, Monad m) => g -> Conj t ->
           m ([(t, Node)], g, [Instance t e])
mkInsts g as =
  do
    nri <- nodeRoleIndex as     -- Compute index and role of each node
    nss <- binNodes nri as      -- Collect nodes on the same strand
    (g, insts) <- foldInsts g as nri nss -- Construct instances
    return (nodeMap nri nss, g, insts) -- Construct node map for later use

type RoleIndex t = (Role t, Int)

-- Computes a map from nodes to role-index pairs
nodeRoleIndex :: (Eq t, Monad m) => Conj t -> m [(t, RoleIndex t)]
nodeRoleIndex as =
  foldM f [] as
  where
    f nri (pos, RolePred r i n) =
      case lookup n nri of
        Nothing -> return ((n, (r, i)) : nri)
        Just _ -> fail (shows pos
                        "Node occurs in more than one role predicate")
    f nri _ = return nri

-- Use this lookup when lookup must succeed.
nriLookup :: Eq t => t -> [(t, RoleIndex t)] -> RoleIndex t
nriLookup n nri =
  case lookup n nri of
    Just ri -> ri
    Nothing -> error "Characteristic.nriLookup: Bad lookup"

--- Use str-prec to collect the nodes on the same strand.  Check to
--- make sure the role associated with nodes is the same.
binNodes :: (Eq t, Monad m) => [(t, RoleIndex t)] -> Conj t -> m [[t]]
binNodes nri as =
  foldM f (map (\(x, _) -> [x]) nri) as
  where
    f nss (pos, StrPrec n n')
      | i >= i' || rname r /= rname r' =
        fail (shows pos "Bad str-prec")
      | otherwise = return $ merge n n' nss
      where
        (r, i) = nriLookup n nri
        (r', i') = nriLookup n' nri
    f nss _ = return nss

-- Merge two sets of nodes and delete the old sets
merge :: Eq t => t -> t -> [[t]] -> [[t]]
merge n n' nss =
  (ns ++ ns') : L.delete ns (L.delete ns' nss)
  where
    ns = findl n nss
    ns' = findl n' nss

-- Find a set containing node n
findl :: Eq t => t -> [[t]] -> [t]
findl n nss =
  case L.find (elem n) nss of
    Just ns -> ns
    Nothing -> error "Characteristic.findl: cannot find a node"

-- Construct instances
foldInsts :: (Algebra t p g s e c, Monad m) => g -> Conj t ->
             [(t, RoleIndex t)] -> [[t]] -> m (g, [Instance t e])
foldInsts g _ _ [] = return (g, [])
foldInsts g as nri (ns : nss) =
  do
    (g, inst) <- mkInst g as nri ns
    (g, insts) <- foldInsts g as nri nss
    return (g, inst : insts)

-- Construct an instance by extracting maplets from the parameter
-- predicates with nodes associated with the strand.
mkInst :: (Algebra t p g s e c, Monad m) => g -> Conj t ->
          [(t, RoleIndex t)] -> [t] -> m (g, Instance t e)
mkInst _ _ _ [] = error "Characteristic.mkInst: no nodes"
mkInst g as nri (n : ns)
  | h < 1 || h > length (rtrace r) = -- Checked by the the loader
    error "Character.mkInst: Bad height"
  | otherwise =
      do
        (g, env) <- foldM (mkMaplet r (n : ns)) (g, emptyEnv) as
        return (mkInstance g r env h)
  where
    (r, i) = nriLookup n nri
    -- The height (1 + max index)
    h = 1 + foldr f i ns
    f n i = max i (snd $ nriLookup n nri)

-- Add match from a maplet
mkMaplet :: (Algebra t p g s e c, Monad m) => Role t ->
            [t] -> (g, e) -> (Pos, AForm t) -> m (g, e)
mkMaplet role ns env (pos, ParamPred r v n t)
  | elem n ns =
    if rname role == rname r then -- Ensure role match the one
      case match v t env of       -- used to create instance
        env : _ -> return env
        [] -> fail (shows pos "Domain does not match range")
    else
      fail (shows pos
            "Role in parameter pred differs from role position pred")
mkMaplet _ _ env _ = return env

-- Generate a map from node variables to node constants.
nodeMap :: Eq t => [(t, RoleIndex t)] -> [[t]] -> [(t, Node)]
nodeMap nri nss =
  [ (n, (z, i)) |
    (z, ns) <- zip [0..] nss,
    n <- ns,
    let (_, i) = nriLookup n nri ]

-- Use this lookup when lookup must succeed, that is when loader makes
-- the check.
nMapLookup :: Eq t => t -> [(t, Node)] -> Node
nMapLookup n nmap =
  case lookup n nmap of
    Just n -> n
    Nothing -> error "Characteristic.nMapLookup: Bad lookup"

-- Create a skeleton given a list of instances

mkSkel :: (Algebra t p g s e c, Monad m) => Pos -> Prot t g ->
          [Goal t] -> [(t, Node)] -> g -> [Instance t e] ->
          Conj t -> [SExpr ()] -> m (Preskel t g s e)
mkSkel pos p goals nmap g insts as comment =
  do
    let o = foldr (mkPrec nmap) [] as
    let nr = foldr mkNon [] as
    let ar = foldr mkPnon [] as
    let ur = foldr mkUniq [] as
    let gr = foldr mkUgen [] as
    let decls = mkDcls nr ar ur gr
    let prios = []
    let k = mkPreskel g p goals insts o [] decls comment prios Nothing []
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

mkPrec :: Eq t => [(t, (Int, Int))] ->
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

checkUniqAt :: (Algebra t p g s e c, Monad m) => [(t, Node)] ->
               Preskel t g s e -> (Pos, AForm t) -> m ()
checkUniqAt nmap k (pos, UniqAt t n) =
  case lookup t $ korig k of
    Nothing -> fail (shows pos "Atom not unique at node")
    Just ns
      | elem (nMapLookup n nmap) ns -> return ()
      | otherwise -> fail (shows pos "Atom not unique at node")
checkUniqAt _ _ _ = return ()
