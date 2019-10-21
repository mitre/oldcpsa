-- Protocol data structures.

-- Copyright (c) 2009 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

{-# LANGUAGE CPP #-}

#if !(MIN_VERSION_base(4,13,0))
#define MonadFail Monad
#endif

module CPSA.Lib.Protocol (Event (..), evtTerms, evtMesgTerms, evtMap, evt,
    recvTerm, Trace, stripSync, tterms, originates,
    originationPos, gainedPos, genGainedPos, firstOccurs,
    Role, rname, rvars, rtrace, rdecls, rcomment, rsearch, generationPos,
    ridecls, mkRole, roleWellFormed,
    RoleDeclInst, RoleDeclInstList, RoleDeclaration, RoleDeclList,
    RoleDeclarations, rpriority, defaultPriority,
    AForm (..), NodeTerm, Goal (..),
    aFormOrder, aFreeVars, Rule (..),
    Prot, mkProt, pname, alg, pgen, roles, rules, listenerRole,
    varsAllAtoms, pcomment) where

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Maybe as M
import CPSA.Lib.Utilities
import CPSA.Lib.SExpr
import CPSA.Lib.Algebra
import CPSA.Lib.AlgebraLibrary
import CPSA.Lib.Declaration
import CPSA.Lib.State

{--
import CPSA.Lib.Debug
--}

-- Message events and traces

data Event t
    = In !t                      -- Inbound message
    | Out !t                     -- Outbound message
    | Sync (Tran t)              -- State synchronization
    deriving (Show, Eq, Ord)

-- Dispatch to function based on direction.
evt :: (t -> a) -> (t -> a) -> (Tran t -> a) -> Event t -> a
evt inDir outDir syncDir evt =
    case evt of
      In t -> inDir t
      Out t -> outDir t
      Sync t -> syncDir t

-- Extract the terms in an event.
evtTerms :: Event t -> [t]
evtTerms (In t) = [t]
evtTerms (Out t) = [t]
evtTerms (Sync t) = tranTerms t

-- Extract the message-passed terms in an event.
evtMesgTerms :: Event t -> [t]
evtMesgTerms (Sync _) = []
evtMesgTerms e = evtTerms e

-- Map the term in an event.
evtMap :: (t -> t) -> Event t -> Event t
evtMap f (In t) = In (f t)
evtMap f (Out t) = Out (f t)
evtMap f (Sync t) = Sync (mapTran f t)

-- Extract the reception term
recvTerm :: Event t -> Maybe t
recvTerm (In t) = Just t
recvTerm _ = Nothing

{-
-- Extract the transmission term
sendTerm :: Event t -> Maybe t
sendTerm (Out t) = Just t
sendTerm _ = Nothing
-}

-- Extract the term in an inbound event.
inbnd :: Event t -> Maybe t
inbnd (In t) = Just t
inbnd (Sync (Tran (Just t, _))) = Just t
inbnd _ = Nothing

-- Extract the term in an outbound event.
outbnd :: Event t -> Maybe t
outbnd (Out t) = Just t
outbnd (Sync (Tran (_, Just t))) = Just t
outbnd _ = Nothing

{-
-- Extract the term in an outbound event.
sync :: Event t -> Maybe (Tran t)
sync (Sync t) = Just t
sync _ = Nothing
-}

-- A trace is a list of events.  The terms in the trace are
-- stored in causal order.
type Trace t = [Event t]

stripSync :: Trace t -> Trace t
stripSync [] = []
stripSync (Sync _:c) = stripSync c
stripSync (e:c) = e:stripSync c

-- The set of terms in a trace.
tterms :: Eq t => Trace t -> [t]
tterms c =
  L.nub [t | evt <- c, t <- evtTerms evt]

tmesgterms :: Eq t => Trace t -> [t]
tmesgterms c =
  L.nub [t | evt <- c, t <- evtMesgTerms evt]

-- Is the term carried by an event, and is the first one outgoing?
originates :: Algebra t p g s e c => t -> Trace t -> Bool
originates t c = M.isJust (originationPos t c)

-- Loop through the events in a trace, looking for an inbound
-- message satisfying (intest1) or (intest2), in which case,
-- return Nothing or Just (height) respectively, or for an
-- outbound message satisfying (outtest1) or (outtest2), in
-- which case return Nothing or Just (height) respecitvely,
-- with Nothing as a default.
--
-- If both intest1 and intest2 are true, return Just (height).
-- If both outtest1 and outtest2 are true, return Just (height).
lookupPos :: Algebra t p g s e c => t -> (t -> t -> Bool) ->
             (t -> t -> Bool) -> (t -> t -> Bool) -> (t -> t -> Bool) ->
             Trace t -> Maybe Int
lookupPos t intest1 intest2 outtest1 outtest2 c =
    loop 0 c
    where
    loop _ [] = Nothing         -- Default to Nothing
    loop pos (evt : c) =
         if bail then inresult else result
         where
           bail = case (inbnd evt) of
             Nothing -> False
             Just t' -> (intest1 t t') || (intest2 t t')
           inresult = case (inbnd evt) of
             Nothing -> error ("protocol:lookupPos -- no result defined.")
             Just t' -> if (intest2 t t') then Just pos else Nothing
           result = case (outbnd evt) of
             Nothing -> loop (pos + 1) c
             Just t' -> if (outtest2 t t') then Just pos else
                       (if (outtest1 t t') then Nothing else loop (pos + 1) c)

-- At what position does a term originate in a trace?
originationPos :: Algebra t p g s e c => t ->
                  Trace t -> Maybe Int
originationPos t c =
    -- Looking for where t is first carried, if it is in an outbound message.
    lookupPos t carriedBy ftest ftest carriedBy c

-- At what position does a term generate in a trace?
generationPos :: Algebra t p g s e c => t ->
                  Trace t -> Maybe Int
generationPos t c =
    -- Looking for where t first occurs, if it is in an outbound message.
    lookupPos t constituent ftest ftest constituent c

-- At what position is a term acquired in a trace?
acquiredPos :: Algebra t p g s e c => t -> Trace t -> Maybe Int
acquiredPos t c =
    -- Looking for where t first occurs, if it is carried there and that is an
    -- inbound message
    lookupPos t occursIn carriedBy occursIn ftest c

-- At what position is a term obtained in a trace?
obtainedPos :: Algebra t p g s e c => t -> Trace t -> Maybe Int
obtainedPos t c =
    -- Looking for where t first occurs, if it occurs there and that is an
    -- inbound message
    lookupPos t occursIn occursIn occursIn ftest c

-- At what position is a term gained in a trace?
gainedPos :: Algebra t p g s e c => t ->
             Trace t -> Maybe Int
gainedPos t c =
    -- Looking for where t is first carried, if it is in an inbound message.
    lookupPos t ftest carriedBy carriedBy ftest c

-- At what position is a term gained in a trace with regards to constituent?
genGainedPos :: Algebra t p g s e c => t ->
             Trace t -> Maybe Int
genGainedPos t c =
    -- Looking for where t first occurs, if it is in an inbound message.
    lookupPos t ftest constituent constituent ftest c

ftest :: a -> b -> Bool
ftest = const (const False)

-- At what position do all of the variables in a term occur in a trace?
usedPos :: Algebra t p g s e c => t -> Trace t -> Maybe Int
usedPos t c =
    loop 0 (varsInTerms [t]) c
    where
      loop _ _ [] = Nothing
      loop pos vars (e : c) =
          let vars' = [ x | x <- vars, notElem x (varsInTerms (evtTerms e)) ] in
          case vars' of
            [] -> Just pos
            _ -> loop (pos + 1) vars' c

-- Roles store internal declarations only in ridecls.
-- Roles store a raw external declarations format.
data Role t = Role
    { rname :: !String,
      rvars :: ![t],            -- Set of role variables
                                -- Events in causal order
      rtrace :: ![Event t],
      -- Set of non-originating atoms, possibly with a trace length
      rdecls :: RoleDeclarations t,
      rpriority :: ![Int],      -- Priorities for each event
      rcomment :: [SExpr ()],   -- Comments from the input
      rsearch :: Bool, -- True when suggesting reverse test node search
      ridecls :: [(RoleDeclarations t, Int)]}
    deriving Show

defaultPriority :: Int
defaultPriority = 5

firstOccurs :: Algebra t p g s e c => t -> Role t -> Maybe Int
firstOccurs v r = firstOccursAt v (rtrace r)

-- | Compute the index of the first event at which the given variable
-- occurs in a trace.
firstOccursAt :: Algebra t p g s e c => t -> Trace t -> Maybe Int
firstOccursAt t c =
    loop 0 c
    where
      loop _ [] = Nothing
      loop i (e : c)
          | any (occursIn t) (evtTerms e) = Just i
          | otherwise = loop (i + 1) c

-- The empty role name is used with listener strands.  All roles in a
-- protocol must have a name with more than one character.

-- The lists vars, non, pnon, and unique are sets and should never
-- contain duplicate terms.

-- Create a role
mkRole :: Algebra t p g s e c => String -> [t] -> Trace t ->
          RoleDeclList t -> [(Int, Int)] -> [SExpr ()] -> Bool -> Role t
mkRole name vars trace dlist priority comment rev =
    Role { rname = name,
           rvars = L.nub vars,  -- Every variable here must
           rtrace = trace,      --  occur in the trace.
           rdecls = mkDecls dlist',
           rcomment = comment,
           rpriority = addDefaultPrio priority,
           ridecls = concatMap mkRidecls $ nats(length(trace)),
           rsearch = rev
         }
    where
      dlist' = expandDecls vars trace dlist
      rothersintro = map addOtherOrig $ dlist'
      restrict i dlist = filter (\x -> not (null (snd x))) (select i dlist)
      select i dlist = map (\ (n, dis) -> (n, map fst
                                              (filter (\ (_,j)->i==j) dis))) dlist
      mkRidecls i =
        let os = restrict i rothersintro in
        [(mkDecls os, i)]
      addOtherOrig (n,ds) = (n, map addDeclOrig ds)
      addDeclOrig dinst
            | null ls && null ts = (dinst,0)
            | any (==Nothing) tps =
              error "Protocol.mkRole: Declaration variables not in trace"
            | any (\i -> (i < 0 || i >= length trace)) ls =
              error "Protocol.mkRole: Declaration height not valid"
            | null ts = (dinst,maximum ls)
            | null ls = (dinst,maximum (map M.fromJust tps))
            | otherwise = (dinst,max (maximum ls) (maximum (map M.fromJust tps)))
            where
               ts = dterms dinst
               ls = dlocs dinst
               tps = map (\t -> usedPos t trace) ts
      addDefaultPrio priority =
          map f (nats $ length trace)
          where
            f n =
              case lookup n priority of
                Nothing -> defaultPriority
                Just p -> p

-- expandDecls: transform declarations from file format to internal format.
-- This means to (1) add nodes to uniq-orig and uniq-gen declarations (throwing
-- an error if there is no appropriate node), (2) adding ind-zero declarations for
-- generating exponents not declared uniq-gen, and (3) adding ind-zero-in
-- declarations for exponents before a generation point of a uniq-gen exponent.
expandDecls :: Algebra t p g s e c => [t] -> Trace t ->
               RoleDeclList t -> RoleDeclList t
expandDecls _ trace dlist =
  (map addUniqs dlist) ++ indzeros
  where
    -- add locations to uniq-orig and uniq-gen declarations
    addUniqs ("uniq-orig", ds) = ("uniq-orig", map addUniqOrig ds)
    addUniqs ("uniq-gen", ds) = ("uniq-gen", map addUniqGen ds)
    -- leave other declarations alone
    addUniqs (n, ds) = (n, ds)
    indzeros = indz_ins
    -- ind-zero-in declarations: for numeric subterms occurring earlier than
    -- the generation point of uniq-gen numeric variables.
    indz_ins = case concatMap indz_ininsts (filter isNum (dkuniqgen decls)) of
      [] -> []
      insts -> [("absent", insts)]
    indz_ininsts v =
      case generationPos (v) trace of
        Nothing -> error "Protocol.mkRole: Atom does not generate"
        Just p -> indz_ininsts_var v p
    -- ind-zero instances for a specific variable v that generates at height p
    indz_ininsts_var v p =
      map (\t -> (declInst [v,t] [])) (numsUpTo p)
    -- returns a list of numeric subterms of all messages prior to height p.
    numsUpTo p =
      concatMap S.toList $ map subNums $ concatMap evtTerms $ take p trace
    -- avoid = avoidTerms decls
    decls = mkDecls dlist
    addUniqOrig dinst
      | null $ dterms dinst =
        error "Protocol.mkRole: malformed uniq-orig declaration"
      | otherwise =
        case originationPos (t) trace of
          Just p -> (declInst [t] [p])
          Nothing -> error "Protocol.mkRole: Atom does not originate"
      where
        t = head $ dterms dinst
    addUniqGen dinst
      | null $ dterms dinst =
        error "Protocol.mkRole: malformed uniq-gen declaration"
      | otherwise =
        case generationPos (t) trace of
          Just p -> (declInst [t] [p])
          Nothing -> error "Protocol.mkRole: Atom does not generate"
      where
        t = head $ dterms dinst

-- A role is well formed if all non-base variables are receive bound,
-- each atom declared to be uniquely-originating originates in
-- the trace, and every variable that occurs in each atom
-- declared to be non-originating occurs in some term in the trace,
-- and the atom must never be carried by any term in the trace.
roleWellFormed :: (MonadFail m, Algebra t p g s e c) => Role t -> m ()
roleWellFormed role =
    do
      failwith "[ASSERT FAILED] invalid declaration"
                   $ all declarationValid (map fst $ declarationTags $ rdecls role)
      mapM_ nonCheck $ dknon $ rdecls role
      mapM_ uniqueCheck $ dkunique $ rdecls role
      let chk = declCheck $ rdecls role
      failwith (snd chk) (fst chk)
      mapM_ acqVarCheck $ rvars role
      mapM_ obtVarCheck $ rvars role
      failwith "role trace is a prefix of a listener"
                   $ notListenerPrefix $ rtrace role
    where
      declarationValid tag = all declInstValid $ tagDecls tag $ rdecls role
      declInstValid dinst =
          (all lenCheck $ dlocs dinst) && (varSubset (dterms dinst) terms)
      terms = tterms (rtrace role)
      nonCheck t =
          failwith (showString "non-orig " $ showst t " carried")
                       $ all (not . carriedBy t) (tmesgterms (rtrace role))
      lenCheck n = (n >= 0 && n < (length $ rtrace role))

      uniqueCheck t =
          failwith (showString "[ASSERT FAILED] uniq-orig " $ showst  t " doesn't originate")
                       $ originates t (rtrace role)
      acqVarCheck v =
          failwith (showString "variable " $ showst v " not acquired")
                       $ not (isAcquiredVar v) ||
                         M.isJust (acquiredPos v (rtrace role))
      obtVarCheck v =
          failwith (showString "variable " $ showst v " not obtained")
                       $ not (isObtainedVar v) ||
                         M.isJust (obtainedPos v (rtrace role))

showst :: Algebra t p g s e c => t -> ShowS
showst t =
    shows $ displayTerm (addToContext emptyContext [t]) t

-- Ensure a trace is not a prefix of a listener
notListenerPrefix :: Algebra t p g s e c => Trace t -> Bool
notListenerPrefix (In t : Out t' : _) | t == t' = False
notListenerPrefix _ = True

-- Security Goals

-- Syntax for the atomic formulas
data AForm t
  = Length (Role t) t Int
  | Param (Role t) t Int t t   -- role param first-height strand value
  | Prec (NodeTerm t) (NodeTerm t)
  | LeadsTo (NodeTerm t) (NodeTerm t)
  | Non t
  | Pnon t
  | Uniq t
  | UniqAt t (NodeTerm t)
  | UgenAt t (NodeTerm t)
  | Ugen t
  | AFact String [t]
  | Equals t t
  deriving Show

type NodeTerm t = (t, Int)

data Goal t
  = Goal { uvars :: [t],          -- Universally quantified variables
           antec :: [AForm t],    -- Antecedent
           -- Consequent with existentially quantified variables
           consq :: [([t], [AForm t])],
           concl :: [[AForm t]] } -- Conclusion
  deriving Show

-- Ordering used to sort by constructor order.
aFormOrder :: AForm t -> AForm t -> Ordering
aFormOrder (Length _ _ _) (Length _ _ _) = EQ
aFormOrder (Length _ _ _) (Param _ _ _ _ _) = LT
aFormOrder (Length _ _ _) (Prec _ _) = LT
aFormOrder (Length _ _ _) (LeadsTo _ _) = LT
aFormOrder (Length _ _ _) (Non _) = LT
aFormOrder (Length _ _ _) (Pnon _) = LT
aFormOrder (Length _ _ _) (Uniq _) = LT
aFormOrder (Length _ _ _) (UniqAt _ _) = LT
aFormOrder (Length _ _ _) (UgenAt _ _) = LT
aFormOrder (Length _ _ _) (Ugen _) = LT
aFormOrder (Length _ _ _) (AFact _ _) = LT
aFormOrder (Length _ _ _) (Equals _ _) = LT
aFormOrder (Param _ _ _ _ _) (Length _ _ _) = GT
aFormOrder (Param _ _ _ _ _) (Param _ _ _ _ _) = EQ
aFormOrder (Param _ _ _ _ _) (Prec _ _) = LT
aFormOrder (Param _ _ _ _ _) (LeadsTo _ _) = LT
aFormOrder (Param _ _ _ _ _) (Non _) = LT
aFormOrder (Param _ _ _ _ _) (Pnon _) = LT
aFormOrder (Param _ _ _ _ _) (Uniq _) = LT
aFormOrder (Param _ _ _ _ _) (UniqAt _ _) = LT
aFormOrder (Param _ _ _ _ _) (UgenAt _ _) = LT
aFormOrder (Param _ _ _ _ _) (Ugen _) = LT
aFormOrder (Param _ _ _ _ _) (AFact _ _) = LT
aFormOrder (Param _ _ _ _ _) (Equals _ _) = LT
aFormOrder (Prec _ _) (Length _ _ _) = GT
aFormOrder (Prec _ _) (Param _ _ _ _ _) = GT
aFormOrder (Prec _ _) (Prec _ _) = EQ
aFormOrder (Prec _ _) (LeadsTo _ _) = LT
aFormOrder (Prec _ _) (Non _) = LT
aFormOrder (Prec _ _) (Pnon _) = LT
aFormOrder (Prec _ _) (Uniq _) = LT
aFormOrder (Prec _ _) (UniqAt _ _) = LT
aFormOrder (Prec _ _) (UgenAt _ _) = LT
aFormOrder (Prec _ _) (Ugen _) = LT
aFormOrder (Prec _ _) (AFact _ _) = LT
aFormOrder (Prec _ _) (Equals _ _) = LT
aFormOrder (LeadsTo _ _) (Length _ _ _) = GT
aFormOrder (LeadsTo _ _) (Param _ _ _ _ _) = GT
aFormOrder (LeadsTo _ _) (Prec _ _) = GT
aFormOrder (LeadsTo _ _) (LeadsTo _ _) = EQ
aFormOrder (LeadsTo _ _) (Non _) = LT
aFormOrder (LeadsTo _ _) (Pnon _) = LT
aFormOrder (LeadsTo _ _) (Uniq _) = LT
aFormOrder (LeadsTo _ _) (UniqAt _ _) = LT
aFormOrder (LeadsTo _ _) (UgenAt _ _) = LT
aFormOrder (LeadsTo _ _) (Ugen _) = LT
aFormOrder (LeadsTo _ _) (AFact _ _) = LT
aFormOrder (LeadsTo _ _) (Equals _ _) = LT
aFormOrder (Non _) (Length _ _ _) = GT
aFormOrder (Non _) (Param _ _ _ _ _) = GT
aFormOrder (Non _) (Prec _ _) = GT
aFormOrder (Non _) (LeadsTo _ _) = GT
aFormOrder (Non _) (Non _) = EQ
aFormOrder (Non _) (Pnon _) = LT
aFormOrder (Non _) (Uniq _) = LT
aFormOrder (Non _) (UniqAt _ _) = LT
aFormOrder (Non _) (UgenAt _ _) = LT
aFormOrder (Non _) (Ugen _) = LT
aFormOrder (Non _) (AFact _ _) = LT
aFormOrder (Non _) (Equals _ _) = LT
aFormOrder (Pnon _) (Length _ _ _) = GT
aFormOrder (Pnon _) (Param _ _ _ _ _) = GT
aFormOrder (Pnon _) (Prec _ _) = GT
aFormOrder (Pnon _) (LeadsTo _ _) = GT
aFormOrder (Pnon _) (Non _) = GT
aFormOrder (Pnon _) (Pnon _) = EQ
aFormOrder (Pnon _) (Uniq _) = LT
aFormOrder (Pnon _) (UniqAt _ _) = LT
aFormOrder (Pnon _) (UgenAt _ _) = LT
aFormOrder (Pnon _) (Ugen _) = LT
aFormOrder (Pnon _) (AFact _ _) = LT
aFormOrder (Pnon _) (Equals _ _) = LT
aFormOrder (Uniq _) (Length _ _ _) = GT
aFormOrder (Uniq _) (Param _ _ _ _ _) = GT
aFormOrder (Uniq _) (Prec _ _) = GT
aFormOrder (Uniq _) (LeadsTo _ _) = GT
aFormOrder (Uniq _) (Non _) = GT
aFormOrder (Uniq _) (Pnon _) = GT
aFormOrder (Uniq _) (Uniq _) = EQ
aFormOrder (Uniq _) (UniqAt _ _) = LT
aFormOrder (Uniq _) (UgenAt _ _) = LT
aFormOrder (Uniq _) (Ugen _) = LT
aFormOrder (Uniq _) (AFact _ _) = LT
aFormOrder (Uniq _) (Equals _ _) = LT
aFormOrder (UniqAt _ _) (Length _ _ _) = GT
aFormOrder (UniqAt _ _) (Param _ _ _ _ _) = GT
aFormOrder (UniqAt _ _) (Prec _ _) = GT
aFormOrder (UniqAt _ _) (LeadsTo _ _) = GT
aFormOrder (UniqAt _ _) (Non _) = GT
aFormOrder (UniqAt _ _) (Pnon _) = GT
aFormOrder (UniqAt _ _) (Uniq _) = GT
aFormOrder (UniqAt _ _) (UniqAt _ _) = EQ
aFormOrder (UniqAt _ _) (UgenAt _ _) = LT
aFormOrder (UniqAt _ _) (Ugen _) = LT
aFormOrder (UniqAt _ _) (AFact _ _) = LT
aFormOrder (UniqAt _ _) (Equals _ _) = LT
aFormOrder (UgenAt _ _) (Length _ _ _) = GT
aFormOrder (UgenAt _ _) (Param _ _ _ _ _) = GT
aFormOrder (UgenAt _ _) (Prec _ _) = GT
aFormOrder (UgenAt _ _) (LeadsTo _ _) = GT
aFormOrder (UgenAt _ _) (Non _) = GT
aFormOrder (UgenAt _ _) (Pnon _) = GT
aFormOrder (UgenAt _ _) (Uniq _) = GT
aFormOrder (UgenAt _ _) (UniqAt _ _) = GT
aFormOrder (UgenAt _ _) (UgenAt _ _) = EQ
aFormOrder (UgenAt _ _) (Ugen _) = LT
aFormOrder (UgenAt _ _) (AFact _ _) = LT
aFormOrder (UgenAt _ _) (Equals _ _) = LT
aFormOrder (Ugen _) (Length _ _ _) = GT
aFormOrder (Ugen _) (Param _ _ _ _ _) = GT
aFormOrder (Ugen _) (Prec _ _) = GT
aFormOrder (Ugen _) (LeadsTo _ _) = GT
aFormOrder (Ugen _) (Non _) = GT
aFormOrder (Ugen _) (Pnon _) = GT
aFormOrder (Ugen _) (Uniq _) = GT
aFormOrder (Ugen _) (UniqAt _ _) = GT
aFormOrder (Ugen _) (UgenAt _ _) = GT
aFormOrder (Ugen _) (Ugen _) = EQ
aFormOrder (Ugen _) (AFact _ _) = LT
aFormOrder (Ugen _) (Equals _ _) = LT
aFormOrder (AFact _ _) (Length _ _ _) = GT
aFormOrder (AFact _ _) (Param _ _ _ _ _) = GT
aFormOrder (AFact _ _) (Prec _ _) = GT
aFormOrder (AFact _ _) (LeadsTo _ _) = GT
aFormOrder (AFact _ _) (Non _) = GT
aFormOrder (AFact _ _) (Pnon _) = GT
aFormOrder (AFact _ _) (Uniq _) = GT
aFormOrder (AFact _ _) (UniqAt _ _) = GT
aFormOrder (AFact _ _) (UgenAt _ _) = GT
aFormOrder (AFact _ _) (Ugen _) = GT
aFormOrder (AFact _ _) (AFact _ _) = EQ
aFormOrder (AFact _ _) (Equals _ _) = LT
aFormOrder (Equals _ _) (Length _ _ _) = GT
aFormOrder (Equals _ _) (Param _ _ _ _ _) = GT
aFormOrder (Equals _ _) (Prec _ _) = GT
aFormOrder (Equals _ _) (LeadsTo _ _) = GT
aFormOrder (Equals _ _) (Non _) = GT
aFormOrder (Equals _ _) (Pnon _) = GT
aFormOrder (Equals _ _) (Uniq _) = GT
aFormOrder (Equals _ _) (UniqAt _ _) = GT
aFormOrder (Equals _ _) (UgenAt _ _) = GT
aFormOrder (Equals _ _) (Ugen _) = GT
aFormOrder (Equals _ _) (AFact _ _) = GT
aFormOrder (Equals _ _) (Equals _ _) = EQ

aFreeVars :: Algebra t p g s e c => [t] -> AForm t -> [t]
aFreeVars vars (Length _ z _) = addVars vars z
aFreeVars vars (Param _ _ _ z t) = addVars (addVars vars z) t
aFreeVars vars (Prec (x, _) (y, _)) = addVars (addVars vars x) y
aFreeVars vars (LeadsTo (x, _) (y, _)) = addVars (addVars vars x) y
aFreeVars vars (Non t) = addVars vars t
aFreeVars vars (Pnon t) = addVars vars t
aFreeVars vars (Uniq t) = addVars vars t
aFreeVars vars (UniqAt t (z, _)) = addVars (addVars vars t) z
aFreeVars vars (UgenAt t (z, _)) = addVars (addVars vars t) z
aFreeVars vars (Ugen t) = addVars vars t
aFreeVars vars (AFact _ ft) = foldl addVars vars ft
aFreeVars vars (Equals x y) = addVars (addVars vars x) y

data Rule t
  = Rule { rlname :: String,    -- Name of rule
           rlgoal :: Goal t,    -- Sentence
           rlcomment :: [SExpr ()] }
    deriving Show

-- Protocols

data Prot t g
    = Prot { pname :: !String,  -- Name of the protocol
             alg :: !String,    -- Name of the algebra
             pgen :: !g,      -- Initial variable generator
             roles :: ![Role t], -- Non-listener roles of a protocol
             listenerRole :: Role t,
             rules :: ![Rule t],  -- Protocol rules
             varsAllAtoms :: !Bool,   -- Are all role variables atoms?
             pcomment :: [SExpr ()] }  -- Comments from the input
    deriving Show

-- Callers should ensure every role has a distinct name.
mkProt :: Algebra t p g s e c => String -> String ->
          g -> [Role t] -> Role t -> [Rule t] -> [SExpr ()] -> Prot t g
mkProt name alg gen roles lrole rules comment =
    Prot { pname = name, alg = alg, pgen = gen, roles = roles,
           listenerRole = lrole, rules = rules, pcomment = comment,
           varsAllAtoms = all roleVarsAllAtoms roles }
    where
      roleVarsAllAtoms role = all isAtom (rvars role)

type RoleDeclInst t = DeclInst t Int
type RoleDeclInstList t = DeclInstList t Int
type RoleDeclaration t = Declaration t Int
type RoleDeclList t = DeclList t Int
type RoleDeclarations t = Declarations t Int
