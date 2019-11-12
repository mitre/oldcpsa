-- Instance and preskeleton data structures and support functions.

-- Copyright (c) 2009 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

{-# LANGUAGE CPP #-}

#if !(MIN_VERSION_base(4,13,0))
#define MonadFail Monad
#endif

module CPSA.Lib.Strand (Instance, mkInstance, bldInstance, mkListener,
    role, env, trace, height, listenerTerm, Sid, Node, mkPreskel,
    firstSkeleton, Pair, Preskel, gen, protocol, kgoals, insts,
    orderings, leadsto, kfacts, useFactVars,
    pov, korig, kcomment, nstrands, kvars, kpriority, kpriorities,
    strandids, kterms, avoid, preskelWellFormed, verbosePreskelWellFormed,
    Strand, inst, nodes, Vertex, preds, event, kabsent,
    graphNode, strands, vertex, Gist, gist, isomorphic, contract, augment,
    addListener, addBaseListener, addAbsence, isNodePrecur,
    Cause (..), Direction (..), Method (..), Operation (..),
    operation, SkelDeclInst, SkelDeclInstList, SkelDeclaration,
    SkelDeclList, SkelDeclarations, priority,
    prob, homomorphism, toSkeleton, generalize, collapse, decls, sat,
    FTerm (..), Fact (..), simplify, rewrite) where

import Control.Monad
-- import System.IO.Error (ioeGetErrorString)
import qualified Data.List as L
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Maybe as M
import CPSA.Lib.Utilities
import CPSA.Lib.SExpr
import CPSA.Lib.Algebra
import CPSA.Lib.AlgebraLibrary
import CPSA.Lib.Declaration
import CPSA.Lib.State
import CPSA.Lib.Protocol

-- Debugging support (Uncomment below)
--import CPSA.Lib.Debug

{-- Additional debugging support
zi :: Algebra t p g s e c => Instance t e -> String
zi inst =
    show (map f e)
    where
      domain = rvars (role inst)
      e = reify domain (env inst)
      range = map snd e
      f (x, t) = (displayTerm (context domain) x,
                  displayTerm (context range) t)
      context ts = addToContext emptyContext ts

zs :: Algebra t p g s e c => Set t -> String
zs s =
    show $ map (displayTerm (addToContext emptyContext ts)) ts
    where
      ts = S.toList s
-- Also see showst
--}

-- Compile time switches for expermentation.

-- Enable thinning, else use pruning.
useThinning :: Bool
useThinning = True -- False

-- Do not do multistrand thinning.
useSingleStrandThinning :: Bool
useSingleStrandThinning = False -- True

-- Enable de-origination.
-- Don't use de-origination without thinning although you may want to
-- use thinning without de-origination.
useDeOrigination :: Bool
useDeOrigination = False -- useThinning

-- Enable variables in facts that do not occur elsewhere in skeletons
useFactVars :: Bool
useFactVars = False -- True

-- Sanity check: ensure no role variable occurs in a skeleton.
useCheckVars :: Bool
useCheckVars = False

-- Instances and Strand Identifiers

-- An Instance is an instance of a role, in the sense that each
-- variable in the role's trace has been replaced by a term.  The
-- trace of the instance might be shorter that the role's trace, but
-- only truncating nodes from the end of the trace.

-- A preskeleton stores its strands as a ordered sequence of
-- instances.  A strand is the position of an instance in the
-- sequence.  Duplicates are allowed in the sequence, as two strands
-- can be instantiated from the same role in the same way.

type Sid = Int                  -- Strand Identifier

data Instance t e = Instance
    { role :: Role t,           -- Role from which this was
                                -- instantiated

      env :: !e,                -- The environment used to build this
                                -- instance's trace from its role's
                                -- trace

      trace :: ![Event t], -- Instance's trace
      height :: !Int }          -- Height of the instance
    deriving Show

-- Create a fresh instance of the given height.  The environment
-- specifies how to map some variables in the role's trace.  Unmapped
-- variables are instantiated with fresh variables to avoid naming
-- conflicts.
mkInstance :: Algebra t p g s e c => g -> Role t ->
              e -> Int -> (g, Instance t e)
mkInstance gen role env height =
    let trace = rtrace role
        rheight = length trace in
    if height < 1 || height > rheight then
        error "Strand.mkInstance: Bad strand height"
    else
        let (gen', env') = grow (rvars role) gen env
            trace' = map (evtMap $ instantiate env') (take height trace) in
        -- Ensure every variable in the range of the environment
        -- occurs in the trace.
        case bldInstance role trace' gen' of
          (gen'', inst) : _ -> (gen'', inst)
          [] -> error "Strand.mkInstance: Not an instance"

-- For each term that matches itself in the environment, extend the
-- mapping so that the term maps to one with a fresh set of variables.
-- It is an error if a variable in one of the terms is explicitly
-- mapped to itself in the initial environment.
grow :: Algebra t p g s e c => [t] -> g -> e -> (g, e)
grow [] gen env = (gen, env)
grow (t : ts) gen env =
    case match t t (gen, env) of
      [] -> grow ts gen env     -- Term already mapped
      _ ->                      -- Otherwise make a fresh mapping
          let (gen', t') = clone gen t in
          case match t t' (gen', env) of
            (gen'', env') : _ -> grow ts gen'' env'
            [] -> error "Strand.grow: Internal error"

-- Build an instance from a role and a trace.  Returns the empty list
-- if the trace is not an instance of the given role.
bldInstance :: Algebra t p g s e c => Role t ->
               Trace t -> g -> [(g, Instance t e)]
bldInstance _ [] _ = error "Strand.bldInstance: Bad trace"
bldInstance role trace gen =
    loop (rtrace role) trace (gen, emptyEnv) -- Loop builds env
    where
      loop _ [] (gen, env) =        -- Trace can be shorter than role's trace
          [(gen, makeInstance role env trace)]
      loop (In t : c) (In t' : c') ge =
          do
            env <- match t t' ge
            loop c c' env
      loop (Out t : c) (Out t' : c') ge =
          do
            env <- match t t' ge
            loop c c' env
      loop (Sync t : c) (Sync t' : c') ge =
          do
            env <- matchTran t t' ge
            loop c c' env
      loop _ _ _ = []

makeInstance :: Role t -> e ->
                Trace t -> Instance t e
makeInstance role env trace =
    Instance { role = role,
               env = env,
               trace = trace,
               height = h }
    where
      h = length trace

-- This is the only place a role is generated with an empty name.
-- This is what marks a strand as a listener.
mkListener :: Algebra t p g s e c => Prot t g -> g -> t ->
              (g, Instance t e)
mkListener p gen term =
    case bldInstance (listenerRole p) [In term, Out term] gen of
      [x] -> x
      _ -> error "Strand.mkListener: Cannot build an instance of a listener"

-- Add to set s the variables that are in the range of instance i
addIvars :: Algebra t p g s e c => Set t -> Instance t e -> Set t
addIvars s i =
    foldl g s (reify (rvars (role i)) (env i))
    where
      g s (_, t) = foldVars (flip S.insert) s t

listenerTerm :: Instance t e -> Maybe t
listenerTerm inst =
    case rname (role inst) of
      "" -> recvTerm (trace inst !! 0) -- Get first term in trace
      _ -> Nothing              -- Not a listener strand

-- Nodes, Pairs, and Graphs

-- A node is composed of two integers, a strand identifier and a
-- position.  The position identifies an event in the strand's trace.
-- The second integer must be non-negative and less than the strand's
-- height

type Node = (Sid, Int)

-- A pair gives an ordering of two nodes, meaning the first node is
-- before the second one.

type Pair = (Node, Node)

-- Graphs of preskeletons

-- A strand is what is referred to by a strand ID.

data GraphStrand e i                 -- e for event, i for instance
    = GraphStrand { inst :: i,
                    nodes :: [GraphNode e i],
                    sid :: Sid }

-- The Strand ID determines equality and orderings
instance Eq (GraphStrand e i) where
    s0 == s1 = sid s0 == sid s1

instance Ord (GraphStrand e i) where
    compare s0 s1 = compare (sid s0) (sid s1)

instance Show (GraphStrand e i) where
    showsPrec _ s = shows (sid s)

-- A vertex is what is referred to by a node.

data GraphNode e i                 -- e for event, i for instance
    = GraphNode { event :: e,
                  preds :: [GraphNode e i], -- Immediate preds including
                  strand :: GraphStrand e i,  -- strand succession edges
                  pos :: Int }  -- The position of the node in the strand

-- The node determines equality and orderings
instance Eq (GraphNode e i) where
    n0 == n1 = (strand n0, pos n0) == (strand n1, pos n1)

instance Ord (GraphNode e i) where
    compare n0 n1 = compare (strand n0, pos n0) (strand n1, pos n1)

instance Show (GraphNode e i) where
    showsPrec _ n = let (s, i') = graphNode n in
                    showChar '(' . shows s . showString ", " .
                    shows i' . showChar ')'

-- The node of a vertex
graphNode :: GraphNode e i -> Node
graphNode n = (sid (strand n), pos n)

type GraphEdge e i = (GraphNode e i, GraphNode e i)

-- The pair of an edge
graphPair :: GraphEdge e i -> Pair
graphPair (n0, n1) = (graphNode n0, graphNode n1)

graphEdges :: [GraphStrand e i] -> [GraphEdge e i]
graphEdges strands =
    [ (dst, src) | s <- strands, src <- nodes s, dst <- preds src ]

data Graph e i
    = Graph { gstrands :: [GraphStrand e i],
              gedges :: [GraphEdge e i] }

-- The graph associated with a preskeleton
graph :: (i -> [d]) -> (i -> Int) -> [i] -> [Pair] -> Graph d i
graph trace height insts pairs =
    Graph { gstrands = strands,
            gedges = map getEdge pairs }
    where
      strands = [ GraphStrand { inst = inst,
                                nodes = nodes !! sid,
                                sid = sid } |
                  (sid, inst) <- zip [0..] insts ]
      nodes = [ [ GraphNode { event = trace (inst strand) !! pos,
                              preds = preds (sid, pos),
                              strand = strand,
                              pos = pos } |
                  pos <- nats (height (inst strand)) ] |
                (sid, strand) <- zip [0..] strands ]
      preds n = map getNode (entry n)
      getNode (s, i) = nodes !! s !! i
      getEdge (n0, n1) = (getNode n0, getNode n1)
      entry n = enrich n [ n0 | (n0, n1) <- pairs, n1 == n ]
      -- add strand succession edges
      enrich (s, i) ns
          | i > 0 = (s, i - 1) : ns
          | otherwise = ns

-- Compute the transitive reduction
graphReduce :: [GraphEdge e i] -> [GraphEdge e i]
graphReduce orderings =
    filter essential orderings
    where
      essential (dst, src) =
          loop dst (L.delete dst (preds src)) [src]
      loop _ [] _ = True        -- No other path found
      loop dst (n : ns) seen
          | n == dst = False    -- There is another path
          | elem n seen = loop dst ns seen
          | otherwise = loop dst (preds n ++ ns) (n : seen)

-- Compute the transitive closure
-- This routine returns pairs that are not well ordered.
-- Deal with it!
graphClose :: [GraphEdge e i] -> [GraphEdge e i]
graphClose orderings =
    filter (not . sameStrands) (loop orderings False orderings)
    where
      loop orderings False [] = orderings
      loop orderings True [] =
          loop orderings False orderings -- restart loop
      loop orderings repeat ((n0, n1) : pairs) =
          inner orderings repeat pairs [(n, n1) | n <- preds n0]
      inner orderings repeat pairs [] =
          loop orderings repeat pairs
      inner orderings repeat pairs (p : rest)
          | elem p orderings = inner orderings repeat pairs rest
          | otherwise = inner (p : orderings) True pairs rest
      sameStrands (n0, n1) = strand n0 == strand n1

{-
-- The nodes that preceed node n via strand succession
spreds :: GraphNode e i -> [GraphNode e i]
spreds n =
  filter f (nodes $ strand n)
  where
    f n' = pos n' < pos n
-}

-- Shared part of preskeletons

data Shared t g = Shared
    { prot  :: Prot t g,
      goals :: [Goal t] }

instance (Show t, Show g) => Show (Shared t g) where
    showsPrec _ s = shows (prot s)

protocol :: Preskel t g s e -> Prot t g
protocol k = prot $ shared k

kgoals :: Preskel t g s e -> [Goal t]
kgoals k = goals $ shared k

-- Preskeletons

data Preskel t g s e = Preskel
    { gen :: !g,
      shared :: Shared t g,
      insts :: ![Instance t e],
      strands :: ![Strand t e],
      orderings :: ![Pair],
      leadsto :: ![Pair],
      edges :: ![Edge t e],
      cInfo :: ConstInfo t,
      decls :: SkelDeclarations t,
      extraDecls :: SkelDeclarations t,
      kfacts :: ![Fact t],
      kcomment :: [SExpr ()],   -- Comments from the input
      pov :: Maybe (Preskel t g s e), -- Point of view, the
                                          -- original problem statement.
      strandids :: ![Sid],
      tc :: [Pair],             -- Transitive closure of orderings
                                -- Used only during generalization
      operation :: Operation t s,
      kpriority :: [(Node,Int)],
      prob :: [Sid] }        -- A map from the strands in the original
    deriving Show               -- problem statement, the pov, into
                                -- these strands.

-- The pov skeleton is the only skeleton that should have Nothing in
-- its pov field.

type Strand t e
    = GraphStrand (Event t) (Instance t e)

type Vertex t e
    = GraphNode (Event t) (Instance t e)

type Edge t e
    = GraphEdge (Event t) (Instance t e)

-- Data structure for tracking the causes for the creation of
-- preskeletons.

data Cause t
    = Cause Direction Node t (Set t)
    deriving Show

data Direction
    = Encryption
    | Nonce
    | StatePassing
    deriving Show

data Method t
    = Deleted Node
    | Weakened Pair
    | Separated t
    | Forgot (SkelDeclarations t)
    deriving Show

-- The operation used to generate the preskeleteton is either new via
-- the loader, a contraction, a regular augmentation, a listener
-- augmentation, or a mininization.  The augmentation includes a role
-- name and instance height.
data Operation t s
    = New
    | Contracted s (Cause t)
    | Displaced Int Int String Int (Cause t)
    | AddedStrand String Int (Cause t)
    | AddedListener t (Cause t)
    | AddedAbsence t t (Cause t)
    | AlgebraSolved s (Cause t)
    | Generalized (Method t)
    | Collapsed Int Int
      deriving Show

-- Create a preskeleton.  The point of view field might not be filled in.
-- This version is exported for use by the loader.  This preskeleton
-- must be consumed by firstSkeleton.
mkPreskel :: Algebra t p g s e c => g -> Prot t g -> [Goal t] ->
             [Instance t e] -> [Pair] -> [Pair] -> SkelDeclList t ->
             [Fact t] -> [SExpr ()] -> [(Node,Int)] ->
             Maybe (Preskel t g s e) -> [Sid] -> Preskel t g s e
mkPreskel gen protocol gs insts orderings leadsto dlist facts comment priorities
   maybePov maybeProb =
    k { kcomment = comment }
    where
      k = newPreskel gen shared insts orderings leadsto' decls'
          facts New prob kpriority maybePov
      shared = Shared { prot = protocol, goals = gs }
      decls' = foldl addInstOrigs decls (zip (nats (length insts)) insts)
      decls = mkDecls dlist'
      leadsto' = checkLeadsto leadsto []
      dlist' = map g dlist
      -- Fixed point on k is okay.
      prob = if M.isNothing maybePov then strandids k else maybeProb
      kpriority = priorities
      g ("uniq-orig", ds) = ("uniq-orig", map fuo ds)
      g ("uniq-gen", ds) = ("uniq-gen", map fug ds)
      g (tag, ds) = (tag, ds)
      fuo dinst
        | length (ts) == 1 = oneOrig (originationNodes (strands k) (head ts))
        | otherwise = error ("Strand.mkPreskel: uniq-orig expects single terms")
        where ts = dterms dinst
      oneOrig (t, []) =
        error ("Strand.mkPreskel: no origination point for " ++ show t)
      oneOrig (t, [l]) = declInst [t] [l]
      oneOrig (t, (_:_:_)) =
        error ("Strand.mkPreskel: too many origination points for " ++ show t)
      fug dinst
        | length (ts) /= 1 = error ("Strand.mkPreskel: uniq-gen expects single terms")
        | not (isVar $ head ts) && (isNum $ head ts) =
          error ("Strand.mkPreskel: uniq-gen numeric values must be variables")
        | otherwise = oneGen (generationNodes (strands k) (head ts))
        where ts = dterms dinst
      oneGen (t, []) =
        error ("Strand.mkPreskel: no generation point for " ++ show t)
      oneGen (t, [l]) = declInst [t] [l]
      oneGen (t, (_:_:_)) =
        error ("Strand.mkPreskel: too many generation points for " ++ show t)
      checkLeadsto [] _ = []
      checkLeadsto (((s1,i1),(s2,i2)) : rest) consumed =
        case ((trace (insts !! s1)) !! i1) of
          Sync (Tran (_, Just t)) -> case ((trace (insts !! s2)) !! i2) of
            Sync (Tran (Just t', nxt)) ->
               if (t /= t') then error ("Strand.mkPreskel: invalid state edge at " ++ show (s1,i1))
               else case nxt of
                 Nothing -> (((s1,i1),(s2,i2)) : checkLeadsto rest consumed)
                 Just _ -> if (elem (s1,i1) consumed) then
                   error ("Strand.mkPreskel: state of " ++ show (s1,i1) ++
                          " already consumed") else
                   (((s1,i1),(s2,i2)) : checkLeadsto rest ((s1,i1) : consumed))
            _ -> error ("Strand.mkPreskel: malformed state edge at " ++ show (s1,i1))
          _ -> error ("Strand.mkPreskel: malformed state edge at " ++ show (s1,i1))

addInstOrigs :: Algebra t p g s e c => SkelDeclarations t ->
                (Sid, Instance t e) -> SkelDeclarations t
addInstOrigs d (s, i) = inheritRdecls s i d

-- Strand functions

strandInst :: Preskel t g s e -> Sid -> Instance t e
strandInst k strand = insts k !! strand

nstrands :: Preskel t g s e -> Int
nstrands k = length (strandids k)

-- Convert the preskeleton made by the loader into the first skeleton
-- used in the search.
firstSkeleton :: Algebra t p g s e c => Preskel t g s e ->
                 [Preskel t g s e]
firstSkeleton k =
    do
      k <- wellFormedPreskel k
      k' <- toSkeleton False k
      -- only k' or its parent should have pov = Nothing
      case pov k' of
        Nothing ->
          return $ k' { prob = strandids k', pov = Just k', kcomment = kcomment k }
        Just _ -> return k'

-- Create a preskeleton.  The node ordering relation is put into the
-- preds field of each instance in this function.  The maybe uniquely
-- originating term data is also filled in.  This version is used
-- within this module.
newPreskel :: Algebra t p g s e c => g -> Shared t g ->
             [Instance t e] -> [Pair] -> [Pair] -> SkelDeclarations t ->
             [Fact t] -> Operation t s -> [Sid] -> [(Node,Int)] ->
             Maybe (Preskel t g s e) -> Preskel t g s e
newPreskel gen shared insts orderings leadsto decls
           facts oper prob kpriority pov =
    let orderings' = L.nub orderings
        leadsto' = L.nub leadsto
        g = graph trace height insts orderings'
        strands = gstrands g
        decls' = declsNub decls
        facts' = L.nub facts
        edges = gedges g
        tc = filter pairWellOrdered (graphClose $ graphEdges strands)
        k = Preskel { gen = gen,
                      shared = shared,
                      insts = insts,
                      strands = strands,
                      orderings = orderings',
                      leadsto = leadsto',
                      edges = edges,
                      decls = decls',
                      extraDecls = mkDecls [],
                      kfacts = facts',
                      cInfo = declsOrig strands decls',
                      kcomment = [],
                      tc = map graphPair tc,
                      strandids = nats (length insts),
                      operation = oper,
                      prob = prob,
                      kpriority = kpriority,
                      pov = pov } in
        if useCheckVars then
            checkVars k
        else k

checkVars :: Algebra t p g s e c => Preskel t g s e ->
             Preskel t g s e
checkVars k =
    foldl f k rolevars
    where
      skelvars = S.fromList $ kvars k
      rolevars = concatMap (rvars . role) (insts k)
      f k v
        | S.member v skelvars =
            error ("Strand.checkVars: role var in skel " ++ show k)
        | otherwise = k

vertex  :: Preskel t g s e -> Node -> Vertex t e
vertex k (s, i) =
    nodes (strands k !! s) !! i

originationNodes :: Algebra t p g s e c => [Strand t e] ->
                    t -> (t, [Node])
originationNodes strands u =
    (u, [ (sid strand, p) |
          strand <- reverse strands,
          p <- M.maybeToList $ originationPos u (trace (inst strand)) ])

generationNodes :: Algebra t p g s e c => [Strand t e] ->
                    t -> (t, [Node])
generationNodes strands u =
    (u, [ (sid strand, p) |
          strand <- reverse strands,
          p <- M.maybeToList $ generationPos u (trace (inst strand)) ])

-- Do the nodes in the orderings have the right direction?
wellOrdered :: Preskel t g s e -> Bool
wellOrdered k =
    all pairWellOrdered (edges k)

pairWellOrdered :: Edge t e -> Bool
pairWellOrdered (n0, n1) =
    case (event n0, event n1) of
      (Out _, In _) -> True
      (Sync _, Sync _) -> True
      (Out _, Sync _) -> True
      (Sync _, In _) -> True
      _ -> False

-- Do transitively reduced orderings have the right direction?
reducedWellOrdered :: Preskel t g s e -> Bool
reducedWellOrdered k =
    all reducedPairWellOrdered (edges k)

reducedPairWellOrdered :: Edge t e -> Bool
reducedPairWellOrdered (n0, n1) =
    case (event n0, event n1) of
      (Out _, In _) -> True
      (Sync (Tran (_, Just _)), Sync (Tran (Just _, _))) ->
        True
      (Sync (Tran (_, Nothing)), Sync (Tran (Just _, Just _))) ->
        True
      (Sync _, In _) -> True
      (Out _, Sync _) -> True
      _ -> False

-- The terms used in the strands in this preskeleton.
-- Should this return a set, or a multiset?
kterms :: Eq t => Preskel t g s e -> [t]
kterms k = iterms (insts k)

-- The terms used in only the message events in this preskeleton.
kmesgterms :: Eq t => Preskel t g s e -> [t]
kmesgterms k = imesgterms (insts k)

-- The terms used in a list of instances.
iterms :: Eq t => [Instance t e] -> [t]
iterms insts =
  L.nub [t | i <- insts,
             evt <- trace i,
             t <- evtTerms evt]

-- The terms used in only the message events in a list of instances.
imesgterms :: Eq t => [Instance t e] -> [t]
imesgterms insts =
  L.nub [t | i <- insts,
             evt <- trace i,
             t <- evtMesgTerms evt]

-- The node orderings form an acyclic order if there are no cycles.
-- Use depth first search to detect cycles.  A graph with no node with
-- an indegree of zero is cyclic and must not be checked with depth
-- first search.
acyclicOrder :: Preskel t g s e -> Bool
acyclicOrder k =
    all (not . backEdge numbering) edges
    where
      edges = graphEdges (strands k)
      -- The starting set contains the last node in every strand
      start = map (last . nodes) (strands k)
      -- Remove nodes that have non-zero indegree
      start' = foldl (flip L.delete) start (map fst edges)
      numbering = dfs preds start'

-- Variables in this preskeleton, excluding ones in roles, and ones
-- that only occur in a cause.  The variables in facts are explicitly
-- included.
kvars :: Algebra t p g s e c => Preskel t g s e -> [t]
kvars k =
    S.elems $ foldl addIvars dvars (insts k)
    where
      factVars = if useFactVars then kfactVars k else S.empty
      dvars = foldl f factVars $ declsTerms (decls k)
      f s t = foldVars (flip S.insert) s t

traceBase :: Int
traceBase = 3

-- Convert a trace to a number based on its pattern of events
tracePattern :: Trace t -> Int
tracePattern [] = 1
tracePattern (In _:r) = traceBase*(tracePattern r)
tracePattern (Out _:r) = traceBase*(tracePattern r) + 1
tracePattern (Sync _:r) = traceBase*(tracePattern r) + 2

-- Isomorphism Check

-- Are two skeletons equivalent?  Two skeletons are equivalent if they
-- are isomorphic.  A key efficiency requirement in the implementation
-- of the cryptograhic protocol shapes analysis algorithm is to ensure
-- only one member of each equivalence class of skeletons is analyzed,
-- and the results of that analysis is immediately used for all other
-- members of the equivalence class.

-- To meet this requirement, a list of skeletons that have been seen
-- is maintained.  Before a skeleton is put on a to do list for
-- analysis, it is checked to see if it is ismorphic to one already
-- seen.  If so, the results of the analysis for the ismorphic
-- skeleton is used instead of putting the skeleton on the to do list.

-- Once a skeleton has been printed, the only reason for saving it is
-- for isomorphism checking.  The isomorphism check is performed
-- frequently, so an specialized data structure is used.  The gist of
-- a skeleton is all that is needed for the test for equivalence.

data Gist t g = Gist
    { ggen :: g,
      gtraces :: [(Int, Trace t)],
      gorderings :: [Pair],
      gleadsto :: [Pair],
      gpatterns :: [Int],
      gdecls :: SkelDeclarations t,
      gfacts :: [Fact t],
      nvars :: !Int,           -- Number of variables
      ntraces :: !Int,         -- Number of traces
      briefs :: [(Int, Int)],  -- Multiset of trace briefs
      norderings :: !Int,      -- Number of orderings
      nleadsto :: !Int,        -- Number of leadsto pairs
      nsndecls :: [Int],       -- Counts of decls
      nfacts :: !Int }         -- Number of facts
    deriving Show

gist :: Algebra t p g s e c => Preskel t g s e -> Gist t g
gist k =
    Gist { ggen = gen k,
           gtraces = gtraces,
           gorderings = gorderings,
           gleadsto = gleadsto,
           gdecls = gdecls,
           gfacts = gfacts,
           gpatterns = patterns,
           nvars = length (kvars k),
           ntraces = length gtraces,
           briefs = multiset (map fst gtraces),
           norderings = length gorderings,
           nleadsto = length gleadsto,
           nsndecls = nsds gdecls,
           nfacts = length gfacts }
    where
      gtraces = map f (insts k)
      -- Old: f i = (height i, trace i)
      f i =
        (brief c, c)
        where c = trace i
      gorderings = orderings k
      gleadsto = leadsto k
      gdecls = decls k
      gfacts = kfacts k
      patterns = L.sort (map (tracePattern . trace) (insts k))
      nsds decls = [length (dknon decls), length (dkpnon decls), length (dkunique decls)]

-- Summarize a trace so that two traces don't match unless they have
-- the same number.  The summary used to be the height of the trace.
brief :: Algebra t p g s e c => Trace t -> Int
brief [] = 0
brief (In _ : c) = 1 + 4 * brief c
brief (Out _ : c) = 2 + 4 * brief c
brief (Sync _ : c) = 3 + 4 * brief c

-- Convert a list of integers into a sorted multiset representation.
-- The output is a list of pairs, (i, n). Integer n gives the
-- multiplity of integer i in the input list.  List is sorted based on
-- the first element in each pair.
multiset :: [Int] -> [(Int, Int)]
multiset brf =
  L.foldl insert [] brf
  where
    insert [] b = [(b, 1)]
    insert ((k, n) : brf) b
      | k == b = (k, n + 1) : brf
      | k > b = (b, 1) : (k, n) : brf
      | otherwise = (k, n) : insert brf b

-- Test to see if two preskeletons are isomorphic

-- First, ensure the two preskeletons have:
-- 1. The same number of variables
-- 2. The same number of strands
-- 3. The same number of node orderings
-- 4. The same number of terms in knon and kunique
-- 5. The same number of facts

-- Next compute the plausible permutations and substitutions.

-- Next, for each permutation of the strands, eliminate the ones that
-- map a strand to another of a different length, and don't cause the
-- node orderings to be equal.

-- For permutations that meet the previous conditions, see if there is
-- a renaming that maps every strand trace in one preskeleton into the
-- appropriate one in the other preskeleton.  Finally, check to see if
-- the renaming works in the nr and ur terms.

isomorphic :: Algebra t p g s e c => Gist t g -> Gist t g -> Bool
isomorphic g g' =
--    nvars g == nvars g' &&  -- Wrong in DH
    ntraces g == ntraces g' &&
    briefs g == briefs g' &&
    norderings g == norderings g' &&
    gpatterns g == gpatterns g' &&
    nleadsto g == nleadsto g' &&
    nsndecls g == nsndecls g' &&
    nfacts g == nfacts g' &&
    any (tryPerm g g') (permutations g g')

probIsomorphic :: Algebra t p g s e c => Preskel t g s e -> Preskel t g s e -> Bool
probIsomorphic k k' =
    any (tryPermProb g g' pr pr') (permutations g g')
    where
      g = gist k
      g' = gist k'
      pr = prob k
      pr' = prob k'

-- Extend a permutation while extending a substitution
-- Extend by matching later strands first
permutations :: Algebra t p g s e c => Gist t g ->
                Gist t g -> [((g, e), (g, e), [Sid])]
permutations g g' =
    map rev $ perms (gg, emptyEnv)
                    (gg, emptyEnv)
                    (reverse $ gtraces g)
                    (reverse $ nats $ ntraces g)
    where
      gg = gmerge (ggen g) (ggen g')
      perms fenv renv [] [] = [(fenv, renv, [])]
      perms fenv  renv ((h, c):hcs) xs =
          [ (fenv'', renv'', x:ys) |
            x <- xs,
            let (h', c') = gtraces g' !! x,
            h == h',
            fenv' <- jibeTraces c c' fenv,
            renv' <- jibeTraces c' c renv,
            (fenv'', renv'', ys) <- perms fenv' renv' hcs (L.delete x xs) ]
      perms _ _ _ _ = error "Strand.permutations: lists not same length"
      rev (fenv, renv, xs) = (fenv, renv, reverse xs)

-- Length of matched traces must agree.
jibeTraces :: Algebra t p g s e c => Trace t ->
              Trace t -> (g, e) -> [(g, e)]
jibeTraces [] [] ge = [ge]
jibeTraces (In t : c) (In t' : c') ge =
    do
      env <- match t t' ge
      jibeTraces c c' env
jibeTraces (Out t : c) (Out t' : c') ge =
    do
      env <- match t t' ge
      jibeTraces c c' env
jibeTraces (Sync t : c) (Sync t' : c') ge =
    do
      env <- matchTran t t' ge
      jibeTraces c c' env
jibeTraces _ _ _ = []

{-
-- Here is the permutation algorithm used

permutations :: Int -> [[Int]]
permutations n =
    perms (nats n)
    where
      perms []  = [[]]
      perms xs = [ x:ys | x <- xs, ys <- perms (L.delete x xs) ]

-- Here is the usual algorithm

-- Returns a list of all the permutations of the natural numbers less
-- that the argument.  The identity permutation is the first one in
-- the returned list.  The code is based on a function in Haskell 1.3.
permutations :: Int -> [[Int]]
permutations n =
    perms (nats n)
    where
      perms [] = [[]]
      perms (x:xs) = [zs | ys <- perms xs, zs <- interleave x ys]
      interleave x [] = [[x]]
      interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)
-}

tryPerm :: Algebra t p g s e c => Gist t g ->
           Gist t g -> ((g, e), (g, e), [Sid]) -> Bool
tryPerm g g' (fenv, renv, perm) =
    checkOrigs g g' fenv perm &&
    checkOrigs g' g renv (invperm perm) &&
    checkFacts g g' fenv perm &&
    checkFacts g' g renv (invperm perm) &&
    containsMapped (permutePair perm) (gorderings g') (gorderings g) &&
    containsMapped (permutePair perm) (gleadsto g') (gleadsto g)

tryPermProb :: Algebra t p g s e c => Gist t g ->
           Gist t g -> [Sid] -> [Sid] -> ((g, e), (g, e), [Sid]) -> Bool
tryPermProb g g' prob prob' (fenv, renv, perm) =
    all (\n -> perm !! (prob !! n) == prob' !! n) [0..((length prob)-1)] &&
    tryPerm g g' (fenv, renv, perm)

invperm :: [Int] -> [Int]
invperm p = map snd (L.sortOn fst (zip p [0..]))

-- containsMapped f xs ys is true when list xs contains each element
-- in ys after being mapped with function f.
containsMapped :: Eq a => (a -> a) -> [a] -> [a] -> Bool
containsMapped f xs ys =
    all (flip elem xs) (map f ys)

permutePair :: [Sid] -> Pair -> Pair
permutePair perm (n, n') = (permuteNode perm n, permuteNode perm n')

permuteNode :: [Sid] -> Node -> Node
permuteNode perm (strand, pos) = (perm !! strand, pos)

checkFacts :: Algebra t p g s e c => Gist t g ->
              Gist t g -> (g, e) -> [Sid] -> Bool
checkFacts g g' (_, e) perm =
  all f (gfacts g)
  where
    f fact = elem (instUpdateFact e (perm !!) fact) (gfacts g')

-- Preskeleton Reduction System (PRS)

-- The PRS reduces a preskeleton to a list of skeletons.  Along the way,
-- it applies the associtated homomorphism to a node and computes a
-- substitution.  Thus if skel (k, n, empty) = [(k', n', sigma)], then
-- phi,sigma is a homomorphism from k to k', n' = phi(n).

type PRS t p g s e c = (Preskel t g s e, -- Parent
                        Preskel t g s e, -- Potential cohort member
                        Node,   -- Image of test node in member
                        [Sid],  -- Strand map part of homomorphism
                        s)      -- Substition part of homomorphism

-- Extract the protential cohort member from a PRS.
skel :: PRS t p g s e c -> Preskel t g s e
skel (_, k, _, _, _) = k

-- Returns the preskeletons that result from applying a substitution.
ksubst :: Algebra t p g s e c => PRS t p g s e c ->
          (g, s) -> [PRS t p g s e c]
ksubst (k0, k, n, phi, hsubst) (gen, subst) =
  do
      (gen', insts') <- foldMapM (substInst subst) gen (insts k)
      let decls' = declsMapTerms (substitute subst) (decls k)
      let facts' = map (substFact subst) (kfacts k)
      let operation' = substOper subst (operation k)
      let k' = newPreskel gen' (shared k) insts'
               (orderings k) (leadsto k) decls' facts' operation'
               (prob k) (kpriority k) (pov k)
      k' <- wellFormedPreskel k'
      return (k0, k', n, phi, compose subst hsubst)

-- Monad version of mapAccumR
foldMapM :: Monad m => (a -> b -> m (a, c)) -> a -> [b] -> m (a, [c])
foldMapM _ acc [] = return (acc, [])
foldMapM f acc (x:xs) =
  do
      (acc', xs') <- foldMapM f acc xs
      (acc'', x') <- f acc' x
      return (acc'', x':xs')

substInst :: Algebra t p g s e c => s -> g -> Instance t e ->
             [(g, Instance t e)]
substInst subst gen i =
    bldInstance (role i) (map (evtMap $ substitute subst) (trace i)) gen

substOper :: Algebra t p g s e c => s ->
             Operation t s ->
             Operation t s
substOper _ New = New
substOper subst (Contracted s cause) =
    Contracted (compose subst s) (substCause subst cause)
substOper _ m@(Displaced _ _ _ _ _) = m
substOper subst (AddedStrand role height cause) =
    AddedStrand role height (substCause subst cause)
substOper subst (AddedListener t cause) =
    AddedListener (substitute subst t) (substCause subst cause)
substOper subst (AddedAbsence t1 t2 cause) =
    AddedAbsence (substitute subst t1) (substitute subst t2) (substCause subst cause)
substOper subst (AlgebraSolved s cause) =
    AlgebraSolved (compose subst s) (substCause subst cause)
substOper _ m@(Generalized _) = m
substOper _ m@(Collapsed _ _) = m

substCause :: Algebra t p g s e c => s ->
              Cause t ->
              Cause t
substCause subst (Cause dir n t escape) =
    Cause dir n (substitute subst t) (S.map (substitute subst) escape)

-- A compression (s is to be eliminated)
compress :: Algebra t p g s e c => Bool -> PRS t p g s e c ->
            Sid -> Sid -> [PRS t p g s e c]
compress validate (k0, k, n, phi, hsubst) s s' =
    do
      let perm = updatePerm s s' (strandids k)
      orderings' <- normalizeOrderings validate
                    (permuteOrderings perm (orderings k))
--      leadsto' <- normalizeOrderings validate
--                    (permuteOrderings perm (leadsto k))
      let k' =
              newPreskel
              (gen k)
              (shared k)
              (deleteNth s (insts k))
              orderings'
              (permuteOrderings perm (leadsto k))
              (declsMapStrands perm (decls k))
              (map (updateFact $ updateStrand s s') (kfacts k))
              (operation k)
              (updateProb perm (prob k))
              (updatePriority perm (kpriority k))
              (pov k)
      k'' <- wellFormedPreskel k'
      return (k0, k'', permuteNode perm n, map (perm !!) phi, hsubst)

permuteOrderings :: [Sid] -> [Pair] -> [Pair]
permuteOrderings perm orderings = map (permutePair perm) orderings

updatePerm :: Int -> Int -> [Sid] -> [Sid]
updatePerm old new perm =
    map (updateStrand old new) perm

-- Old is to be eliminated and merged into new
updateStrand :: Int -> Int -> Sid -> Sid
updateStrand old new i =
    let j = if old == i then new else i in
    if j > old then j - 1 else j

-- Eliminates implied intrastrand orderings and fails if it finds a
-- reverse intrastrand ordering when flag is true.
normalizeOrderings :: Bool -> [Pair] -> [[Pair]]
normalizeOrderings True orderings =
    loop [] orderings
    where
      loop acc [] = [acc]
      loop acc (p@((s0, i0), (s1, i1)) : ps)
          | s0 /= s1 = loop (p : acc) ps
          | i0 < i1 = loop acc ps
          | otherwise = []
normalizeOrderings False orderings =
    [filter (\ ((s0, _), (s1, _)) -> s0 /= s1) orderings]

updateProb :: [Sid] -> [Sid] -> [Sid]
updateProb mapping prob =
    map (mapping !!) prob

updatePriority :: [Sid] -> [(Node,Int)] -> [(Node,Int)]
updatePriority mapping kpriority =
    map (\ (n,i) -> (nodeMap mapping n, i)) kpriority

-- Provide a list of nodes with non-default priorities
kpriorities :: Preskel t g s e -> [(Node,Int)]
kpriorities k =
  [((s,i),priority k (s,i)) | s <- [0..(length (insts k) - 1)],
               i <- [0..(height ((insts k) !! s) -1)],
               priority k (s,i) /= defaultPriority,
               outNode s i]
  where
    outNode s i =
      case event (nodes (strands k !! s) !! i) of
        Out _ -> False
        In _ -> True
        Sync _ -> False

priority :: Preskel t g s e -> Node -> Int
priority k (s,i) =
  case lookup (s,i) (kpriority k) of
    Just p -> p
    Nothing -> rpriority (role $ (insts k !! s)) !! i

-- Purge a strand.  Used by thinning.
purge :: Algebra t p g s e c => PRS t p g s e c ->
            Sid -> Sid -> [PRS t p g s e c]
purge (k0, k, n, phi, hsubst) s s' =
    do
      let perm = updatePerm s s' (strandids k)
      orderings' <- normalizeOrderings False
                    (permuteOrderings perm
                     (forward s (orderings k))) -- Pruning difference
      leadsto' <- normalizeOrderings False
                    (permuteOrderings perm
                     (forward s (leadsto k))) -- Pruning difference
      let insts' = (deleteNth s (insts k))
      let k' =
              newPreskel
              (gen k)
              (shared k)
              insts'
              orderings'
              leadsto'
              (declsMapStrands perm
                      (declsFilterValid (iterms insts') (strandNotInSkel s)  (decls k)))
              (map (updateFact $ updateStrand s s') (kfacts k))
              (operation k)
              (updateProb perm (prob k))
              (updatePriority perm (kpriority k))
              (pov k)
      k'' <- wellFormedPreskel $ cleansPreskel k'
      return (k0, k'', permuteNode perm n, map (perm !!) phi, hsubst)

-- Forward orderings from strand s
forward :: Sid -> [Pair] -> [Pair]
forward s orderings =
    concatMap f orderings
    where
      f p@((s0, i0), (s1, i1))
          | s0 == s = [((s2, i2), (s1, i1)) | -- Forward here
                       ((s2, i2), (s3, i3)) <- orderings,
                       s3 == s0 && i0 >= i3]
          | s1 == s = []        -- Dump edges to strand s
          | otherwise = [p]     -- Pass thru other edges

-- Cleans facts in a skeleton
cleansPreskel :: Algebra t p g s e c => Preskel t g s e ->  Preskel t g s e
cleansPreskel k =
  newPreskel
  (gen k)
  (shared k)
  (insts k)
  (orderings k)
  (leadsto k)
  (decls k)
  (cleansFacts (kvars k) (kfacts k))
  (operation k)
  (prob k)
  (kpriority k)
  (pov k)

matchTraces :: Algebra t p g s e c => Trace t ->
               Trace t -> (g, e) -> [(g, e)]
matchTraces [] _ env = [env]    -- Pattern can be shorter
matchTraces (In t : c) (In t' : c') env =
    do
      e <- match t t' env
      matchTraces c c' e
matchTraces (Out t : c) (Out t' : c') env =
    do
      e <- match t t' env
      matchTraces c c' e
matchTraces (Sync t : c) (Sync t' : c') env =
    do
      e <- matchTran t t' env
      matchTraces c c' e
matchTraces _ _ _ = []

-- Thinning

maybeThin :: Algebra t p g s e c => Bool -> PRS t p g s e c -> [PRS t p g s e c]
maybeThin True prs = thin prs
maybeThin False prs = twist prs

thin :: Algebra t p g s e c => PRS t p g s e c -> [PRS t p g s e c]
thin prs = ans
  where
    ans = do
      prs <- twist prs
      thinStrands prs [] $ reverse ss
    --where                       -- Remove strands in image of POV
    ss = filter (\s -> notElem s (prob $ skel prs)) (strandids $ skel prs)

-- Takes a skeleton, a list of pairs of strands that matched, and a
-- list of strands yet to be analyzed, and produces the result of
-- skeletonization.  It first tries single pair thinning, and during
-- that process collects potential multistrand thinning pairs.  Once
-- there are no more unanalyzed strands, it tries multistrand
-- thinning.
thinStrands :: Algebra t p g s e c => PRS t p g s e c ->
               [(Sid, Sid)] -> [Sid] -> [PRS t p g s e c]
thinStrands prs ps [] =         -- All strands analyzied
    case multiPairs ps of       -- Generate multipairs
      [] -> [prs]
      mps -> thinMany prs mps   -- Try multistrand thinning
thinStrands prs ps (s:ss) =
  thinStrandPairs prs ps s ss ss

-- This loop tries pairs of strands.  Takes a skeleton, a list of
-- pairs of strands that matched but did not thin, a strand to be
-- eliminated, a list of strands to be used in the outer loop
-- (thinStrands), and a list of strands to be considered for matching,
-- and produces the result of skeletonization.
thinStrandPairs :: Algebra t p g s e c => PRS t p g s e c -> [(Sid, Sid)] ->
                   Sid -> [Sid] -> [Sid] -> [PRS t p g s e c]
thinStrandPairs prs ps _ ss [] =
  thinStrands prs ps ss
thinStrandPairs prs ps s ss (s':ss') =
  case thinStrand prs s s' of
    -- Try next pair, there was no match
    Nothing -> thinStrandPairs prs ps s ss ss'
    -- Try next pair, there was a match so save this pair
    Just [] -> thinStrandPairs prs ((s, s'):ps) s ss ss'
    Just prss ->                -- Success at single strand thinning
      do
        prs <- prss
        thin prs                -- Restart from the beginning

-- Try thinning s with s'.  If there was no match, return Nothing,
-- else return Just the list of results.
thinStrand :: Algebra t p g s e c => PRS t p g s e c ->
              Sid -> Sid -> Maybe [PRS t p g s e c]
thinStrand prs s s' =
    let k = skel prs in
    case thinStrandMatch k s s' (gen k, emptyEnv) of
      False -> Nothing
      True ->
        Just [ prs' | prs' <- purge prs s s',
                      prs'' <- purge prs s' s,
                      probIsomorphic (skel prs') (skel prs'')]

-- See if two strands match.
thinStrandMatch :: Algebra t p g s e c => Preskel t g s e ->
                   Sid -> Sid -> (g, e) -> Bool
thinStrandMatch k s s' env =
  height i == height i'
  && not (null $ matchTraces (trace i) (trace i') env)
  && not (null $ matchTraces (trace i') (trace i) env)
  where
    i = strandInst k s
    i' = strandInst k s'

-- Multistrand thinning

-- Generate all candidate pairings
multiPairs :: [(Sid, Sid)] ->  [[(Sid, Sid)]]
multiPairs _ | useSingleStrandThinning = []
multiPairs ps = filter atLeastTwo $ thinOne ps

-- List is of length at least two.
atLeastTwo :: [a] -> Bool
atLeastTwo (_:_:_) = True
atLeastTwo _ = False

thinOne :: [(Sid, Sid)] -> [[(Sid, Sid)]]
thinOne [] = []
thinOne (p:ps) =
    thinTwo p ps ++ thinOne ps

-- Construct possible pairs that include (s, s').
thinTwo :: (Sid, Sid) -> [(Sid, Sid)] -> [[(Sid, Sid)]]
thinTwo (s, s') ps =
    [(s, s')] : [(s, s') : ps' | ps' <- thinOne (filter (diff (s, s')) ps)]
    where                       -- filter out pairs with seen strands
      diff (s0, s1) (s2, s3) =
          s0 /= s2 && s0 /= s3 && s1 /= s2 && s1 /= s3

-- Try all multistrand pairings until one succeeds.
thinMany :: Algebra t p g s e c => PRS t p g s e c ->
            [[(Sid, Sid)]] -> [PRS t p g s e c]
thinMany prs [] = twist prs
thinMany prs (ps:mps) =
    case thinManyStrands prs ps of
      [] -> thinMany prs mps
      prss ->                           -- Success
        do
          prs <- prss
          thin prs

thinManyStrands :: Algebra t p g s e c => PRS t p g s e c ->
                   [(Sid, Sid)] -> [PRS t p g s e c]
thinManyStrands prs ps =
  [ prs' | prs' <- compressMany prs ps,
           prs'' <- compressMany prs (swap ps),
           probIsomorphic (skel prs') (skel prs'')]

compressMany :: Algebra t p g s e c => PRS t p g s e c ->
                [(Sid, Sid)] -> [PRS t p g s e c]
compressMany prs [] = [prs]
compressMany prs ((s, s'):ps) =
    do
      prs' <- purge prs s s'
      compressMany prs' (map (updatePairs s s') ps)

swap :: [(a, a)] -> [(a, a)]
swap ps =
    map (\(x, y) -> (y, x)) ps

updatePairs :: Sid -> Sid -> (Sid, Sid) -> (Sid, Sid)
updatePairs old new (s, s') =
    (updateStrand old new s, updateStrand old new s')

-- Enfore absence constraints.

useTwist :: Bool
useTwist = True -- False

twist :: Algebra t p g s e c => PRS t p g s e c -> [PRS t p g s e c]
twist prs | not useTwist = reduce prs
twist prs =
  do
    prs <- enforceAbsence prs
    reduce prs

{-
badabs :: Algebra t p g s e c => Preskel t g s e -> Bool
badabs k =
  any f (kabsent k)
  where
    f (x, y) = expnInExpr x y

chk :: Algebra t p g s e c => Preskel t g s e -> Preskel t g s e
chk k | badabs k = error ("No twist " ++ show (gen k))
      | otherwise = k

chkprs :: Algebra t p g s e c => PRS t p g s e c ->  PRS t p g s e c
chkprs prs@(_, k, _, _, _)
  | badabs k = error ("No twist " ++ show (gen k))
  | otherwise = prs

-}

-- Transitive Reduction

-- An edge is essential if its removal eliminates all paths from its
-- source to its destination.  This function removes all non-essential
-- edges from the ordering relation.

reduce :: Algebra t p g s e c => PRS t p g s e c -> [PRS t p g s e c]
reduce (k0, k, n, phi, hsubst) =
    let o = L.nub (map graphPair (graphReduce (edges k)) ++ (filter nonIntraStrand (leadsto k))) in
    if length o == length (orderings k) then
        do
          reducedWellFormed k
          noStateSplit k
          return (k0, k, n, phi, hsubst) -- Nothing to take away
    else
        do
          let k' =
                  newPreskel
                  (gen k)
                  (shared k)
                  (insts k)
                  o
                  (leadsto k)
                  (decls k)
                  (kfacts k)
                  (operation k)
                  (prob k)
                  (kpriority k)
                  (pov k)
          k'' <- wellFormedPreskel k'
          reducedWellFormed k''
          noStateSplit k''
          return (k0, k'', n, phi, hsubst)
      where
        nonIntraStrand ((s1,_),(s2,_)) = (s1 /= s2)
--    where
--      leadstoPairs = filter isLeadsTo pairs
--      pairs = map graphPair (edges k)
--      isLeadsTo ((s1,i1),(s2,i2)) =
--        case (trace (insts k !! s1) !! i1) of
--        (Sync (Tran (_, Just next, _))) -> case (trace (insts k !! s2) !! i2) of
--                      (Sync (Tran (Just now, _, _))) -> (now == next)
--                      _ -> False
--        _ -> False

-- Answers for cohorts
type Ans t p g s e c = (Preskel t g s e, Node, [Sid], s)

ans :: PRS t p g s e c -> [Ans t p g s e c]
ans (_, k, n, phi, subst) = [(k, n, phi, subst)]

-- Returns the skeleton associated with a preskeleton or nothing when
-- there isn't one.  Manufacture a node and a term, and then drop them
-- afterwards.
toSkeleton :: Algebra t p g s e c => Bool -> Preskel t g s e ->
              [Preskel t g s e]
toSkeleton prune k =
    do
      prs <- skeletonize prune (k, k, (0, 0), strandids k, emptySubst)
      (k', _, _, _) <- ans prs
      return k'

-- Contraction

contract :: Algebra t p g s e c => Preskel t g s e -> Node ->
            Cause t -> (g, s) -> [Ans t p g s e c]
contract k n cause subst =
    do
      prs <- ksubst (k, k { operation = Contracted emptySubst cause },
                           n, strandids k, emptySubst) subst
      prs' <- skeletonize useThinning prs
      ans prs'

-- Regular Augmentation

-- First argument determines if displacement is used.
augment :: Algebra t p g s e c => Preskel t g s e ->
           Node -> Cause t -> Role t ->
           (g, s) -> Instance t e -> Bool -> [Ans t p g s e c]
augment k0 n cause role subst inst isState =
  do
    prs <- augmentAndDisplace k0 n cause role subst inst isState
    ans prs

-- Apply a substitution, and then augment and displace.  Augmentations
-- add an instance and one ordered pair.  Displacements add an ordered
-- pair and may add nodes.
augmentAndDisplace :: Algebra t p g s e c => Preskel t g s e ->
                      Node -> Cause t -> Role t ->
                      (g, s) -> Instance t e -> Bool -> [PRS t p g s e c]
augmentAndDisplace k0 n cause role subst inst isState =
    do
      prs <- substAndAugment k0 n cause role subst inst isState
      augDisplace prs ++ skeletonize useThinning prs

-- Apply the substitution and apply augmentation operator.
substAndAugment :: Algebra t p g s e c => Preskel t g s e ->
                   Node -> Cause t -> Role t ->
                   (g, s) -> Instance t e -> Bool -> [PRS t p g s e c]
substAndAugment k n cause role subst inst isState =
  do
      let operation' = AddedStrand (rname role) (height inst) cause
      prs <- (ksubst (k, k { operation = operation' }, n,
                           strandids k, emptySubst) subst)
      aug prs inst isState

-- Apply the augmentation operator by adding an instance and one
-- ordered pair.
aug :: Algebra t p g s e c => PRS t p g s e c ->
       Instance t e -> Bool -> [PRS t p g s e c]
aug (k0, k, n, phi, hsubst) inst isState =
  do
      let insts' = (insts k) ++ [inst]
      let s = length (insts k)  -- The newly created strand
      let pair = ((s, height inst - 1), n)
      let orderings' = pair : orderings k
      let leadsto' = if isState then pair : leadsto k else leadsto k
      let decls' = inheritRdecls s inst (decls k)
      let k' = newPreskel (gen k) (shared k) insts'
               orderings' leadsto' decls' (kfacts k) (operation k)
               (prob k) (kpriority k) (pov k)
      k' <- wellFormedPreskel k'
      return (k0, k', n, phi, hsubst)

-- Add all displacements
augDisplace :: Algebra t p g s e c => PRS t p g s e c -> [PRS t p g s e c]
augDisplace prs =
  do
      let s = nstrands (skel prs) - 1
      s' <- nats s
      augDisplaceStrands prs s s'

-- Try to displace with strand s'
augDisplaceStrands :: Algebra t p g s e c => PRS t p g s e c ->
                      Sid -> Sid -> [PRS t p g s e c]
augDisplaceStrands (k0, k, n, phi, hsubst) s s' =
    do
      (s, s', subst) <- us
      let op = addedToDisplaced (operation k) s s'
      prs <- ksubst (k0, k { operation = op}, n, phi, hsubst) subst
      prs <- compress True prs s s'
      skeletonize useThinning prs
  where
    us = unifyStrands k s s'

-- See if two strands unify.  They can be of differing heights.  The
-- second strand returned may be longer.
unifyStrands :: Algebra t p g s e c => Preskel t g s e ->
                Sid -> Sid -> [(Sid, Sid, (g, s))]
unifyStrands k s s' =
    let i = strandInst k s
        i' = strandInst k s' in
    if height i > height i' then
        unifyStrands k s' s
    else
        do
          (gen', subst) <- unifyTraces (trace i) (trace i') (gen k, emptySubst)
          return (s, s', (gen', subst))

-- Unify traces where the first trace is allowed to be shorter than
-- the second trace.
unifyTraces :: Algebra t p g s e c => Trace t ->
               Trace t -> (g, s) -> [(g, s)]
unifyTraces [] _ subst = [subst]
unifyTraces (In t : c) (In t' : c') subst =
    do
      s <- unify t t' subst
      unifyTraces c c' s
unifyTraces (Out t : c) (Out t' : c') subst =
    do
      s <- unify t t' subst
      unifyTraces c c' s
unifyTraces (Sync t : c) (Sync t' : c') subst =
    do
      s <- unifyTran t t' subst
      unifyTraces c c' s
unifyTraces _ _ _ = []

addedToDisplaced :: Algebra t p g s e c => Operation t s ->
                    Int -> Int -> Operation t s
addedToDisplaced (AddedStrand role height cause) s s' =
    Displaced s s' role height cause
addedToDisplaced _ _ _ = error "Strand.addedToDisplaced: Bad operation"

-- Listener Augmentation

addListener :: Algebra t p g s e c => Preskel t g s e -> Node ->
               Cause t -> t -> [Ans t p g s e c]
addListener k n cause t =
    do
      k' <- wellFormedPreskel k'
      prs <- skeletonize useThinning
             (k, k', n, strandids k, emptySubst)
      ans prs
    where
      k' = newPreskel gen' (shared k) insts' orderings' (leadsto k) (decls k)
           (kfacts k) (AddedListener t cause) (prob k) (kpriority k) (pov k)
      (gen', inst) = mkListener (protocol k) (gen k) t
      insts' = insts k ++ [inst]
      pair = ((length (insts k), 1), n)
      orderings' = pair : orderings k

addBaseListener :: Algebra t p g s e c => Preskel t g s e -> Node ->
               Cause t -> t -> [Ans t p g s e c]
addBaseListener k n cause t =
    do
      k' <- wellFormedPreskel k'
      prs <- skeletonize useThinning
             (k, k', n, strandids k, emptySubst)
      ans prs
    where
      k' = newPreskel gen'' (shared k) insts' orderings' (leadsto k) decls'
           (kfacts k) (AddedListener t' cause) (prob k) (kpriority k) (pov k)
      (gen', t') = basePrecursor (gen k) t
      (gen'', inst) = mkListener (protocol k) gen' t'
      insts' = insts k ++ [inst]
      pair = ((length (insts k), 1), n)
      orderings' = pair : orderings k
      decls' = addDeclInst "precur" [] [(length (insts k), 0)] (decls k)

isNodePrecur :: Algebra t p g s e c => Preskel t g s e -> Node -> Bool
isNodePrecur k n =
  declMember "precur" [] [n] (decls k)

-- addAbsence
addAbsence :: Algebra t p g s e c => Preskel t g s e -> Node ->
              Cause t -> t -> t -> [Ans t p g s e c]
addAbsence k n cause x t =
    do
      k'' <- wellFormedPreskel k'
      prs <- skeletonize useThinning
             (k, k'', n, strandids k, emptySubst)
      ans prs
    where                       -- New cause should be added!
      k' = newPreskel (gen k) (shared k) (insts k) (orderings k)
           (leadsto k) decls' (kfacts k) (AddedAbsence x t cause)
           (prob k) (kpriority k) (pov k)
      decls' = addDeclInst "absent" [x, t] [] (decls k)

-- Numeric contraction
-- MDL: Diffie-Hellman specific
-- algebraSolve :: Algebra t p g s e c => Preskel t g s e -> Node ->
--                Cause t -> t -> t -> [Ans t p g s e c]
-- algebraSolve k n cause ct t =
--  do
--    subst <- solutions
--    prs <- ksubst (k, k { operation = AlgebraSolved emptySubst cause },
--                         n, strandids k, emptySubst) subst
--    prs' <- skeletonize usePruningWhileSolving prs
--    ans prs'
--  where
--    solutions = unify ct t' (gen', emptySubst)
--    (gen', t') = genericize (gen k) t

-- Homomorphisms

-- Find a substitution that demonstrates the existence of a
-- homomorphism between the two skeletons using the given
-- strand mapping function.

homomorphism :: Algebra t p g s e c => Preskel t g s e ->
                Preskel t g s e -> [Sid] -> [e]
homomorphism k k' mapping =
    do
      (_, env) <- findReplacement k k' mapping
      case validateEnv k k' mapping env of
        True -> [env]
        False -> []

findReplacement :: Algebra t p g s e c => Preskel t g s e ->
                   Preskel t g s e -> [Sid] -> [(g, e)]
findReplacement k k' mapping =
    foldM (matchStrand k k' mapping) (gg, emptyEnv) (strandids k)
    where
      gg = gmerge (gen k) (gen k')

matchStrand :: Algebra t p g s e c => Preskel t g s e ->
               Preskel t g s e -> [Sid] -> (g, e) -> Sid -> [(g, e)]
matchStrand k k' mapping env s =
    matchTraces (trace (strandInst k s)) (trace (strandInst k' s')) env
    where
      s' = mapping !! s

validateEnv :: Algebra t p g s e c => Preskel t g s e ->
               Preskel t g s e -> [Sid] -> e -> Bool
validateEnv k k' mapping env =
  validateDeclEnv k k' mapping env &&
  all (flip elem (kfacts k'))
  (map (instUpdateFact env (mapping !!)) (kfacts k)) &&
  all (flip elem (tc k')) (permuteOrderings mapping (orderings k))

-- Given a realized skeleton k, generate candidates for minimization.
-- A candidate is a preskeleton and a strand mapping from the
-- candidate to k.  The preskeleton need not be well formed, as that
-- test is applied elsewhere.

type Candidate t p g s e c = (Preskel t g s e, [Sid])

addIdentity :: Preskel t g s e -> Candidate t p g s e c
addIdentity k = (k, strandids k)

separateVariablesLimit :: Int
separateVariablesLimit = 1024

generalize :: Algebra t p g s e c => Preskel t g s e ->
              [Candidate t p g s e c]
generalize k = deleteNodes k ++
               weakenOrderings k ++
               forgetAssumption k ++
               take separateVariablesLimit (separateVariables k)

-- Node deletion

{-

delete node n in k

1. if (s, 0) part of prob return Nothing
2. if not initial node, truncate instance of node else delete instance
3. weaken ordering when filtering it (see shortenOrdering)
4. drop nons that aren't mentioned anywhere
5. drop uniques that aren't carried anywhere
6. update prob upon instance deletion
-}

deleteNodes :: Algebra t p g s e c => Preskel t g s e ->
               [Candidate t p g s e c]
deleteNodes k =
    do
      strand <- strands k
      node <- nodes strand
      deleteNode k node

deleteNode :: Algebra t p g s e c => Preskel t g s e ->
              Vertex t e -> [(Preskel t g s e, [Sid])]
deleteNode k n
    | p == 0 && elem s (prob k) = []
    | p == 0 =
        do
          let mapping = deleteNth s (strandids k)
          let k' = deleteStrand k (gen k) s (s, p)
          return (k', mapping)
    | otherwise =
        do
          let mapping = strandids k
          let i = inst (strand n)
          (gen', i') <- bldInstance (role i) (take p (trace i)) (gen k)
          let k' = deleteNodeRest k gen' (s, p) (replaceNth i' s (insts k))
                   (shortenOrderings (s, p) (tc k))
                   (shortenOrderings (s, p) (leadsto k))
                   (prob k) (kfacts k)
          return (k', mapping)
    where
      p = pos n
      s = sid (strand n)

-- Update orderings when a strand is eliminated (p == 0)
deleteOrderings :: Sid -> [Pair] -> [Pair]
deleteOrderings s ps =
    do
      p <- ps
      deleteOrdering p
    where
      deleteOrdering (n0@(s0, _), n1@(s1, _))
          | s == s0 || s == s1 = []
          | otherwise = [(adjust n0, adjust n1)]
      adjust n@(s', i')
          | s' > s = (s' - 1, i')
          | otherwise = n

-- Update orderings when a strand is shortened (p > 0)
shortenOrderings :: Node -> [Pair] -> [Pair]
shortenOrderings (s, i) ps =
    do
      pair <- ps
      shortenOrdering pair
    where
      shortenOrdering p@((s0, i0), (s1, i1))
          | s == s0 && i <= i0 = []
          | s == s1 && i <= i1 = []
          | otherwise = [p]

strandNotInSkel :: Sid -> Node -> Bool
strandNotInSkel s (s', _) = s /= s'

deleteStrand :: Algebra t p g s e c => Preskel t g s e ->
                  g -> Sid -> Node -> Preskel t g s e
deleteStrand k gen s n =
    newPreskel gen (shared k) insts'
    (deleteOrderings s (tc k))
    (deleteOrderings s (leadsto k)) -- is this the right thing?
    decls'
    facts'
    (Generalized (Deleted n))
    (updatePerm s s (prob k)) -- s shouldn't appear in prob, but we need to update
                              -- in case any strands in prob are > s.
    (kpriority k)
    (pov k)
  where
    insts' = deleteNth s (insts k)
    -- Drop nons that aren't mentioned anywhere
    mapping = updatePerm s (-1) (strandids k)
    decls0 = declsFilterValid (iterms (insts k)) (strandNotInSkel s) (decls k)
    decls' = declsFilterValid terms (const True)
             (declsMapStrands mapping decls0)
    facts' = cleansFacts terms (map
                                (updateFact (updateStrand s s))
                                (deleteFacts s $ kfacts k))
    terms = iterms insts'

nodeInSkel :: Preskel t g s e -> Node -> Bool
nodeInSkel k (s, i) =
  s >= 0 && s < nstrands k && i >= 0 &&
  i < height (strandInst k s)

deleteNodeRest :: Algebra t p g s e c => Preskel t g s e ->
                  g -> Node -> [Instance t e] -> [Pair] -> [Pair] ->
                  [Sid] -> [Fact t] -> Preskel t g s e
deleteNodeRest k gen n insts' orderings leadsto prob facts =
    newPreskel gen (shared k) insts' orderings leadsto decls' facts'
                   (Generalized (Deleted n)) prob (kpriority k) (pov k)
    where
      -- Drop nons that aren't mentioned anywhere
      decls' = declsFilterValid terms (nodeInSkel k) (decls k)
      facts' = cleansFacts terms facts
      terms = iterms insts'

deleteFacts :: Sid -> [Fact t] -> [Fact t]
deleteFacts s facts =
  filter f facts
  where
    f (Fact _ ft) =
      all g ft
    g (FSid s') = s /= s'
    g (FTerm _) = True

-- Node ordering weakening

-- To weaken, create a candidate for each element in the current
-- ordering, which is already the result of a transitive reduction.
-- Weaken by computing the transitive closure of the ordering, and
-- then remove the selected element from the current ordering.  After
-- computing the transitive closure, filter out the edges that are not
-- well ordered, i.e. originate at a reception node or terminate at a
-- transmission node.  Also, filter out edges that link nodes in the
-- same strand.  The preskeleton constructor function performs a
-- transitive reduction on the generated ordering.

weakenOrderings :: Algebra t p g s e c => Preskel t g s e ->
                   [Candidate t p g s e c]
weakenOrderings k =
    map (weakenOrdering k) ((orderings k) L.\\ (leadsto k))

weakenOrdering :: Algebra t p g s e c => Preskel t g s e ->
                  Pair -> Candidate t p g s e c
weakenOrdering k p =
    weaken k p (L.delete p (tc k))

weaken :: Algebra t p g s e c => Preskel t g s e ->
          Pair -> [Pair] -> Candidate t p g s e c
weaken k p orderings =
    addIdentity k'
    where
      k' = newPreskel (gen k) (shared k) (insts k)
           orderings (leadsto k) (decls k) (kfacts k)
           (Generalized (Weakened p)) (prob k) (kpriority k) (pov k)

-- Origination assumption forgetting

-- Delete each non-originating term that is not specified by a
-- role.  Do the same for each uniquely-originating term.

forgetAssumption :: Algebra t p g s e c => Preskel t g s e ->
                    [Candidate t p g s e c]
forgetAssumption k =
    forgetSomeTerm k

-- Variable separation

-- A location is a strand, a role variable, and a position in the term
-- associated with the role variable.

-- step one: extract places
--
-- for each maplet in each environment in each strand
--   for each variable V in the range of the maplet
--     let P be the places at which V occurs
--     associate V with the location described by each element in P
--
-- step two: generate preskeletons
--
-- for each variable V in the skeleton K
--  let V' be a clone of V
--  let L be the set of locations associated with V from above
--  for each subset L' of L
--    for each instance I in K
--      update the environment of I by replacing V' at the locations
--      given by L' that refer to I, and use the modified environment
--      to update the instance
--    let K' be the result of updating instances in K as above
--    if V occurs in non, add terms with V replaced by V' to
--      the non's of K'
--    if V occurs in unique, add terms with V replaced by V'
--      to the unique's of K'
--    add K' to the list of generated preskeletons

separateVariables :: Algebra t p g s e c => Preskel t g s e ->
                     [Candidate t p g s e c]
separateVariables k | M.isJust (pov k) =
    do
      let pk = M.fromJust (pov k)
      v <- kvars k
      case isNum v of
        True -> fail "separation of numeric values not supported"
        False -> separateVariable k pk
          (extractPlaces pk)
          (extractPlaces k) v
separateVariables _ | otherwise = []

-- A location is a strand, a role variable, and a position in the term
-- associated with the role variable.

type Location t p g s e c = (Sid, t, p)

-- A POV location is a location with the map from the POV.  It
-- consists of a POV variable, list of locations, and a position in
-- the term assoctiated with the POV variable.

type PovLocation t p g s e c = (t, [Location t p g s e c], p)

-- Returns a list of pairs.  For each occurrence of a preskeleton
-- variable in every instance, there is a pair where the first element
-- is the variable, and the second as the location at which it occurs.
extractPlaces :: Algebra t p g s e c => Preskel t g s e ->
                 [(t, Location t p g s e c)]
extractPlaces k =
    [ (var, (sid s, v, p)) |
      s <- strands k,
      (v, t) <- instAssocs (inst s),
      var <- foldVars (flip adjoin) [] t,
      p <- places var t ]

instAssocs :: Algebra t p g s e c => Instance t e -> [(t, t)]
instAssocs i =
    reify (rvars (role i)) (env i)

-- For each variable, generate candidates by generating a fresh
-- variable for subsets of the locations associated with the variable.
separateVariable :: Algebra t p g s e c => Preskel t g s e ->
                    Preskel t g s e ->
                    [(t, Location t p g s e c)] ->
                    [(t, Location t p g s e c)] -> t ->
                    [Candidate t p g s e c]
separateVariable k pk _ _ _
    | null (homomorphism pk k (prob k)) = [] -- HACK
separateVariable k pk povps ps v =
    sepVar rolelocs' povlocs
    where
      povhom = head (homomorphism pk k (prob k))  -- For completeness, should
      rolelocs = locsFor ps v                      -- use all homs.
      povlocs = povLocsFor k povps rolelocs
      povrolelocs = concatMap (\(_,x,_)->x) povlocs
      rolelocs' = rolelocs L.\\ povrolelocs
      sepVar [] [] = []
      sepVar locs plocs =
          do
            comblocs <- combparts locs plocs
            changeLocations k pk gen' v' comblocs povhom
      parts x = map (map (x !!)) (subsets (length x))
      combparts x y = [(xs, ys) | xs <- parts x,
                                  ys <- parts y,
                                  let n = length xs + length ys,
                                  n /= 0 && n /= length x + length y]
      (gen', v') = clone (gen k) v

-- Extract the locations for a given variable
locsFor :: Algebra t p g s e c => [(t, Location t p g s e c)] ->
           t -> [Location t p g s e c]
locsFor ps t =
    map snd (filter (\(t', _) -> t == t') (reverse ps)) -- Why reverse?

-- On input k e povps locs v, produces a list of pairs
-- (t, tps) where t is a pov variable and tps is a non-empty list of
-- locations in locs such that if (sid, t', p) is in tps, then sid is
-- in (prob k), and (t, (sid, t', p')) is in povps where p' is a prefix
-- of p.
povLocsFor :: Algebra t p g s e c => Preskel t g s e ->
              [(t, Location t p g s e c)] -> [Location t p g s e c] ->
              [PovLocation t p g s e c]
povLocsFor k povps locs =
   [ (t, tps, p) |
     povp <- map (probMapPOVps k) povps,
     Just p <- map (f povp) locs,
     let (t, _) = povp,
     let tps = [ loc |
                 loc <- locs,
                 M.isJust $ f povp loc ] ]
   where
     f (_, (sid', t', p')) (sid, t, p)  =
       if sid == sid' && t == t' then
         placeStripPrefix p' p
       else
         Nothing

matchAlways :: Algebra t p g s e c => String -> t -> t -> (g, e) -> (g, e)
matchAlways note t t' env =
    case match t t' env of
      e : _ -> e
      [] -> error ("Strand.matchAlways: bad match " ++ note)

probMapPOVps :: Algebra t p g s e c => Preskel t g s e ->
                (t, Location t p g s e c) -> (t, Location t p g s e c)
probMapPOVps k (t, loc) =
  (t, f loc)
  where
    f (s, t, p) | length (prob k) <= s =
      error ("Strand.probMapPOVps: bad s " ++ show s)
                | otherwise = (prob k !! s, t, p)

-- Change the given locations and create the resulting preskeleton
-- JDR: Something about facts is needed here.
changeLocations :: Algebra t p g s e c => Preskel t g s e ->
                   Preskel t g s e -> g -> t ->
                   ([Location t p g s e c], [PovLocation t p g s e c]) ->
                   e -> [Candidate t p g s e c]
changeLocations k pk gen t (rlocs, povlocs) pove =
    [addIdentity ks]
    where
      ks = f (splitDecls k pk gen' insts' t povlocs pove)
      locs = rlocs ++ concatMap (\(_,x,_)->x) povlocs
      f (gen'', newdecl) =
        newPreskel gen'' (shared k) insts' (orderings k) (leadsto k) newdecl
        (kfacts k)              -- This is wrong.  Facts need to be cleansed!
        (Generalized (Separated t)) (prob k) (kpriority k) (pov k)
      (gen', insts') = changeStrands locs t gen (strands k)

-- For variable separation
-- Exported
splitDecls :: Algebra t p g s e c => Preskel t g s e ->
              Preskel t g s e -> g ->
              [Instance t e] -> t -> [PovLocation t p g s e c] ->
              e -> (g, SkelDeclarations t)
splitDecls k pk gen insts copy povlocs pove =
    (gen', foldl addInstOrigs d (zip (nats (length insts)) insts))
    where
      -- povdecls: calculate from pov
      d = declsMap (instantiate pove') (nodeMap (prob k)) (decls pk)
      (gen', pove') = foldl f (gen, emptyEnv)
             (map (changePovMaplet povlocs copy) pova)
      f env (v, t) = matchAll v t env
      pova = reify (kvars pk) pove
      matchAll v' t' env' =
        case match v' t' env' of
          e : _ -> e
          [] -> error ("Strand.matchAlways: bad match " ++ show v' ++ "\n"
                       ++ show t' ++ "\n" ++ show (length pova) ++ "\n\n" ++
                       show pova ++ "\n\n" ++
                       show (map (changePovMaplet povlocs copy) pova))

changeStrands :: Algebra t p g s e c => [Location t p g s e c] -> t ->
                 g -> [Strand t e] -> (g, [Instance t e])
changeStrands locs copy gen strands =
    case foldMapM (changeStrand locs copy) gen strands of
      i : _ -> i
      [] -> error "Strand.changeStrands: bad strand build"

-- Create an new environment incorporating changes, and from that,
-- create the new strand.
changeStrand :: Algebra t p g s e c => [Location t p g s e c] ->
                t -> g -> Strand t e -> [(g, Instance t e)]
changeStrand locs copy gen s =
    bldInstance (role i) trace'  gen'
    where
      i = inst s
      (gen', env') = foldl f (gen, emptyEnv)
             (map (changeMaplet locs copy (sid s)) (instAssocs i))
      f env (v, t) = matchAlways "in changeStrand" v t env
      trace' = map (evtMap $ instantiate env') trace
      trace = take (height i) (rtrace (role i))

-- Change a maplet
changeMaplet :: Algebra t p g s e c => [Location t p g s e c] ->
                t -> Sid -> (t, t) -> (t, t)
changeMaplet [] _ _ maplet = maplet
changeMaplet ((s', v', p) : locs) copy s (v, t) =
    changeMaplet locs copy s (v, t')
    where
      t' = if s' == s && v' == v then replace copy p t else t

-- Change a maplet
changePovMaplet :: Algebra t p g s e c => [PovLocation t p g s e c] ->
                t -> (t, t) -> (t, t)
changePovMaplet [] _ maplet = maplet
changePovMaplet ((v', _, p) : locs) copy (v, t) =
    changePovMaplet locs copy (v, t')
    where
      t' = if v' == v then replace copy p t else t

-- Return the set of subsets of natural numbers less than n
subsets :: Int -> [[Int]]
subsets n
    | n < 0 = error $ "Utilities.subsets: Bad argument " ++ show n
    | n == 0 = []
    | otherwise =
        [n - 1] : subset ++ map (n - 1 :) subset
        where
          subset = subsets (n - 1)

-- Collapse a shape by unifying strands.

collapse :: Algebra t p g s e c => Preskel t g s e ->
            [Preskel t g s e]
collapse k =
    [k' | s <- strandids k, s' <- nats s,
          k' <- collapseStrands k s s']

collapseStrands :: Algebra t p g s e c => Preskel t g s e ->
                   Sid -> Sid -> [Preskel t g s e]
collapseStrands k s s' =
    do
      (s, s', subst) <- unifyStrands k s s'
      prs <- ksubst (k, k { operation = Collapsed s s' },
                           (0, 0), strandids k, emptySubst) subst
      prs <- compress True prs s s'
      prs <- skeletonize False prs
      return $ skel prs

------------------------------------------------------------------

-------------- Declarations and Constraints Section --------------

------------------------------------------------------------------

data ConstInfo t = ConstInfo
     {
       dkorig :: ![(t, [Node])],  -- This is an association list with a
                                  -- pair for each element of kunique.
                                  -- The value associated with a term
                                  -- is a list of the nodes at which it
                                  -- originates--the term's provenance.
       dkugen :: ![(t, [Node])] }  -- As korig but for uniq-gen terms.
     deriving Show

-- Exported
knon :: Preskel t g s e -> [t]
knon = dknon . decls

-- Exported
kpnon :: Preskel t g s e -> [t]
kpnon = dkpnon . decls

-- Exported
kunique :: Preskel t g s e -> [t]
kunique = dkunique . decls

kabsent :: Preskel t g s e -> [(t, t)]
kabsent = dkabsent . decls

-- Exported
korig :: Preskel t g s e -> [(t, [Node])]
korig = dkorig . cInfo

-- Exported
kugen :: Preskel t g s e -> [(t, [Node])]
kugen = dkugen . cInfo

-- Exported
kuniqgen :: Preskel t g s e -> [t]
kuniqgen k = map head $ filter (\ts -> not $ null ts) $ map dterms $ tagDecls "uniq-gen" $ decls k

{-- Exported
kuniqexp :: Algebra t p g s e c => Preskel t g s e -> [t]
kuniqexp k = map head $ filter (\ts -> not $ null ts) $ map dterms $ tagDecls "uniq-exp" $ decls k
--}

-- Exported
{-
nsdecls :: Declarations t l -> [Int]
nsdecls decls =
  [length (dknon decls), length (dkpnon decls), length (dkunique decls)]
-}

-- Exported
-- JDR: something about facts is needed here I think
forgetSomeTerm :: Algebra t p g s e c => Preskel t g s e ->
                  [Candidate t p g s e c]
forgetSomeTerm k =
  [ addIdentity (k { decls = d, operation = Generalized (Forgot dd) }) |
      (dd, d) <- forgetSomeDecls (decls k),
      let insts' = insts k,
      let rd = foldl addInstOrigs (declsUnion [])
               (zip (nats (length insts')) insts'),
      validateDeclMap rd d id emptyEnv ]

-- Returns that atoms that cannot be guess when determining if a
-- term is derivable from some other terms, and the atoms that
-- uniquely originate in this skeleton.
avoid :: Algebra t p g s e c => Preskel t g s e -> Set t
avoid k =
    avoidTerms (decls k)

{-- uniqOrig :: Algebra t p g s e c => Preskel t g s e -> [t]
uniqOrig k =
    do
      (t, [_]) <- reverse (korig k)
      return t --}

-- A preskeleton is well formed if the ordering relation is acyclic,
-- each atom declared to be uniquely-originating is carried in some
-- preskeleton term, and every variable that occurs in each base term
-- declared to be non-originating or pen-non-originating occurs in
-- some preskeleton term, and the atom must never be carried by any
-- term, and every uniquely originating role term mapped by an
-- instance is mapped to a term that originates on the instance's
-- strand.

-- Exported
preskelWellFormed :: Algebra t p g s e c => Preskel t g s e -> Bool
preskelWellFormed k =
    varSubset (knon k) terms &&
    varSubset (kpnon k) terms &&
    all nonCheck (knon k) &&
    all uniqueCheck (kunique k) &&
    all factCheck (kfacts k) &&
    origNonNullCheck k && ugenNonNullCheck k &&
    wellOrdered k && acyclicOrder k &&
    roleOrigCheck k && fst (declCheck (decls k))
    where
      terms = kterms k
      mesgterms = kmesgterms k
      nonCheck t = all (not . carriedBy t) mesgterms
      uniqueCheck t = any (carriedBy t) mesgterms
      factCheck f = factVarsElem (kvars k) f
      origNonNullCheck k = (all (\(_, ns) -> not (null ns)) (korig k))
      ugenNonNullCheck k = (all (\(_, ns) -> not (null ns))
                            (filter (\(t, _) -> not (isNum t)) (kugen k)))

-- Do notation friendly preskeleton well formed check.
wellFormedPreskel :: (MonadFail m, Algebra t p g s e c) =>
                     Preskel t g s e -> m (Preskel t g s e)
wellFormedPreskel k
    | preskelWellFormed k = return k
    | otherwise = fail "preskeleton not well formed"

-- Exported
-- A version of preskelWellFormed that explains why a preskeleton is
-- not well formed.
verbosePreskelWellFormed :: (MonadFail m, Algebra t p g s e c) =>
                            Preskel t g s e -> m ()
verbosePreskelWellFormed k =
    do
      failwith "a variable in non-orig is not in some trace"
                   $ varSubset (knon k) terms
      failwith "a variable in pen-non-orig is not in some trace"
                   $ varSubset (kpnon k) terms
      mapM_ nonCheck $ knon k
      mapM_ uniqueCheck $ kunique k
      mapM_ factCheck $ kfacts k
      origNonNullCheck k
      uniqgenNonNullCheck k
      failwith "ordered pairs not well formed" $ wellOrdered k
      failwith "cycle found in ordered pairs" $ acyclicOrder k
      failwith "an inherited unique doesn't originate in its strand"
                   $ roleOrigCheck k
      let chk = declCheck $ decls k
      failwith (snd chk) (fst chk)
    where
      terms = kterms k
      mesgterms = kmesgterms k
      nonCheck t =
          failwith (showString "non-orig " $ showst t " carried")
                       $ all (not . carriedBy t) mesgterms
      uniqueCheck t =
          failwith (showString "uniq-orig " $ showst t " not carried")
                       $ any (carriedBy t) mesgterms
      factCheck f =
          failwith ("a fact var in " ++  factPred f ++ " not in some strand")
                       $ factVarsElem (kvars k) f
      origNonNullCheck k =
          let ts = filter (\(_, ns) -> null ns) (korig k) in
          failwith (showString "uniq-orig " $
                    showst (fst $ head ts) " does not originate")
          (null ts)
      uniqgenNonNullCheck k =
          let ts = filter (\(_, ns) -> null ns)
                   (filter (\(t, _) -> not (isNum t)) (kugen k)) in
          failwith (showString "uniq-gen " $
                    showst (fst $ head ts) " does not generate")
          (null ts)

-- Do notation friendly preskeleton well formed check.
reducedWellFormed :: MonadFail m => Preskel t g s e -> m ()
reducedWellFormed k
  | reducedWellOrdered k = return ()
  | otherwise = fail "reduced preskeleton not well formed"

-- A transition or an observer node should have at most one transition
-- node immediately after it.
noStateSplit :: MonadFail m => Preskel t g s e -> m ()
noStateSplit k
  | loop (leadsto k) S.empty = return ()
  | otherwise = fail "reduced preskeleton has a state split"
  where
    loop [] _ = True
    loop ((n0, (s, i)) : es) ns =
      case (trace (insts k !! s) !! i) of
        Sync (Tran (Just _, Just _)) ->
          S.notMember n0 ns && loop es (S.insert n0 ns)
        _ -> loop es ns

---------------------------------------
-- Constraints
---------------------------------------

-- Ensure each role unique origination assumption mapped by an
-- instance originates in the instance's strand.
roleOrigCheck :: Algebra t p g s e c => Preskel t g s e -> Bool
roleOrigCheck k =
    all declOrigCheck (dkuniqFull $ decls k)
    && all declUGenCheck (filter nonNum $ (dkugenFull $ decls k))
    where
      declOrigCheck (u, n) =
         case lookup u (korig k) of
           Nothing -> error "Strand.roleOrigCheck: u not found in korig"
           Just ns -> any (==n) ns
      declUGenCheck (u, Just n) =
         case lookup u (kugen k) of
           Nothing -> error "Strand.roleUGenCheck: u not found in kugen"
           Just ns -> any (==n) ns
      declUGenCheck (_, Nothing) = True
                 --error "Strand.roleUGenCheck: u generation point not found in kugen"
      nonNum (u, _) = not (isNum u)

showst :: Algebra t p g s e c => t -> ShowS
showst t =
    shows $ displayTerm (addToContext emptyContext [t]) t

-- This is the starting point of the Preskeleton Reduction System
-- This code is structured a bit strangely, in order to not rewrite
-- the existing PRS code.  "coreAlg" is the rectification algorithm
-- that might or might not use deorigination, pruning, thinning, etc.
-- "rectifiableConstraintCheck" returns True on a PRS that already
-- meets all rectifiable constraints and false otherwise.  "rectify"
-- takes a PRS and outputs a [PRS] that rectifies failed rectifiable
-- constraints.
--
-- The idea is to run the coreAlg first.  Any PRS'es in the result
-- are left alone if the rectifiable constraint check passes, and
-- replaced with the result of rectify-and-then-skeletonize otherwise.
skeletonize :: Algebra t p g s e c => Bool -> PRS t p g s e c ->
               [PRS t p g s e c]
skeletonize prune prs =
    skeletonize2 prune 5 prs

enforceAbsence :: Algebra t p g s e c => PRS t p g s e c -> [PRS t p g s e c]
enforceAbsence prs@(_, k, _, _, _) =
  [prs' | s <- absenceSubst (gen k) (kabsent k), prs' <- ksubst prs s]

skeletonize2 :: Algebra t p g s e c => Bool -> Int -> PRS t p g s e c ->
               [PRS t p g s e c]
skeletonize2 _ 0 prs = [prs]
skeletonize2 prune iter prs =
  concatMap maybeRectify coreResult
  where
      coreResult = concatMap coreAlg (enforceAbsence prs)
      coreAlg
        | useDeOrigination = hull prune
        | otherwise = normalAlg
      normalAlg prs | hasMultipleOrig prs = []
                    | otherwise = (enrich prune prs)
      maybeRectify prs
                | rectifiableConstraintCheck (skel prs) = [prs]
                | otherwise = concatMap (skeletonize2 prune (iter-1)) (rectify prs)

-- rectifiableConstraintCheck: outputs True if all rectifiable
-- constraints are true of the input skeleton.
rectifiableConstraintCheck :: Algebra t p g s e c => Preskel t g s e
                              -> Bool
rectifiableConstraintCheck k
  | not (null (rectUnifications k)) = False
  | not (indicatorConstraintsCheck k) = False
  | otherwise = True

-- indicatorConstraintsCheck: outputs True iff all indicator constraints are
-- currently met.
indicatorConstraintsCheck :: Algebra t p g s e c => Preskel t g s e -> Bool
indicatorConstraintsCheck k =
  (indZeroCheck && indZeroInCheck)
  where
    indZeroCheck = all (\(t,v) -> t == v || indicator t v == Just 0) (pairs indZeroTerms avoidExpVars)
    indZeroTerms = map head (map dterms $ tagDecls "ind-zero" (decls k))
    avoidExpVars = filter isNum (S.toList $ avoid k)
    indZeroInCheck = all (\(t,v) -> indicator t v == Just 0) indZeroInPairs
    indZeroInPairs = map (\ts -> (ts !! 0, ts !! 1)) (map dterms $ tagDecls "ind-zero-in" (decls k))
    pairs x y = [(a,b)|a <- x, b <- y]

-- rectify: rectify rectifiable constraints.
rectify :: Algebra t p g s e c => PRS t p g s e c -> [PRS t p g s e c]
rectify prs =
   concatMap rectifyIndicatorConstraints $
   doUnifs prs
   where
     doUnifs prs
       | null $ unifs prs = [prs]
       | otherwise = concatMap (ksubst prs) (rectUnifs (unifs prs) (gen (skel prs), emptySubst))
     unifs prs = rectUnifications (skel prs)
     rectUnifs [] sigma = [sigma]
     rectUnifs ((t1,t2):rest) sigma =
       do
         s <- unify t1 t2 sigma
         rectUnifs rest s

-- sklyynch: cancel a numeric variable from points earlier than its
-- generation point.
rectifyIndicatorConstraints :: Algebra t p g s e c => PRS t p g s e c -> [PRS t p g s e c]
rectifyIndicatorConstraints prs
  | indicatorConstraintsCheck (skel prs) = [prs]
  | otherwise = [prs' | ge <- gefix,
                        prs' <- recurse1 (ind0Problems (skel prs)) prs ge]
  where
    -- Rectify ind-zero constraints until none are left, then rectify
    -- ind-zero-in constraints.
    recurse1 [] prs ge = recurse2 (ind0_inProblems (skel prs)) prs ge
    recurse1 (t:ts) prs ge =
      [prs' | (g,e) <- zeroIndicator t ge (filter (/= t) $ resExps (skel prs)),
              sprs <- ksubst prs (g, substitution e),
              prs' <- recurse1 (map (substitute (substitution e)) ts) sprs (g,e)]

    -- Rectify ind-zero-in constraints until none are left
    recurse2 [] prs _ = [prs]
    recurse2 (tv:tvs) prs ge =
      [prs' | (g,e) <- zeroIndicatorIn (tv !! 1) (tv !! 0) ge,
              sprs <- ksubst prs (g,substitution e),
              prs' <- recurse2 (map (\tv -> map (substitute (substitution e)) tv) tvs) sprs (g,e)]

    -- Include restricted variables
    gefix = matchMany (resExps (skel prs)) (resExps (skel prs)) (gen (skel prs), emptyEnv)
    resExps k = (filter isNum $ S.toList (avoid k))

    ind0Problems k = filter (\t -> ind0ProblemCheck t (resExps k))
                  (map head $ map dterms $ tagDecls "ind-zero" (decls k))
    ind0_inProblems k = filter (\ts -> indicator (ts !! 1) (ts !! 0) /= Just 0)
                  (map dterms $ tagDecls "ind-zero-in" (decls k))
    ind0ProblemCheck t av = any (\v -> indicator t v /= Just 0) (filter (/= t) av)

-- rectUnifications: list required, non-trivial unification for
-- rectifying k
rectUnifications :: Algebra t p g s e c => Preskel t g s e -> [(t,t)]
rectUnifications k = fnofUnifications k

-- fnofUnifications: list unifications required by fnof constraint
fnofUnifications :: Algebra t p g s e c => Preskel t g s e -> [(t,t)]
fnofUnifications k =
   reducePairs [(head $ dterms ti1, head $ dterms ti2) |
          ti1 <- fnofDecls, ti2 <- fnofDecls, daux ti1 == daux ti2,
          (head $ dterms ti1) /= (head $ dterms ti2),
          ((dterms ti1) !! 1) == ((dterms ti2) !! 1)]
   where
     -- Relies every fn-of declaration including at least 2 entries in dterms.
     fnofDecls = filter (\d -> (length $ dterms d) >= 2) $ tagDecls "fn-of" (decls k)
     -- removes duplicates and flips
     reducePairs [] = []
     reducePairs ((a,b):rest) = ((a,b):(reducePairs (filter (\ (c,d) ->
                              ((c,d) /= (a,b)) && ((c,d) /= (b,a))) rest)))

-- Determine if a given PRS has a multiple origination of a
-- non-numeric fresh value.
hasMultipleOrig :: PRS t p g s e c -> Bool
hasMultipleOrig prs =
  any (\(_, l) -> length l > 1) (korig (skel prs)) ||
  any (\(_, l) -> length l > 1) (kugen (skel prs))

checkOrigs :: Algebra t p g s e c => Gist t g ->
              Gist t g -> (g, e) -> [Sid] -> Bool
checkOrigs g g' env perm =
  declCheckOrigs (declsMapLocations (permuteNode perm) $ gdecls g) (gdecls g') env

-- Exported
declsOrig :: Algebra t p g s e c => [Strand t e] ->
             Declarations t l -> ConstInfo t
declsOrig strands d =
  ConstInfo { dkorig = map (originationNodes strands) (dkunique d),
              dkugen = map (generationNodes strands) (dkuniqgen d)}

-- Exported
declsMapStrands :: Algebra t p g s e c => [Sid] ->
                   SkelDeclarations t ->
                   SkelDeclarations t
declsMapStrands mapping d = declsMapLocations (nodeMap mapping) d

-- Exported
inheritRdecls :: Algebra t p g s e c => Sid -> Instance t e ->
                 SkelDeclarations t -> SkelDeclarations t
inheritRdecls s inst d =
  declsUnion [inherit s inst (ridecls (role inst)), d]

inherit :: Algebra t p g s e c => Sid -> Instance t e ->
           [(RoleDeclarations t, Int)] ->
           SkelDeclarations t
inherit s i ridecls =
    declsUnion (map g $ filter f ridecls)
    where
      f (_, pos) = pos < height i
      g = declsMap (instantiate (env i)) h . fst
      h j = (s, j)

declsMap :: (Algebra t p g s e c, Loc l, Loc l') => (t->t) -> (l->l') ->
            Declarations t l -> Declarations t l'
declsMap tf lf = declsMapLocations lf . declsMapTerms tf

-- Order Enrichment

-- Adds orderings so that a skeleton respects origination.

enrich :: Algebra t p g s e c => Bool -> PRS t p g s e c ->
          [PRS t p g s e c]
enrich prune (k0, k, n, phi, hsubst) =
    if length o == length (orderings k) then
        maybeThin prune (k0, k, n, phi, hsubst) -- Nothing to add
    else
        do
          k'' <- wellFormedPreskel k'
          maybeThin prune (k0, k'', n, phi, hsubst)
    where
      o = addOrderings k
      k' = newPreskel
                  (gen k)
                  (shared k)
                  (insts k)
                  o
                  (leadsto k)
                  (decls k)
                  (kfacts k)
                  (operation k)
                  (prob k)
                  (kpriority k)
                  (pov k)

origNode :: Algebra t p g s e c => t -> [(t,[Node])] -> Maybe Node
origNode t origlist =
    case lookup t origlist of
      Nothing -> Nothing
      Just [] -> Nothing
      Just [n] -> Just n
      Just _ -> error "Strand.origNode: not a hulled skeleton"

addOrderings :: Algebra t p g s e c => Preskel t g s e ->
                [Pair]
addOrderings k =
  addSyncOrderings (map foo (leadsto k)) orderings''
  where
    orderings' = foldl (addUniqOrigOrderings k) (orderings k) (kunique k)
    orderings'' = foldl (addUniqGenOrderings k) orderings' (kuniqgen k)
    foo (p1, p2) = ((p1,p2), (f p1, f p2))
    f (s,n) = (trace (insts k !! s) !! n)

addUniqOrigOrderings :: Algebra t p g s e c => Preskel t g s e ->
                [Pair] -> t -> [Pair]
addUniqOrigOrderings k orderings t =
  case origNode t (korig k) of
      Nothing -> orderings
      Just n@(s, _) ->
          foldl f orderings (L.delete s (strandids k))
          where
            f orderings s =
                case gainedPos t (trace (strandInst k s)) of
                  Nothing -> orderings
                  Just pos -> adjoin (n, (s, pos)) orderings

addUniqGenOrderings :: Algebra t p g s e c => Preskel t g s e ->
                [Pair] -> t -> [Pair]
addUniqGenOrderings k orderings t =
  case origNode t (kugen k) of
      Nothing -> orderings
      Just n@(s, _) ->
          foldl f orderings (L.delete s (strandids k))
          where
            f orderings s =
                case genGainedPos t (trace (strandInst k s)) of
                  Nothing -> orderings
                  Just pos -> adjoin (n, (s, pos)) orderings

-- Add implied edges from an observer
addSyncOrderings :: Algebra t p g s e c => [(Pair, (Event t,Event t))] -> [Pair] -> [Pair]
addSyncOrderings es orderings =
  foldl f orderings es
  where
    f acc (p, (Sync t0, Sync t1)) = -- t0 transforms, t1 observes
      if (path t0 && observer t1) then
        -- Need to look for a pair (p', (e1, e2)) in xform s.t. e1 = Sync t0
            if (null (h t0 xform)) then acc else ((snd p, snd (fst ((h t0 xform) !! 0))) : acc)
                                                 else acc
    f acc _ = acc
    xform = filter g es         -- Edges that transform
    g (_, (Sync t@(Tran (_, Just _)), Sync t'@(Tran (Just _, Just _)))) =
      next t == now t'
    g _ = False
    h t0 pes =
      filter (\(_,(t1,_))->t1==Sync t0) pes

nodeMap :: [Sid] -> Node -> Node
nodeMap mapping (s, i) =
  if s < length mapping && s >= 0 then
    (mapping !! s, i)
  else
    error ("Strand.nodeMap: bad " ++ show s ++ " " ++ show mapping)

-- Exported
validateDeclEnv :: Algebra t p g s e c => Preskel t g s e ->
                   Preskel t g s e -> [Sid] -> e -> Bool
validateDeclEnv k k' mapping env =
    validateDeclMap (decls k) (decls k') (nodeMap mapping) env

-- Hulling or Ensuring Unique Origination
hull :: Algebra t p g s e c => Bool -> PRS t p g s e c ->
        [PRS t p g s e c]
hull prune prs =
    loop (korig $ skel prs)
    where
      -- No uniques originate on more than one strand
      loop [] = enrich prune prs
      -- Found a pair that needs hulling
      loop ((u, (s, i) : (s', i') : _) : _) =
              hullByDeOrigination prune prs u (s, i) (s', i')
      loop(_ : orig) = loop orig

-- De-Origination

hullByDeOrigination :: Algebra t p g s e c => Bool -> PRS t p g s e c ->
                       t -> Node -> Node -> [PRS t p g s e c]
hullByDeOrigination  prune prs u (s, i) (s', i') =
    do
      subst <- deOrig (skel prs) u (s, i) ++ deOrig (skel prs) u (s', i')
      prs <- ksubst prs subst
      hull prune prs

deOrig :: Algebra t p g s e c => Preskel t g s e -> t -> Node -> [(g, s)]
deOrig k u (s, i) =
    [ (g, s) |
      let tr = trace $ strandInst k s,
      e <- take i tr,
      t <- M.maybeToList $ recvTerm e,
      subterm <- S.toList $ foldCarriedTerms (flip S.insert) S.empty t,
      (g, s) <- unify u subterm (gen k, emptySubst),
      not $ originates (substitute s u) (map (evtMap $ substitute s) tr) ]

type SkelDeclInst t = DeclInst t (Int, Int)
type SkelDeclInstList t = DeclInstList t (Int, Int)
type SkelDeclaration t = Declaration t (Int, Int)
type SkelDeclList t = DeclList t (Int, Int)
type SkelDeclarations t = Declarations t (Int, Int)

-- Facts

data FTerm t
  = FSid Sid
  | FTerm t
  deriving (Eq, Show)

data Fact t
  = Fact String [FTerm t]
  deriving (Eq, Show)

factPred :: Fact t -> String
factPred (Fact pred _) = pred

substFTerm :: Algebra t p g s e c => s -> FTerm t -> FTerm t
substFTerm s (FTerm t) = FTerm $ substitute s t
substFTerm _ t = t

substFact :: Algebra t p g s e c => s -> Fact t -> Fact t
substFact s (Fact name fs) = Fact name $ map (substFTerm s) fs

{-
instFTerm :: Algebra t p g s e c => e -> FTerm t -> FTerm t
instFTerm s (FTerm t) = FTerm $ instantiate s t
instFTerm _ t = t

instFact :: Algebra t p g s e c => e -> Fact t -> Fact t
instFact s (Fact name fs) = Fact name $ map (instFTerm s) fs
-}

updateFTerm :: (Sid -> Sid) -> FTerm t -> FTerm t
updateFTerm f (FSid s) = FSid $ f s
updateFTerm _ t = t

updateFact :: (Sid -> Sid) -> Fact t -> Fact t
updateFact f (Fact name fs) = Fact name $ map (updateFTerm f) fs

instUpdateFTerm :: Algebra t p g s e c => e ->
                   (Sid -> Sid) -> FTerm t -> FTerm t
instUpdateFTerm _ f (FSid s) = FSid $ f s
instUpdateFTerm e _ (FTerm t) = FTerm $ instantiate e t

instUpdateFact :: Algebra t p g s e c => e ->
                  (Sid -> Sid) -> Fact t -> Fact t
instUpdateFact e f (Fact name fs) = Fact name $ map (instUpdateFTerm e f) fs

-- Is all of the fact's variables in a list of variables?

factVarsElem :: Algebra t p g s e c => [t] -> Fact t -> Bool
factVarsElem vs (Fact _ ts) =
  all f ts
  where
    f (FSid _) = True
    f (FTerm t) = all (\v -> elem v vs) (varsInTerms [t])

cleansFacts :: Algebra t p g s e c => [t] -> [Fact t] -> [Fact t]
cleansFacts vs facts =
  L.filter (factVarsElem vs) facts

kfactVars :: Algebra t p g s e c => Preskel t g s e -> Set t
kfactVars k =
  foldl (foldFactTerms $ foldVars $ flip S.insert) S.empty (kfacts k)

foldFactTerms :: Algebra t p g s e c => (a -> t -> a) -> a -> Fact t -> a
foldFactTerms f z (Fact _ ts) =
  foldl g z ts
  where
    g a (FSid _) = a
    g a (FTerm t) = f a t

{-
instvars :: Algebra t p g s e c => [Instance t e] -> [t]
instvars insts =
    S.elems $ foldl addIvars S.empty insts
-}

{-- For debugging
factVars :: Algebra t p g s e c => Fact t -> [t] -> [t]
factVars (Fact _ ts) vs =
  foldr f vs ts
  where
    f (FSid _) vs = vs
    f (FTerm t) vs = addVars vs t

chkFVars :: Algebra t p g s e c => Preskel t g s e -> Preskel t g s e
chkFVars k =
  foldl f k (foldr factVars [] (kfacts k))
  where
    f k v
      | elem v (kvars k) = k
      | otherwise = error ("Bad var in fact " ++ show v)
        -- ++ "\n\n" ++ show k)

chkFVarsS :: Algebra t p g s e c => String -> Preskel t g s e -> Preskel t g s e
chkFVarsS s k =
  foldl f k (foldr factVars [] (kfacts k))
  where
    f k v
      | elem v (kvars k) = k
      | otherwise = error (s ++ ": Bad var in fact " ++ show v)
        -- ++ "\n\n" ++ show k)
--}

{-
chkFacts :: Preskel -> Bool
chkFacts k =
  all checkFact (kfacts k)
  where
    checkFact (Fact _ ft) =
      all checkFTerm ft
    checkFTerm (FSid s) =
      s < nstrands k
    checkFTerm (FNode (s, _)) =
      s < nstrands k
    checkFTerm (FTerm _) = True

-- Use this to find bad fact updates.
chkFacts :: Preskel -> Bool
chkFacts k =
  all checkFact (kfacts k)
  where
    checkFact (Fact _ ft) =
      all checkFTerm ft
    checkFTerm (FSid s) | s >= nstrands k = error "Bad strand in fact"
    checkFTerm (FNode (s, _)) | s >= nstrands k = error "Bad node in fact"
    checkFTerm _ = True

-}
-- Security goals: satisfaction of atomic formulas

type Sem t g s e = Preskel t g s e -> (g, e) -> [(g, e)]

satisfy :: Algebra t p g s e c => AForm t -> Sem t g s e
satisfy (Equals t t') = geq t t'
satisfy (AFact name fs) = gafact name fs
satisfy (Non t) = gnon t
satisfy (Pnon t) = gpnon t
satisfy (Uniq t) = guniq t
satisfy (UniqAt t n) = guniqAt t n
satisfy (UgenAt t n) = gugenAt t n
satisfy (Ugen t) = gugen t
satisfy (Prec n n') = gprec n n'
satisfy (LeadsTo n n') = gleadsTo n n'
satisfy (Length r n i) = glength r n i
satisfy (Param r v f n t) = gparam r v f n t

-- Equality assumes there has been a static role specific check to
-- eliminate error cases.
geq :: Algebra t p g s e c => t -> t -> Sem t g s e
geq t t' _ (g, e)
  -- Ensure all variables in t and t' are in the domain of e.
  -- This always happens for goals because they must be role specific
  -- but it is not always true for rules.
  | not (matched e t) || not (matched e t') =
      error ("In a rule equality check, " ++
             "cannot find a binding for some variable")
  | ti == ti' = [(g, e)]
  | otherwise = []
  where
    ti = instantiate e t
    ti' = instantiate e t'

-- Facts
gafact :: Algebra t p g s e c => String -> [t] -> Sem t g s e
gafact name fs k e =
  do
    Fact name' ts <- kfacts k
    case name == name' of
      True -> fmatchList fs ts e
      False -> []

fmatchList :: Algebra t p g s e c => [t] -> [FTerm t] -> (g, e) -> [(g, e)]
fmatchList [] [] e = [e]
fmatchList (f : fs) (t : ts) e =
  do
    e <- fmatch f t e
    fmatchList fs ts e
fmatchList _ _ _ = []

fmatch :: Algebra t p g s e c => t -> FTerm t -> (g, e) -> [(g, e)]
fmatch z (FSid s) e =
  strdMatch z s e
fmatch t (FTerm t') e =
  match t t' e

-- Non-origination
gnon :: Algebra t p g s e c => t -> Sem t g s e
gnon t k e =
  do
    t' <- dknon $ decls k
    match t t' e

-- Penetrator non-origination
gpnon :: Algebra t p g s e c => t -> Sem t g s e
gpnon t k e =
  do
    t' <- dkpnon $ decls k
    match t t' e

-- Unique origination
guniq :: Algebra t p g s e c => t -> Sem t g s e
guniq t k e =
  do
    t' <- kunique k
    match t t' e

-- Unique origination at a node
guniqAt :: Algebra t p g s e c => t -> NodeTerm t -> Sem t g s e
guniqAt t (z, i) k e =
  do
    (t', (s, j)) <- dkuniqFull $ decls k
    case i == j of
      True ->
        do
          e <- match t t' e
          strdMatch z s e
      _ -> []

-- Unique generation at a node
gugenAt :: Algebra t p g s e c => t -> NodeTerm t -> Sem t g s e
gugenAt t (z, i) k e =
  do
    (t', l) <- dkugenFull $ decls k
    case l of
      Just (s, j) | i == j ->
        do
          e <- match t t' e
          strdMatch z s e
      _ -> []

-- Unique generation
gugen :: Algebra t p g s e c => t -> Sem t g s e
gugen t k e =
  do
    t' <- dkuniqgen $ decls k
    match t t' e

inSkel :: Preskel t g s e -> (Int, Int) -> Bool
inSkel k (s, i) =
  s >= 0 && s < nstrands k && i >= 0 && i < height (insts k !! s)

strandPrec :: Node -> Node -> Bool
strandPrec (s, i) (s', i')
  | s == s' && i < i' = True
  | otherwise = False

nodeLookup :: Algebra t p g s e c => e -> NodeTerm t -> Maybe Node
nodeLookup e (z, i) =
  do
    s <- strdLookup e z
    return (s, i)

nodeMatch :: Algebra t p g s e c => NodeTerm t -> Node -> (g, e) -> [(g, e)]
nodeMatch (z, i) (s, j) e
  | i == j = strdMatch z s e
  | otherwise = []

-- Node precedes
-- This should look at the transitive closure of the ordering graph.
gprec :: Algebra t p g s e c => NodeTerm t -> NodeTerm t -> Sem t g s e
gprec n n' k (g, e) =
  case (nodeLookup e n, nodeLookup e n') of
    (Just p, Just p')
      | inSkel k p && inSkel k p' &&
        (strandPrec p p' || elem (p, p') tc) -> [(g, e)]
    _ ->
      do
        (p, p') <- tc
        (g, e) <- nodeMatch n p (g, e)
        nodeMatch n' p' (g, e)
  where
    tc = map graphPair $ graphClose $ graphEdges $ strands k

-- Node leads to
gleadsTo :: Algebra t p g s e c => NodeTerm t -> NodeTerm t -> Sem t g s e
gleadsTo n n' k (g, e) =
  case (nodeLookup e n, nodeLookup e n') of
    (Just p, Just p')
      | inSkel k p && inSkel k p' && elem (p, p') (leadsto k) -> [(g, e)]
    _ ->
      do
        (p, p') <- leadsto k
        (g, e) <- nodeMatch n p (g, e)
        nodeMatch n' p' (g, e)

-- Length predicate
-- r and h determine the predicate, which has arity one.
glength :: Algebra t p g s e c => Role t -> t -> Int -> Sem t g s e
glength r z h k (g, e) =
  case strdLookup e z of
    Nothing ->
      do
        (s, inst) <- zip [0..] $ insts k
        case () of
          _ | h > height inst -> []
            | rname (role inst) == rname r -> strdMatch z s (g, e)
            | otherwise ->      -- See if z could have been an instance of r
              case bldInstance r (take h $ trace inst) g of
                [] -> []
                _ -> strdMatch z s (g, e)
    Just s | s < nstrands k ->
      let inst = insts k !! s in
      case () of
        _ | h > height inst -> []
          | rname (role inst) == rname r -> [(g, e)]
          | otherwise ->
            case bldInstance r (take h $ trace inst) g of
              [] -> []
              _ -> [(g, e)]
    _ -> []

-- Parameter predicate

-- r and t determine the predicate, which has arity two.  t must be
-- a variable declared in role r.
gparam :: Algebra t p g s e c => Role t -> t -> Int -> t -> t -> Sem t g s e
gparam r t h z t' k (g, e) =
  case strdLookup e z of
    Nothing ->
      do
        (s, inst) <- zip [0..] $ insts k
        case () of
          _ | h > height inst -> []
            | rname (role inst) == rname r ->
              do
                ge <- strdMatch z s (g, e)
                match t' (instantiate (env inst) t) ge
            | otherwise ->      -- See if z could have been an instance of r
                do
                  (g, inst) <- bldInstance r (take h $ trace inst) g
                  ge <- strdMatch z s (g, e)
                  match t' (instantiate (env inst) t) ge
    Just s | s < nstrands k  ->
      let inst = insts k !! s in
      case () of
        _ | h > height inst -> []
          | rname (role inst) == rname r ->
            match t' (instantiate (env inst) t) (g, e)
          | otherwise ->
              do
                (g, inst) <- bldInstance r (take h $ trace inst) g
                match t' (instantiate (env inst) t) (g, e)
    _ -> []

-- Conjunction

conjoin :: Algebra t p g s e c => [AForm t] -> Sem t g s e
conjoin [] _ e = [e]
conjoin (a: as)  k e =
  do
    e <- satisfy a k e
    conjoin as k e

-- Satisfaction

-- Returns the environments that show satifaction of the antecedent
-- but fail to be extendable to show satifaction of one of the
-- conclusions.
goalSat :: Algebra t p g s e c => Preskel t g s e -> Goal t -> (Goal t, [e])
goalSat k g =
  (g, [ e |
        (gen, e) <- conjoin (antec g) k (gen k, emptyEnv),
        conclusion (gen, e) ])
  where
    conclusion e = all (disjunct e) $ concl g
    disjunct e a = null $ conjoin a k e

sat :: Algebra t p g s e c => Preskel t g s e -> [(Goal t, [e])]
sat k =
  map (goalSat k) (kgoals k)

-- Rules

-- Try simplifying k if possible
simplify :: Algebra t p g s e c => Preskel t g s e -> [Preskel t g s e]
simplify k =
  case rewrite k of
    Nothing -> [k]
    Just ks -> ks

{-
-- Try all rules associated with the protocol of k.  Return nothing if
-- no rule applies, otherwise return the replacements.
rewrite :: Algebra t p g s e c => Preskel t g s e -> Maybe [Preskel t g s e]
rewrite _ = Nothing
-}

-- Try all rules associated with the protocol of k.  Return nothing if
-- no rule applies, otherwise return the replacements.
rewrite :: Algebra t p g s e c => Preskel t g s e -> Maybe [Preskel t g s e]
rewrite k =
  loop prules
  where
    prules = rules $ protocol k
    loop [] = Nothing           -- No rules apply
    loop (r : rs) =
      let vas = tryRule k r in
        if null vas then
          loop rs               -- Rule does not apply
        else
          Just $ doRewrites prules k r vas

-- Returns the environments that show satifaction of the antecedent
-- but fail to be extendable to show satifaction of one of the
-- conclusions.
tryRule :: Algebra t p g s e c => Preskel t g s e -> Rule t -> [(g, e)]
tryRule k r =
  [(g, e) | (g, e) <- conjoin (antec $ rlgoal r) k (gen k, emptyEnv),
            conclusion (g, e) ]
  where
    conclusion e = all (disjunct e) $ concl $ rlgoal r
    disjunct e a = null $ conjoin a k e

ruleLimit :: Int
ruleLimit = 500

-- Repeatedly applies rules until no rule applies.
doRewrites :: Algebra t p g s e c => [Rule t] -> Preskel t g s e ->
              Rule t -> [(g, e)] -> [Preskel t g s e]
doRewrites rules k r vas =
  doRewritesLoop rules k (length vas) (doRewrite k r vas) []

doRewritesLoop :: Algebra t p g s e c => [Rule t] -> Preskel t g s e ->
                  Int -> [Preskel t g s e] -> [Preskel t g s e] ->
                  [Preskel t g s e]
doRewritesLoop _ _ lim _ _
  | lim >= ruleLimit =
    error ("Aborting after applying " ++ show ruleLimit ++
           " rules and more are applicable")
doRewritesLoop _ _ _ [] ks = reverse ks
doRewritesLoop rules k lim (k' : todo) ks =
  loop rules
  where
    loop [] =                   -- No rules apply
      doRewritesLoop rules k lim todo (k' : ks)
    loop (r : rs) =
      let vas = tryRule k' r in
        if null vas then
          loop rs               -- Rule does not apply
        else
          let new = doRewrite k' r vas in
            doRewritesLoop rules k (lim + length vas) (todo ++ new) ks

-- Apply rewrite rule at all assignments
doRewrite :: Algebra t p g s e c => Preskel t g s e ->
             Rule t -> [(g, e)] -> [Preskel t g s e]
doRewrite k r vas =
  concatMap (doRewriteOne k r) vas

-- Apply rewrite rule at one assignment
doRewriteOne :: Algebra t p g s e c => Preskel t g s e ->
                Rule t -> (g, e) -> [Preskel t g s e]
doRewriteOne k r e =
  do
    (evars, cl) <- consq $ rlgoal r
    let e' = foldl fresh e evars
    (k, _) <- doConj (rlname r) cl k e'
    k <- wellFormedPreskel k
    k <- toSkeleton True k
    return $ f k                -- Add comment about rule application
  where f k = k { kcomment =
                  L () [S () "rule", S () (rlname r)] :
                  kcomment k }

fresh :: Algebra t p g s e c => (g, e) -> t -> (g, e)
fresh (g, e) t
  | isStrdVar t = (g, e)
  | otherwise =
      case match t t' (g', e) of
        e' : _ -> e'
        [] -> error "Strand.fresh: Cannot match logical variable to clone"
    where
      (g', t') = clone g t

type Rewrite t g s e =
  Preskel t g s e -> (g, e) -> [(Preskel t g s e, (g, e))]

doConj :: Algebra t p g s e c => String -> [AForm t] -> Rewrite t g s e
doConj _ [] k e = [(k, e)]
doConj rule (f : fs) k e =
  do
    (k, e) <- rwt rule f k e
    doConj rule fs k e

rwt :: Algebra t p g s e c => String -> AForm t -> Rewrite t g s e
rwt _ (Length r z h) = rlength r z h
rwt rule (Param r v i z t) = rparam rule r v i z t
rwt rule (Prec n n') = rprec rule n n'
rwt rule (LeadsTo n n') = rleadsTo rule n n'
rwt rule (Non t) = rlnon rule t
rwt rule (Pnon t) = rlpnon rule t
rwt rule (Uniq t) = rluniq rule t
rwt rule (UniqAt t n) = runiqAt rule t n
rwt rule (UgenAt t n) = rugenAt rule t n
rwt rule (Ugen t) = rlugen rule t
rwt rule (AFact name fs) = rafact rule name fs
rwt rule (Equals t t') = req rule t t'

rlength :: Algebra t p g s e c => Role t -> t -> Int -> Rewrite t g s e
rlength r z h k (g, e)
  | length (rtrace r) < h = []
  | otherwise =
    case strdLookup e z of
      Just s ->                 -- Try to displace
        rDisplace e k' ns s
        where
          k' = addStrand g k r h
          ns = nstrands k
      Nothing ->                -- Try to augment
        do                      -- and displace everywhere
          let ns = nstrands k
          (g, e) <- strdMatch z ns (g, e)
          let k' = addStrand g k r h
          let f s' = rDisplace e k' ns s'
          (k', (gen k', e)) : concatMap f (nats ns)

-- Just add a strand cloned from a role.
-- The length must be greater than one.
addStrand :: Algebra t p g s e c => g -> Preskel t g s e ->
             Role t -> Int -> Preskel t g s e
addStrand g k r h =
  newPreskel g' (shared k) insts'
  (orderings k) (leadsto k) decls' (kfacts k) (operation k)
  (prob k) (kpriority k) (pov k)
  where
    (g', inst) = mkInstance g r emptyEnv h -- Create instance
    insts' = (insts k) ++ [inst]
    decls' = inheritRdecls s inst (decls k)
    s = length (insts k)

rDisplace :: Algebra t p g s e c => e -> Preskel t g s e ->
             Sid -> Sid -> [(Preskel t g s e, (g, e))]
rDisplace e k s s' | s == s' = [(k, (gen k, e))]
rDisplace e k s s' =
  do
    (s, s', subst) <- unifyStrands k s s'
    k <- rSubst k subst
    k <- rCompress k s s'
    return (k, (gen k, strdUpdate
                       (substUpdate e (snd subst))
                       (updateStrand s s')))

rSubst :: Algebra t p g s e c => Preskel t g s e ->
          (g, s) -> [Preskel t g s e]
rSubst k (gen, subst) =
    do
      (gen', insts') <- foldMapM (substInst subst) gen (insts k)
      let decls' = declsMapTerms (substitute subst) (decls k)
      let facts' = map (substFact subst) (kfacts k)
      let operation' = substOper subst (operation k)
      return $
        newPreskel gen' (shared k) insts'
        (orderings k) (leadsto k) decls' facts' operation'
        (prob k) (kpriority k) (pov k)

rCompress :: Algebra t p g s e c => Preskel t g s e ->
             Sid -> Sid -> [Preskel t g s e]
rCompress k s s' =
    do
      let perm = updatePerm s s' (strandids k)
      orderings' <- normalizeOrderings True
                    (permuteOrderings perm (orderings k))
      return $
        newPreskel
        (gen k)
        (shared k)
        (deleteNth s (insts k))
        orderings'
        (permuteOrderings perm (leadsto k))
        (declsMapStrands perm (decls k))
        (map (updateFact $ updateStrand s s') (kfacts k))
        (operation k)
        (updateProb perm (prob k))
        (updatePriority perm (kpriority k))
        (pov k)

rparam :: Algebra t p g s e c => String -> Role t ->
          t -> Int -> t -> t -> Rewrite t g s e
rparam name r v h z t k (g, e) =
  case strdLookup e z of
    Just s
      | height inst < h -> []
      | rname (role inst) == rname r ->
        rParam name k (g, e) t t'
      | otherwise ->
        do
          (k, (g, e)) <- rDisplace e k' ns s
          rParam name k (g, e) t t'
      where
        inst = strandInst k s
        t' = instantiate (env inst) v
        k' = addStrand g k r h
        ns = nstrands k
    Nothing ->
      error ("In rule " ++ name ++
             ", parameter predicate did not get a strand")

rParam :: Algebra t p g s e c => String -> Preskel t g s e ->
          (g, e) -> t -> t -> [(Preskel t g s e, (g, e))]
rParam name k (g, e) t t' =
  case matched e t of
    True ->
      rUnify k (g, e) (instantiate e t) t'
    False ->
      error ("In rule " ++ name ++
             ", parameter predicate did not get a value")

rUnify :: Algebra t p g s e c => Preskel t g s e ->
          (g, e) -> t -> t -> [(Preskel t g s e, (g , e))]
rUnify k (g, e) t t' =
  do
    subst <- unify t t' (g, emptySubst)
    k <- rSubst k subst
    return (k, (gen k, substUpdate e (snd subst)))

rprec :: Algebra t p g s e c => String -> NodeTerm t ->
         NodeTerm t -> Rewrite t g s e
rprec name (z, i) (z', i') k (g, e) =
  case (strdLookup e z, strdLookup e z') of
    (Just s, Just s')
      | elem ((s, i), (s', i')) tc -> [(k, (g, e))]
      | badIndex k s i || badIndex k s' i' -> []
      | otherwise ->
        do                      -- Add one ordering
          orderings' <- normalizeOrderings True
                        (((s, i), (s', i')) : orderings k)
          let k' = newPreskel g (shared k) (insts k) orderings'
                   (leadsto k) (decls k) (kfacts k) (operation k)
                   (prob k) (kpriority k) (pov k)
          return (k', (gen k, e))
    _ ->
      error ("In rule " ++ name ++ ", precedence did not get a strand")
  where
    tc = map graphPair $ graphClose $ graphEdges $ strands k

badIndex :: Algebra t p g s e c => Preskel t g s e -> Sid -> Int -> Bool
badIndex k s i =
  i >= height (strandInst k s)

rleadsTo :: Algebra t p g s e c => String -> NodeTerm t ->
            NodeTerm t -> Rewrite t g s e
rleadsTo name (z, i) (z', i') k (g, e) =
  case (strdLookup e z, strdLookup e z') of
    (Just s, Just s')
      | elem ((s, i), (s', i')) (leadsto k) -> [(k, (g, e))]
      | badIndex k s i || badIndex k s' i' -> []
      | otherwise ->
        do                      -- Add one ordering
          let leadsto' = ((s, i), (s', i')) : leadsto k
          let k' = newPreskel g (shared k) (insts k) (orderings k)
                   leadsto' (decls k) (kfacts k) (operation k)
                   (prob k) (kpriority k) (pov k)
          return (k', (gen k, e))
    _ ->
      error ("In rule " ++ name ++ ", leads to did not get a strand")

withNewDecl :: Algebra t p g s e c => Preskel t g s e ->
               SkelDeclarations t -> Preskel t g s e
withNewDecl k d =
  newPreskel (gen k) (shared k) (insts k) (orderings k)
  (leadsto k) d (kfacts k) (operation k)
  (prob k) (kpriority k) (pov k)

rlnon :: Algebra t p g s e c => String -> t -> Rewrite t g s e
rlnon name t k (g, e) =
  case matched e t of
    True
      | elem t' (knon k) -> [(k, (g, e))]
      | not $ isAtom t' -> []
      | otherwise ->
        [(k', (g, e))]
        where
          k' = withNewDecl k (addDeclInst "non-orig" [t'] [] (decls k))
    False ->
      error ("In rule " ++ name ++ ", non did not get a term")
  where
    t' = instantiate e t

rlpnon :: Algebra t p g s e c => String -> t -> Rewrite t g s e
rlpnon name t k (g, e) =
  case matched e t of
    True
      | elem t' (kpnon k) -> [(k, (g, e))]
      | not $ isAtom t' -> []
      | otherwise ->
        [(k', (g, e))]
        where
          k' = withNewDecl k (addDeclInst "pen-non-orig" [t'] [] (decls k))
    False ->
      error ("In rule " ++ name ++ ", pnon did not get a term")
  where
    t' = instantiate e t

rluniq :: Algebra t p g s e c => String -> t -> Rewrite t g s e
rluniq name t k (g, e) =
  case matched e t of
    True
      | elem t' (dkunique $ decls k) -> [(k, (g, e))]
      | not $ isAtom t' -> []
      | otherwise ->
        case originationNodes (strands k) t' of
          (_, []) -> []
          (_, n : _) ->
            [(k', (g, e))]
            where
              k' = withNewDecl k (addDeclInst "uniq-orig" [t'] [n] (decls k))
    False ->
      error ("In rule " ++ name ++ ", uniq did not get a term")
  where
    t' = instantiate e t

runiqAt :: Algebra t p g s e c => String -> t ->
           NodeTerm t -> Rewrite t g s e
runiqAt name t (z, i) k (g, e) =
  case (matched e t, strdLookup e z) of
    (True, Just s)
      | elem (t', (s, i)) (dkuniqFull $ decls k) -> [(k, (g, e))]
      | not $ isAtom t' -> []
      | i >= height (strandInst k s) -> []
      | otherwise ->
        [(k', (g, e))]
        where
          k' = withNewDecl k (addDeclInst "uniq-orig" [t'] [(s, i)] (decls k))
    (False, _) ->
      error ("In rule " ++ name ++ ", uniq-at did not get a term")
    (_, Nothing) ->
      error ("In rule " ++ name ++ ", uniq-at did not get a strand")
  where
    t' = instantiate e t

rugenAt :: Algebra t p g s e c => String -> t ->
           NodeTerm t -> Rewrite t g s e
rugenAt name t (z, i) k (g, e) =
  case (matched e t, strdLookup e z) of
    (True, Just s)
      | elem (t', (s, i)) (dkuniqFull $ decls k) -> [(k, (g, e))]
      | not $ isAtom t' -> []
      | otherwise ->
        [(k', (g, e))]
        where
          k' = withNewDecl k (addDeclInst "uniq-gen" [t'] [(s, i)] (decls k))
    (False, _) ->
      error ("In rule " ++ name ++ ", ugen-at did not get a term")
    (_, Nothing) ->
      error ("In rule " ++ name ++ ", ugen-at did not get a strand")
  where
    t' = instantiate e t

rlugen :: Algebra t p g s e c => String -> t -> Rewrite t g s e
rlugen name t k (g, e) =
  case matched e t of
    True
      | elem t' (dkuniqgen $ decls k) -> [(k, (g, e))]
      | not $ isAtom t' -> []
      | otherwise ->
        case originationNodes (strands k) t' of
          (_, []) -> []
          (_, n : _) ->
            [(k', (g, e))]
            where
              k' = withNewDecl k (addDeclInst "uniq-gen" [t'] [n] (decls k))
    False ->
      error ("In rule " ++ name ++ ", ugen did not get a term")
  where
    t' = instantiate e t

rafact :: Algebra t p g s e c => String -> String -> [t] -> Rewrite t g s e
rafact rule name fts k (g, e)
  | elem fact (kfacts k) = [(k, (g, e))]
  | otherwise = [(k', (gen k', e))]
  where
    fts' = map (rFactLookup rule e) fts
    fact = Fact name fts'
    k' = newPreskel
         g (shared k) (insts k) (orderings k) (leadsto k)
         (decls k) (fact : kfacts k)
         (operation k) (prob k) (kpriority k) (pov k)

rFactLookup :: Algebra t p g s e c => String -> e -> t -> FTerm t
rFactLookup name e t
  | isStrdVar t =
    case strdLookup e t of
      Just s -> FSid s
      Nothing ->
        error ("In rule " ++ name ++ ": fact did not get a strand")
  | matched e t = FTerm $ instantiate e t
  | otherwise =
      error ("In rule " ++ name ++ ": fact did not get a term")

req :: Algebra t p g s e c => String -> t -> t -> Rewrite t g s e
req name x y k (g, e)
  | isStrdVar x =
    case (strdLookup e x, strdLookup e y) of
      (Just s, Just t)
        | s == t -> [(k, (g, e))]
        | otherwise ->
          rDisplace e (k {gen = g})  s t
      _ ->
        error ("In rule " ++ name ++ ", = did not get a strand")
  | otherwise =
    case (matched e x, matched e y) of
      (True, True)
        | u == v -> [(k, (g, e))]
        | otherwise ->
          rUnify k (g, e) u v
      _ ->
        error ("In rule " ++ name ++ ", = did not get a term")
    where
      u = instantiate e x
      v = instantiate e y
