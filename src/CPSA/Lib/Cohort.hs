-- Computes the cohort associated with a skeleton or its generalization

-- Copyright (c) 2009 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module CPSA.Lib.Cohort (Mode(..), reduce, unrealized, minPriority) where

import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.List as L
import qualified Data.Tuple as T
import CPSA.Lib.Algebra
import CPSA.Lib.State
import CPSA.Lib.Protocol
import CPSA.Lib.Strand
import CPSA.Lib.Utilities

-- Debugging support
--import CPSA.Lib.Debug
--import CPSA.Lib.DebugLibrary

--}

-- Compile time switches for expermentation.

-- Include the escape set in the set of target terms
useEscapeSetInTargetTerms :: Bool
useEscapeSetInTargetTerms = False -- True

-- Filter a cohort for skeletons that solve the test.  Turn off only
-- to debug the other parts of the test solving algorithm.
useSolvedFilter :: Bool
useSolvedFilter = True -- False

-- Use pruning during generalization.
usePruningDuringGeneralization :: Bool
usePruningDuringGeneralization = False -- True

-- Minimum priority to solve
minPriority :: Int
minPriority = 1

-- Returns the nodes in a preskeleton that are not realized.
unrealized :: Algebra t p g s e c => Preskel t g s e -> [Node]
unrealized k =
    foldl unrealizedInStrand [] (strands k)
    where
      unrealizedInStrand acc s =
          fst $ foldl (unrealizedInNode k) (acc, S.empty) (nodes s)

-- Lifted here because of ambiguity complains in GHC 8.4.2
unrealizedInNode :: Algebra t p g s e c => Preskel t g s e ->
                    ([Node], Set (Vertex t e)) -> Vertex t e ->
                    ([Node], Set (Vertex t e))
unrealizedInNode k (acc, ns) n =
  case event n of
    In t ->
      let ns' = addSendingBefore ns n
          ts = transmissionsBefore ns n in
        case derivable (avoid k) ts t of
          True -> (acc, ns')
          False -> (graphNode n : acc, ns')
    Sync (Tran (Just _, _, _))
      | not (explainable k n (leadsto k)) ->
          (graphNode n : acc, ns)
    _ -> (acc, ns)

transmissionsBefore :: Algebra t p g s e c => Set (Vertex t e) ->
                       Vertex t e -> Set t
transmissionsBefore vs v =
    termsInNodes (addSendingBefore vs v)

addSendingBefore :: Algebra t p g s e c => Set (Vertex t e) ->
                    Vertex t e  -> Set (Vertex t e)
addSendingBefore s n =
    foldl addSending s (preds n)
    where
      addSending s n
        | S.member n s = s
        | otherwise = addSendingBefore (addIfSending s n) n
      addIfSending s n =
          case event n of
            Out _ -> S.insert n s
            _ -> s

termsInNodes :: Algebra t p g s e c => Set (Vertex t e) -> Set t
termsInNodes ns =
  S.foldl' f S.empty ns
  where
    f ts n =
      L.foldl' (flip S.insert) ts (evtMesgTerms (event n))

-- Is node n a tranition or observation node that is explainable,
-- either because its current state is a variable of sort message or
-- because an immediate transition produces the current state.
explainable :: Algebra t p g s e c => Preskel t g s e -> Vertex t e -> [Pair] -> Bool
explainable k n leadsto =
  case (event n) of
    Sync t -> case (now t) of
      Nothing ->  True
      Just st -> case lookup (s,i) (map T.swap leadsto) of
        Nothing -> isAcquiredVar st || isObtainedVar st
        Just (s2,i2) -> case (trace (insts k !! s2) !! i2) of
          Sync t2 -> Just st == (next t2)
          _ -> False
    _ -> True
  where
    (s,i) = graphNode n

-- Suppose k --v,p-> k', where k |-phi,sigma-> k'.  Let t=msg(k, v)@p,
-- t'=sigma(t), T=sigma(esc(k, v, t)), and t"=msg(k', phi(v)).
-- Position p is solved in k' from k at v if:
--
-- 1. some member of anc(t", p) is in T', or
--
-- 2. for some t in outpred(k', phi(v)), t' is not carried only within
--    T in t, OR
-- ("2a") targetterms(t', T) \ sigma(targetterms(t, esc(k, v, t))) /= empty
--    and there are variables in k's protocol that are not atoms, or
--
-- 3. the decryption key for an element of T is derivable, or
--
-- 4. t' is an encryption and the encryption key for t' is derivable, or
--
-- 5. t' is derivable in k' at phi(v).
--
-- 6. k' has a distinct absent constraint
--
-- Haskell variables:
-- ct     = t
-- pos    = p
-- ek     = encription key if ct is an encyption else nothing
-- escape = esc(k, v, t)
-- k      = k'
-- n      = v
-- subst  = sigma
solved :: Algebra t p g s e c => t -> p -> [t] -> Set t ->
          Preskel t g s e -> Node -> s -> [(t,t)] -> Bool
solved ct pos eks escape k n subst absent =
    -- Condition 1
    isAncestorInSet escape' t pos || derivable a escape' ct' ||
    -- Condition 2
    any (\t -> (not $ carriedOnlyWithin ct' escape' t)) (S.toList ts) ||
    not (varsAllAtoms (protocol k)) && not (S.null targetTermsDiff) ||
    -- Condition 3
    any (maybe False (derivable a ts) . decryptionKey) (S.toList escape') ||
    -- Condition 4
    -- Bug fix: apply subst to eks
    any (derivable a ts) (map (substitute subst) eks) ||
    -- Condition 5
    derivable a ts ct' ||
    -- Condition 6: hack!
    length (kabsent k) > length absent
    where
      v = vertex k n                 -- Look up vertex in k
      t = evt id erro errs (event v)  -- Term at v
      erro = const $ assertError "Cohort.solved: got an outbound term"
      errs = const $ assertError "Cohort.solved: got a state synchronization term"
      ct' = substitute subst ct -- Mapped critical term
      escape' = S.map (substitute subst) escape
      mappedTargetTerms = S.map (substitute subst) (targetTerms ct escape)
      targetTermsDiff = S.difference (targetTerms ct' escape') mappedTargetTerms
      ts = transmissionsBefore S.empty v -- Outbound predecessors
      a = avoid k

maybeSolved :: Algebra t p g s e c => t -> p -> [t] -> Set t ->
               Preskel t g s e -> Node -> s -> [(t,t)] -> Bool
maybeSolved ct pos eks escape k n subst absent =
    not useSolvedFilter || solved ct pos eks escape k n subst absent

data Mode = Mode
    { noGeneralization :: Bool,
      nonceFirstOrder :: Bool,
      visitOldStrandsFirst :: Bool,
      reverseNodeOrder :: Bool }
    deriving Show

-- Abort if there is an unrealized node without a test, otherwise
-- return a list of skeletons that solve one test.  If the skeleton is
-- realized, try to generalize it, but only when noIsoChk is false.
reduce :: Algebra t p g s e c => Mode -> Preskel t g s e ->
          [Preskel t g s e]
reduce mode k =
    filterSame k $ concatMap simplify ks -- Apply rewrites
    where
      ks = firstJust (map (testNode mode k a) ns)
           (whenRealized k) -- Skeleton is realized
      triples = L.sortBy priorityOrder
                [(s,i,p) | s <- [0..((length $ strands k)-1)],
                           i <- [0..((height $ instidx s)-1)],
                           let p = priority k (s,i)]
      short = filter (\(_, _, p)-> p >= minPriority) triples
      ns = map (\(s,i,_) -> (nodes (strandidx s) !! i)) short
      a = avoid k
      instidx s = (insts k) !! s
      strandidx s = (strands k) !! s
      whenRealized k =
          if noGeneralization mode then [] else maximize k
      priorityOrder (s0,i0,p0) (s1,i1,p1)
        | p0 /= p1 = compare p1 p0  -- reverse, so that higher number = higher priority
        | s0 /= s1 = compareStrandOrder mode s0 s1
        | otherwise = compareNodeOrder mode (strandidx s0) i0 i1

-- Filter out skeletons in ks that are isomorphic to k.
filterSame :: Algebra t p g s e c => Preskel t g s e ->
              [Preskel t g s e] -> [Preskel t g s e]
filterSame k ks =
  filter f ks
  where
    f k' = not $ isomorphic (gist k) (gist k')

compareStrandOrder :: Mode -> Int -> Int -> Ordering
compareStrandOrder mode s0 s1 =
  if visitOldStrandsFirst mode then (compare s0 s1)
  else (compare s1 s0)

compareNodeOrder :: Mode -> Strand t e -> Int -> Int -> Ordering
compareNodeOrder mode s i0 i1 =
  if (reverseNodeOrder mode == rsearch (role $ inst s)) then (compare i0 i1)
  else (compare i1 i0)

-- Returns the first Just value in a list or the default when there is
-- none.
firstJust :: [Maybe a] -> a -> a
firstJust [] x = x
firstJust (Just x : _) _ = x
firstJust (Nothing : xs) x = firstJust xs x

-- Look for a critical term that makes this node a test node.
testNode :: Algebra t p g s e c => Mode -> Preskel t g s e ->
                   Set t -> Vertex t e -> Maybe [Preskel t g s e]
testNode mode k a n =
   case event n of
     In t ->
       let ts = transmissionsBefore S.empty n -- Public messages
           (ts', a') = decompose ts a in
       if buildable ts' a' t then Nothing
       else Just $ solveNode mode k a' ts' (graphNode n) t
     Sync t | not (explainable k n (leadsto k)) ->
       Just $ solveSyncNode mode k (graphNode n) (now t)
     _ -> Nothing

carriedOnlyWithin :: Algebra t p g s e c => t -> Set t -> t -> Bool
carriedOnlyWithin target escape source =
    all (isAncestorInSet escape source) (carriedPlaces target source)

-- isAncestorInSet set source position is true if there is one ancestor of
-- source at position that is in the set.
isAncestorInSet :: Algebra t p g s e c => Set t -> t -> p -> Bool
isAncestorInSet set source position =
    any (flip S.member set) (ancestors source position)

-- Look for a critical term that makes this node a test node.
solveNode :: Algebra t p g s e c => Mode -> Preskel t g s e ->
            Set t -> Set t -> Node -> t ->
            [Preskel t g s e]
solveNode mode k a ts n t =
    loop cts
    where
      loop [] = assertError ("Cohort.solveNode missing test at " ++ show n)
      loop ((ct, eks) : rest) =
          case escapeSet ts a ct of
            Nothing -> loop rest
            Just escape ->
                places (carriedPlaces ct t)
                    where
                  places [] = loop rest -- Find position at which
                  places (p : ps)       -- ct has escaped
                      | isAncestorInSet escape t p = places ps
                      | otherwise = solvePath k a ct p eks n t escape
      cts =                     -- Potential critical messages
          if nonceFirstOrder mode then
            nonces ++ encs
          else
            encs ++ nonces
      -- include all carried terms that are in a or
      -- are numeric.
      nonces = map f (filter (flip carriedBy t) (S.toList a)) ++
               (foldCarriedTerms fnum [] t)
      encs = filter g (map h (encryptions t))
      fnum nums t | isNum t && (not $ buildable ts a t) = nums ++ [(t, [])]
                  | otherwise = nums
      f ct = (ct, [])           -- A nonce tests has no eks
      g (_, []) = False         -- An encryption test must have
      g _ = True                -- at least one non-derivable key
      -- Dump derivable encryption keys
      h (ct, eks) = (ct, filter (not . (buildable ts a)) eks)

-- Solve critical message at position pos at node n.
-- ct = t @ pos
-- t  = msg(k, n)

solvePath :: Algebra t p g s e c => Preskel t g s e -> Set t ->
             t -> p -> [t] -> Node -> t -> Set t -> [Preskel t g s e]
solvePath k a ct pos eks n t escape =
    mgs $ cons ++ augs ++ lsns ++ dhs
    where
      dhs = theDHSubcohort k a ct pos eks n escape cause
      cons = contractions k ct pos eks n t escape cause
      augs = augmentations k ct pos eks n escape cause
      lsns = addListeners k ct pos eks n t escape cause
      cause = Cause (dir eks) n ct escape

-- Filter out all but the skeletons with the most general homomorphisms.
mgs :: Algebra t p g s e c => [(Preskel t g s e, [Sid])] ->
       [Preskel t g s e]
mgs cohort =
  reverse $ map fst $ loop cohort []
  where
    loop [] acc = acc
    loop (kphi : cohort) acc
      | any (f kphi) cohort || any (f kphi) acc =
        loop cohort acc
      | otherwise = loop cohort (kphi : acc)
    f (k, phi) (k', phi') =
      any (not. null . homomorphism k' k)
          (composeFactors (strandids k) (strandids k') phi phi')

-- Given two permutations p and p', with ranges r and r', this
-- function returns the list of permutations p'' such that
--
--    p'' o p' = p.
--
-- This function assumes p' is injective and the returns permutations
-- that also must be.

composeFactors :: [Int] -> [Int] -> [Int] -> [Int] -> [[Int]]
composeFactors r r' p p' =
  perms (zip p' p) (filter (flip notElem p) r) r'

-- The correctness of this function depends on the fact that the
-- length of range is at most one so that the result is always
-- injective.

perms :: [(Int, Int)] -> [Int] -> [Int] -> [[Int]]
perms _ _ [] = [[]]
perms alist range (s:domain) =
  case lookup s alist of
    Just s' -> [ s':ss | ss <- perms alist range domain ]
    Nothing -> [ s':ss | s' <- range, ss <- perms alist range domain ]

-- DH Subcohort

theDHSubcohort :: Algebra t p g s e c => Preskel t g s e -> Set t ->
                  t -> p -> [t] -> Node -> Set t -> Cause t ->
                  [(Preskel t g s e, [Sid])]
theDHSubcohort k a ct pos eks n escape cause
  | isBase ct = baseDHSubcohort k ct pos eks n escape cause
  | isExpr ct = exprDHSubcohort k a ct pos eks n escape cause
  | otherwise = []

baseDHSubcohort :: Algebra t p g s e c => Preskel t g s e ->
                  t -> p -> [t] -> Node -> Set t -> Cause t ->
                  [(Preskel t g s e, [Sid])]
baseDHSubcohort k ct pos eks n escape cause
  | isNodePrecur k n = []
  | otherwise =
    [ (k', phi) |
      (k', n', phi, subst) <- addBaseListener k n cause ct,
      maybeSolved ct pos eks escape k' n' subst (kabsent k)]

exprDHSubcohort :: Algebra t p g s e c => Preskel t g s e -> Set t ->
                  t -> p -> [t] -> Node -> Set t -> Cause t ->
                  [(Preskel t g s e, [Sid])]
exprDHSubcohort k a ct pos eks n escape cause =
--  | isNodePrecur k n = []
--  | otherwise =
  do
    x <- S.toList a
    case expnInExpr x ct of
      False -> []
      True ->
        [ (k', phi) |
           (k', n', phi, subst) <- addAbsence k n cause x ct,
             maybeSolved ct pos eks escape k' n' subst (kabsent k) ] ++
        [ (k', phi) |
          (k', n', phi, subst) <- addListener k n cause x,
          maybeSolved ct pos eks escape k' n' subst (kabsent k) ]

-- Contractions

-- Contract the critical message at the given position.
contractions :: Algebra t p g s e c => Preskel t g s e ->
                t -> p -> [t] -> Node -> t -> Set t ->
                Cause t -> [(Preskel t g s e, [Sid])]
contractions k ct pos eks n t escape cause =
    [ (k, phi) | subst <- (solve escape anc (gen k, emptySubst) ++
                           constSolve (gen k, emptySubst) ct (kterms k)),
          (k, n, phi, subst') <- contract k n cause subst,
          maybeSolved ct pos eks escape k n subst' (kabsent k) ]
    where
      -- stick to only proper ancestors if ct is numeric;
      -- algebraSolve is strictly more general otherwise
      anc = if (isNum ct) then ancs else (ct : ancs)
      ancs = (ancestors t pos)

constSolve :: Algebra t p g s e c => (g, s) -> t -> [t] -> [(g, s)]
constSolve subst ct kts =
    [ s | c <- consts ct kts,
          s <- unify ct c subst]

solve :: Algebra t p g s e c => Set t -> [t] -> (g, s) -> [(g, s)]
solve escape ancestors subst =
    [ s | e <- S.toList escape,
          a <- ancestors,
          s <- unify a e subst ]

carriedOnlyWithinAtSubst :: Algebra t p g s e c =>
                            t -> Set t -> t -> (g, s) ->  Bool
carriedOnlyWithinAtSubst  ct escape t (_, subst) =
    carriedOnlyWithin ct' escape' t'
    where
      ct' = substitute subst ct
      escape' = S.map (substitute subst) escape
      t' = substitute subst t

fold :: Algebra t p g s e c => t -> Set t -> t -> Set t -> (g, s) -> [(g, s)]
fold ct escape t avoid (gen, subst) =
    [ (gen', compose subst' subst) |
      (gen', subst') <- foldl f [(gen, emptySubst)] (carriedRelPlaces ct' t' avoid') ]
    where
      ct' = substitute subst ct
      escape' = S.map (substitute subst) escape
      avoid' = S.map (substitute subst) avoid
      t' = substitute subst t
      f substs p =
          [ s | subst <- substs, s <- solve escape' (ancestors t' p) subst ]

dir :: [a] -> Direction
dir [] = Nonce
dir _ = Encryption

-- Augmentations

augmentations :: Algebra t p g s e c => Preskel t g s e ->
                t -> p -> [t] -> Node -> Set t ->
                Cause t -> [(Preskel t g s e, [Sid])]
augmentations k ct pos eks n escape cause
--  | ((not (isNodePrecur k n)) && ((isBase ct) || (isExpr ct))) = []
  | otherwise =
    [ k' | r <- roles (protocol k),
           k' <- roleAugs (gen k) k ct pos eks n escape cause targets r ]
    where
      targets = S.toList tterms
      tterms = targetTerms ct escape

roleAugs :: Algebra t p g s e c => g -> Preskel t g s e ->
            t -> p -> [t] -> Node -> Set t -> Cause t ->
            [t] -> Role t -> [(Preskel t g s e, [Sid])]
roleAugs g k ct pos eks n escape cause targets role =
    [ (k', phi) | (subst', inst) <- transformingNode ct escape targets role subst
                                    (avoid k),
           (k', n', phi, subst'') <- augment k n cause role subst' inst False,
           maybeSolved ct pos eks escape k' n' subst'' (kabsent k) ]
    where
      subst = cloneRoleVars g role

-- Generate a fresh set of role variables
cloneRoleVars :: Algebra t p g s e c => g -> Role t -> (g, s)
cloneRoleVars gen role =
    grow (rvars role) gen emptyEnv
    where
      grow [] gen env = (gen, substitution env)
      grow (t : ts) gen env =
          let (gen', t') = clone gen t in
          case match t t' (gen', env) of
            (gen'', env') : _ -> grow ts gen'' env'
            [] -> assertError "Cohort.cloneRoleVars: Internal error"

-- Identify all distinct potential transforming nodes
transformingNode :: Algebra t p g s e c => t -> Set t ->
                    [t] -> Role t -> (g, s) -> Set t ->
                    [((g, s), Instance t e)]
transformingNode ct escape targets role subst avoid =
  tNLoop ct escape targets role subst avoid 1 [] [] (rtrace role)
{-  Fails the ambiguity check
  loop 1 [] [] (rtrace role)
  where
      -- loop height past acc trace
      loop _ _ acc [] = acc
      -- receptions cannot be transforming
      loop ht past acc (In t : c) =
          loop (ht + 1) (In t : past) acc c
      loop ht past acc (Sync t : c) =
          loop (ht + 1) (Sync t : past) acc c
      loop ht past acc (Out t : c) =
          loop (ht + 1) (Out t : past) acc' c
          where
            substs = carriedBindings targets t subst
            substs' = cowt ct escape (stripSync past) substs avoid
--            substs' = cowt ct escape past substs avoid
            acc' = maybeAug ct escape role ht substs' acc t
-}

tNLoop :: Algebra t p g s e c => t -> Set t ->
                    [t] -> Role t -> (g, s) -> Set t -> Int -> Trace t ->
                    [((g, s), Instance t e)] -> Trace t ->
                    [((g, s), Instance t e)]
tNLoop _ _ _ _ _ _ _ _ acc [] = acc
-- receptions cannot be transforming
tNLoop ct escape targets role subst avoid ht past acc (In t : c) =
  tNLoop ct escape targets role subst avoid (ht + 1) (In t : past) acc c
tNLoop ct escape targets role subst avoid ht past acc (Sync t : c) =
  tNLoop ct escape targets role subst avoid (ht + 1) (Sync t : past) acc c
tNLoop ct escape targets role subst avoid ht past acc (Out t : c) =
  tNLoop ct escape targets role subst avoid (ht + 1) (Out t : past) acc' c
  where
    substs = carriedBindings targets t subst
    substs' = cowt ct escape (stripSync past) substs avoid
--            substs' = cowt ct escape past substs avoid
    acc' = maybeAug ct escape role ht substs' acc t

-- Terms considered for binding with the carried terms in an outbound
-- term.
targetTerms :: Algebra t p g s e c => t -> Set t -> Set t
targetTerms ct escape =
    if useEscapeSetInTargetTerms then
       targetTermsWithEscapeSet
    else
       S.difference targetTermsWithEscapeSet escape
    where
      -- hack: including both ct and ct' for cases where ct' leads to something
      -- that doesn't pass the solved filter.
      targetTermsWithEscapeSet = S.fold f (S.singleton ct) escape
      f t ts =
          foldl (flip S.insert) ts
                (concatMap (ancestors t) (carriedPlaces ct t))

-- Find bindings for terms in the test.
carriedBindings :: Algebra t p g s e c => [t] -> t -> (g, s) -> [(g, s)]
carriedBindings targets outbound subst =
  concatMap (\(subterm, target) -> unify subterm target subst) stPairs
  where
    stPairs = [(subterm,target)|
               subterm <- S.toList (foldCarriedTerms (flip S.insert) S.empty outbound),
               target <- targets]

-- Ensure the critical term is carried only within the escape set of
-- every term in the past using fold from cows.
cowt :: Algebra t p g s e c => t -> Set t ->
        Trace t -> [(g, s)] -> Set t -> [(g, s)]
-- cowt 0 _ _ _ _ _ = error ("cowt recursion limit reached")
cowt ct escape c substs avoid =
    nubSnd $ concatMap (cowt0 ct escape c avoid) substs

-- Remove pairs with the same second element.
nubSnd :: Eq b => [(a, b)] -> [(a, b)]
nubSnd substs =
    L.nubBy (\(_, s) (_, s') -> s == s') substs

-- Handle one substitution at a time.
cowt0 :: Algebra t p g s e c => t -> Set t ->
         Trace t -> Set t -> (g, s) -> [(g, s)]
cowt0 ct escape c avoid subst =
    if all (f subst) c then     -- Substitution works
        [subst]
    else                        -- Substitution needs refinement
        cowt ct escape c (foldn ct escape c [subst] avoid) avoid
    where
      f subst evt =
          carriedOnlyWithinAtSubst ct escape (evtTerm evt) subst

-- Apply fold to each message in the trace.
foldn :: Algebra t p g s e c => t -> Set t ->
         Trace t -> [(g, s)] -> Set t -> [(g, s)]
foldn _ _ [] substs _ = substs
foldn ct escape (evt : c) substs avoid =
    foldn ct escape c (concatMap (fold ct escape (evtTerm evt) avoid) substs) avoid

evtTerm :: Event t -> t
evtTerm (In t) = t
evtTerm (Out t) = t
evtTerm _ = assertError "Cohort: Extracting a term from a sync event"

-- If the outbound term is carried only within, no transforming node
-- was found, otherwise, add a candidate augmentation to the
-- accumulator.
maybeAug :: Algebra t p g s e c => t -> Set t ->
            Role t -> Int -> [(g, s)] ->
            [((g, s), Instance t e)] -> t ->
            [((g, s), Instance t e)]
maybeAug ct escape role ht substs acc t =
  answer
  where
      answer = foldl f acc $ L.filter testSolved substs
      testSolved (_, subst) =
        (not $ carriedOnlyWithin
                  (substitute subst ct)
                  (S.map (substitute subst) escape)
                  (substitute subst t))
  {- Fails the ambiguity check
      f acc (gen, subst) =
        case bldInstance role itrace gen of
            (gen, inst) : _ -> ((gen, subst), inst) : acc
            [] -> acc
          where
            itrace = map (evtMap $ substitute subst) (take ht (rtrace role))
-}
      f = maybeAugF role ht

maybeAugF :: Algebra t p g s e c => Role t -> Int ->
            [((g, s), Instance t e)] -> (g, s) ->
            [((g, s), Instance t e)]
maybeAugF role ht acc (gen, subst) =
  case bldInstance role itrace gen of
    (gen, inst) : _ -> ((gen, subst), inst) : acc
    [] -> acc
  where
    itrace = map (evtMap $ substitute subst) (take ht (rtrace role))

-- Listener augmentations

addListeners :: Algebra t p g s e c => Preskel t g s e ->
                t -> p -> [t] -> Node -> t -> Set t ->
                Cause t -> [(Preskel t g s e, [Sid])]
addListeners k ct pos eks n t escape cause =
    [ (k', phi) | t' <- filter (/= t) (S.toList (escapeKeys eks escape)),
           (k', n', phi, subst) <- addListener k n cause t',
           maybeSolved ct pos eks escape k' n' subst (kabsent k) ]

escapeKeys :: Algebra t p g s e c => [t] -> Set t -> Set t
escapeKeys eks escape =
    S.fold f es escape
    where
      f e s = maybe s (flip S.insert s) (decryptionKey e)
      es = S.fromList eks

-- solveSyncNode
solveSyncNode :: Algebra t p g s e c => Mode -> Preskel t g s e ->
                 Node -> Maybe t -> [Preskel t g s e]
solveSyncNode _ _ _ Nothing =
  assertError ("Cohort:solveSyncNode: unexpectedly asked to solve Sync node with no current state.")
solveSyncNode mode k n (Just t) =
  [ k' | r <- roles (protocol k),
         k' <- roleSPAugs cause mode k n t r ]
  where
    cause = Cause StatePassing n t (S.empty)

roleSPAugs :: Algebra t p g s e c => Cause t -> Mode ->
              Preskel t g s e -> Node -> t -> Role t -> [Preskel t g s e]
roleSPAugs cause _ k n t r =
  [ k' | (subst, inst) <- nextNode t (gen k) r,
         (k', _, _, _) <- augment k n cause r subst inst True ]

{- New failure of ambiguity check: this breaks!
-- Identify all distinct potential next nodes
nextNode :: Algebra t p g s e c => t -> g -> Role t ->
            [((g, s), Instance t e)]
nextNode now g role =
  loop 1 [] (rtrace role)
  where
    -- loop height past acc trace
    loop _ acc [] = acc
    loop ht acc (In _ : c) =
      loop (ht + 1) acc c
    loop ht acc (Out _ : c) =
      loop (ht + 1) acc c
    loop ht acc (Sync (Tran (_, Nothing, _)) : c) =
      loop (ht + 1) acc c
    loop ht acc (Sync (Tran (_, Just next, _)) : c) =
      loop (ht + 1) acc' c
      where
        substs = unify now next (cloneRoleVars g role)
        acc' = foldl f acc substs
        f = nextNodeF role ht
{- Fails the ambiguity check
        f acc (gen, subst) =
          case bldInstance role itrace gen of
            (gen, inst) : _ -> ((gen, subst), inst) : acc
            [] -> acc
          where
            itrace = map (evtMap $ substitute subst) (take ht (rtrace role))
-}
-}

-- Identify all distinct potential next nodes
nextNode :: Algebra t p g s e c => t -> g -> Role t ->
            [((g, s), Instance t e)]
nextNode now g role =
  loop 1 [] (rtrace role)
  where
    -- loop height past acc trace
    loop _ acc [] = acc
    loop ht acc (In _ : c) =
      loop (ht + 1) acc c
    loop ht acc (Out _ : c) =
      loop (ht + 1) acc c
    loop ht acc (Sync (Tran (_, Nothing, _)) : c) =
      loop (ht + 1) acc c
    loop ht acc (Sync (Tran (_, Just next, _)) : c) =
      loop (ht + 1) acc' c
      where
        acc' = nextNodeA now g role ht next acc

-- Lifted here because of ambiguity complains in GHC 8.4.2
nextNodeA :: Algebra t p g s e c => t -> g -> Role t -> Int -> t ->
            [((g, s), Instance t e)] -> [((g, s), Instance t e)]
nextNodeA now g role ht next acc =
  foldl f acc substs
  where
    f = nextNodeF role ht
    substs = unify now next (cloneRoleVars g role)

nextNodeF :: Algebra t p g s e c => Role t -> Int ->
             [((g, s), Instance t e)] -> (g, s) -> [((g, s), Instance t e)]
nextNodeF role ht acc (gen, subst) =
  case bldInstance role itrace gen of
    (gen, inst) : _ -> ((gen, subst), inst) : acc
    [] -> acc
  where
    itrace = map (evtMap $ substitute subst) (take ht (rtrace role))

-- Maximize a realized skeleton if possible.  Do not consider
-- generalizations that fail to satisfy the rules of the skeleton's
-- protocol.

maximize :: Algebra t p g s e c => Preskel t g s e ->
            [Preskel t g s e]
maximize k =
    take 1 (filter f gens)      -- Return at most the first answer
    where
      gens = do
        (k', mapping) <- generalize k -- Generalize generates candidates
        specialization k k' mapping   -- Test a candidate
      f k =
        case rewrite k of
          Nothing -> True
          _ -> False

-- Test to see if realized skeleton k is a specialization of
-- preskeleton k' using the given strand mapping.  Returns the
-- skeleton associated with k' if it refines k.

specialization :: Algebra t p g s e c => Preskel t g s e ->
                  Preskel t g s e -> [Sid] ->
                  [Preskel t g s e]
specialization k k' mapping
    | not (preskelWellFormed k') = []
    | otherwise =
        do
          k'' <- toSkeleton usePruningDuringGeneralization k'
          case realized k'' && not (isomorphic (gist k) (gist k'')) &&
               refines k'' (pov k'') (prob k'') &&
               refines k (Just k') mapping of
            True -> [k'']
            False -> []
        where
          realized = null . unrealized
          refines _ Nothing _ =
              assertError "Cohort.specialization: cannot find point of view"
          refines k (Just k') mapping =
              not $ null $ homomorphism k' k mapping
