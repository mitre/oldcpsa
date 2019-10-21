-- Adds annotations to preskeletons

-- Copyright (c) 2009 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

{-# LANGUAGE CPP #-}

#if !(MIN_VERSION_base(4,13,0))
#define MonadFail Monad
#endif

module CPSA.Annotations.Annotations (Prot, annotations) where

import Control.Monad
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Maybe as M
import CPSA.Lib.Utilities
import CPSA.Lib.SExpr
import CPSA.Lib.Algebra
import CPSA.Annotations.Formulas

{--
import System.IO.Unsafe
z :: Show a => a -> b -> b
z x y = unsafePerformIO (print x >> return y)
--}

-- Constructs the output or reports an error from the input given a
-- protocol name, and its algebra specific starting variable
-- generator.  The third argument is the protocol information required
-- for processing preskeletons.

annotations :: (Algebra t p g s e c, MonadFail m) => String -> g ->
         [Prot t g] -> SExpr Pos -> m ([Prot t g], SExpr Pos)
annotations name origin ps x@(L pos (S _ "defprotocol" : xs)) =
    do
      p <- loadProt name origin pos xs
      return (p : ps, x)
annotations _ _ ps (L pos (S _ "defskeleton" : xs)) =
    do
      y <- findPreskel pos ps xs
      return (ps, y)
annotations _ _ ps x = return (ps, x)

-- Load a protocol

-- The Prot record contains information extraced from protocols for
-- use when processing preskeletons.
data Prot t g = Prot
    { pname :: String,          -- Protocol name
      gen :: g,                 -- Generator for preskeletons
      roles :: [Role t] }
    deriving Show

-- The Role record contains information extraced from roles for use
-- when processing preskeletons.
data Role t = Role
    { rname :: String,          -- Role name
      vars :: [t],              -- Declared variables
      prin :: t,                -- Principal
      forms :: [Formula t] }    -- Annotations
    deriving Show

-- Load a protocol.  On success, returns a Prot record.

loadProt :: (Algebra t p g s e c, MonadFail m) => String -> g ->
            Pos -> [SExpr Pos] -> m (Prot t g)
loadProt nom origin pos (S _ name : S _ alg : x : xs)
    | alg /= nom =
        fail (shows pos $ "Expecting terms in algebra " ++ nom)
    | otherwise =
        do
          (gen, rs) <- loadRoles origin (x : xs)
          return (Prot { pname = name, gen = gen, roles = rs })
loadProt _ _ pos _ =
    fail (shows pos "Malformed protocol")

-- A generator is threaded thoughout the protocol loading process so
-- as to ensure no variable occurs in two roles.  It also ensures that
-- every variable that occurs in a preskeleton never occurs in one of
-- its roles.

loadRoles :: (Algebra t p g s e c, MonadFail m) => g ->
             [SExpr Pos] -> m (g, [Role t])
loadRoles origin xs =
    mapAccumLM loadRole origin xs

loadRole :: (Algebra t p g s e c, MonadFail m) => g ->
            SExpr Pos -> m (g, Role t)
loadRole gen (L pos (S _ "defrole" :
                     S _ name :
                     L _ (S _ "vars" : vars) :
                     L _ (S _ "trace" : _ : c) :
                     rest)) =
    do
      (gen, vars) <- loadDecls gen vars
      let len = 1 + length c    -- Length of the trace
      let annos = assoc annotationsKey rest
      (gen, prin, forms) <- loadFormulas pos vars len gen annos
      let r = Role { rname = name, vars = vars, prin = prin, forms = forms }
      return (gen, r)
loadRole _ x =
    fail (shows (annotation x) "Malformed role")

loadFormulas :: (Algebra t p g s e c, MonadFail m) => Pos -> [t] -> Int ->
                g -> [SExpr Pos] -> m (g, t, [Formula t])
loadFormulas pos vars len gen (x : xs) =
    do
      prin <- loadTerm vars False x
      case isAtom prin of
        True -> return ()
        False -> fail (shows pos "principal not an atom")
      (g, alist) <- mapAccumLM (loadIndexedFormula vars len) gen xs
      checkIndices pos alist
      return (g, prin, map (getNth alist) (nats len))
    where
      getNth alist i = maybe true id (lookup i alist)
loadFormulas pos _ _ _ [] =
    fail (shows pos "Role missing annotations")

loadIndexedFormula :: (Algebra t p g s e c, MonadFail m) => [t] -> Int ->
                      g -> SExpr Pos -> m (g, (Int, Formula t))
loadIndexedFormula _ len _ (L _ [N pos i, _])
    | i < 0 || i >= len = fail (shows pos "Bad index for formula")
loadIndexedFormula vars _ gen (L _ [N _ i, form]) =
    do
      (g, f) <-  loadFormula vars gen form
      return (g, (i, f))
loadIndexedFormula _ _ _ x =
    fail (shows (annotation x) "Malformed indexed formula")

-- Ensure there are no formulas with the same index
checkIndices :: (Algebra t p g s e c, MonadFail m) => Pos ->
                [(Int, Formula t)] -> m ()
checkIndices _ [] = return ()
checkIndices pos ((i, _) : alist)
    | any ((== i) . fst) alist =
        fail (shows pos $ showString "Duplicate index " $ show i)
    | otherwise = checkIndices pos alist

-- Load a preskeleton

data Instance t e = Instance
    -- Role from which this was instantiated (Nothing for listeners)
    { pos :: Pos,               -- Instance position
      role :: Maybe (Role t),
      env :: e,                 -- The environment
      height :: Int }           -- Height of the instance
    deriving Show

type Strands = [Int]            -- [Strand height]

data Dir = InDir
         | OutDir
         | SyncDir
         deriving Eq

type Trace = [Dir]              -- Directions of terms in trace

type Node = (Int, Int)          -- (Strand, Position)

type Pair = (Node, Node)        -- Precedes relation

data Preskel t g e = Preskel
    { protocol :: Prot t g,
      insts :: [Instance t e],
      traces :: [Trace],
      orderings :: [Pair] }

-- Find protocol and then load preskeleton.
-- Remove any old annotations, and add new ones.
findPreskel :: (Algebra t p g s e c, MonadFail m) => Pos ->
               [Prot t g] -> [SExpr Pos] -> m (SExpr Pos)
findPreskel pos ps (S _ name : xs) =
    case L.find (\p -> name == pname p) ps of
      Nothing -> fail (shows pos $ "Protocol " ++ name ++ " unknown")
      Just p ->
          do
            xs <- updatePreskel pos p xs
            return (L pos (S pos "defskeleton" : S pos name : xs))
findPreskel pos _ _ = fail (shows pos "Malformed skeleton")

updatePreskel :: (Algebra t p g s e c, MonadFail m) => Pos ->
                 Prot t g -> [SExpr Pos] -> m [SExpr Pos]
updatePreskel pos prot (vs@(L _ (S _ "vars" : vars)) : xs)
    | not (realized xs) || not (hasKey tracesKey xs) =
        return (vs : xs)        -- Don't annotate
    | otherwise =
        do
          (gen', kvars) <- loadDecls (gen prot) vars
          k <- loadPreskel prot gen' kvars xs
          let xs' = strip annotationsKey (strip obligationsKey xs)
          annos <- mapM instAnnos (insts k)
          obls <- obligations pos k annos
          let xs'' = xs' ++ [addPos pos $ displayAnnos kvars annos,
                             addPos pos $ displayObls kvars obls]
          return (vs : xs'')
updatePreskel pos _ _ = fail (shows pos "Malformed skeleton")

realized :: [SExpr a] -> Bool
realized xs =
    null (assoc "unrealized" xs) && hasKey "unrealized" xs

loadPreskel :: (Algebra t p g s e c, MonadFail m) => Prot t g ->
               g -> [t] -> [SExpr Pos] -> m (Preskel t g e)
loadPreskel prot gen kvars xs =
    do
      traces <- mapM loadTrace (assoc tracesKey xs)
      insts <- loadInsts prot gen kvars [] xs
      let heights = map height insts
      orderings <- loadOrderings heights (assoc precedesKey xs)
      return (Preskel { protocol = prot,
                        insts = insts,
                        traces = traces,
                        orderings = orderings })

loadInsts :: (Algebra t p g s e c, MonadFail m) => Prot t g ->
             g -> [t] -> [Instance t e] -> [SExpr Pos] ->
             m [Instance t e]
loadInsts prot gen kvars insts (L pos (S _ "defstrand" : x) : xs) =
    case x of
      S _ role : N _ height : env ->
          do
            (gen', i) <- loadInst pos prot gen kvars role height env
            loadInsts prot gen' kvars (i : insts) xs
      _ ->
          fail (shows pos "Malformed defstrand")
loadInsts prot kvars gen insts (L pos (S _ "deflistener" : x) : xs) =
    case x of
      [_] ->
          do
            let i = Instance { pos = pos, role = Nothing,
                               env = emptyEnv, height = 2 }
            loadInsts prot kvars gen (i : insts) xs
      _ ->
          fail (shows pos "Malformed deflistener")
loadInsts _ _ _ insts _ =
    return (reverse insts)

loadInst :: (Algebra t p g s e c, MonadFail m) => Pos -> Prot t g ->
            g -> [t] -> String -> Int -> [SExpr Pos] ->
            m (g, Instance t e)
loadInst pos prot gen kvars role height env =
    do
      r <- lookupRole pos prot role
      case height < 1 || height > length (forms r) of
        True -> fail (shows pos "Bad height")
        False -> return ()
      (gen', env) <- foldM (loadMaplet kvars (vars r)) (gen, emptyEnv) env
      return (gen', Instance { pos = pos, role = Just r,
                               env = env, height = height })

lookupRole :: MonadFail m => Pos -> Prot t g -> String -> m (Role t)
lookupRole pos prot role =
    case L.find (\r -> role == rname r) (roles prot) of
      Nothing ->
          fail (shows pos $ "Role " ++ role ++ " not found in " ++ pname prot)
      Just r -> return r

loadMaplet :: (Algebra t p g s e c, MonadFail m) =>
              [t] -> [t] -> (g, e) -> SExpr Pos -> m (g, e)
loadMaplet kvars vars env (L pos [domain, range]) =
    do
      t <- loadTerm vars False domain
      t' <- loadTerm kvars False range
      case match t t' env of
        env' : _ -> return env'
        [] -> fail (shows pos "Domain does not match range")
loadMaplet _ _ _ x = fail (shows (annotation x) "Malformed maplet")

-- Load a trace

loadTrace :: MonadFail m => SExpr Pos -> m Trace
loadTrace (L _ xs) = mapM loadDt xs
loadTrace x = fail (shows (annotation x) "Malformed trace")

loadDt :: MonadFail m => SExpr Pos -> m Dir
loadDt (L _ [S _ "recv", _]) = return InDir
loadDt (L _ [S _ "send", _]) = return OutDir
loadDt (L _ [S _ "sync", _]) = return SyncDir
loadDt  (L pos [S _ dir, _]) =
    fail (shows pos $ "Unrecognized direction " ++ dir)
loadDt x = fail (shows (annotation x) "Malformed direction")

-- Load the node orderings

loadOrderings :: MonadFail m => Strands -> [SExpr Pos] -> m [Pair]
loadOrderings _ [] = return []
loadOrderings strands (x : xs) =
    do
      np <- loadPair strands x
      nps <- loadOrderings strands xs
      return (adjoin np nps)

loadPair :: MonadFail m => [Int] -> SExpr Pos -> m Pair
loadPair heights (L pos [x0, x1]) =
    do
      n0 <- loadNode heights x0
      n1 <- loadNode heights x1
      case sameStrands n0 n1 of  -- Same strand
        True -> fail (shows pos "Malformed pair -- nodes in same strand")
        False -> return (n0, n1)
    where
      sameStrands (s0, _) (s1, _) = s0 == s1
loadPair _ x = fail (shows (annotation x) "Malformed pair")

loadNode :: MonadFail m => [Int] -> SExpr Pos -> m Node
loadNode heights (L pos [N _ s, N _ p])
    | s < 0 = fail (shows pos "Negative strand in node")
    | p < 0 = fail (shows pos "Negative position in node")
    | otherwise =
        case height heights s of
          Nothing -> fail (shows pos "Bad strand in node")
          Just h | p < h -> return (s, p)
          _ -> fail (shows pos "Bad position in node")
    where
      height [] _ = Nothing
      height (x: xs) s          -- Assume s non-negative
          | s == 0 = Just x
          | otherwise = height xs (s - 1)
loadNode _ x = fail (shows (annotation x) "Malformed node")

-- Formula instantiation

type Annotations t = Maybe (t, [Formula t])

-- Construct the annotations for an instance
instAnnos :: (Algebra t p g s e c, MonadFail m) =>
             Instance t e -> m (Annotations t)
-- A listener has no annotations
instAnnos (Instance { role = Nothing }) = return Nothing
instAnnos i@(Instance { pos = pos, role = Just r }) =
    do
      t <- transTerm pos (vars r) (env i) (prin r)
      fs <- mapM instAnno (take (height i) (forms r))
      return (Just (t, fs))
    where
      instAnno f =
          transForm pos (vars r) (env i) f

showsTerm :: Algebra t p g s e c => [t] -> t -> ShowS
showsTerm vars t = shows (displayTerm (varsContext vars) t)

varsContext :: Algebra t p g s e c => [t] -> c
varsContext vars =
    addToContext emptyContext vars

-- Instantiate term and ensure all the free variables have been mapped.
transTerm :: (Algebra t p g s e c, MonadFail m) => Pos -> [t] -> e -> t -> m t
transTerm pos vars env t =
    let t' = instantiate env t in
    case L.find (flip S.member (fv t')) vars of
      Nothing -> return t'
      Just var ->
          fail $ shows pos $ showString "The variable " $
               showsTerm vars var " is not mapped in a term"
    where
      fv t = foldVars (flip S.insert) S.empty t

-- Instantiate formula and ensure all the free variables have been mapped.
transForm :: (Algebra t p g s e c, MonadFail m) => Pos -> [t] -> e ->
             Formula t -> m (Formula t)
transForm pos vars env f =
    let f' = finstantiate env f in
    case L.find (flip S.member (freeVars f')) vars of
      Nothing -> return f'
      Just var ->
          fail $ shows pos $ showString "The variable " $
               showsTerm vars var " is not mapped in a formula"

displayAnnos :: Algebra t p g s e c => [t] ->
                [Annotations t] -> SExpr ()
displayAnnos vars annos =
    L () (S () annotationsKey : table)
    where
      table = [ displayObl vars ((s, p), t, f) |
                (s, Just (t, fs)) <- zip [0..] annos,
                (p, f) <- zip [0..] fs,
                not (truth f) ]

-- Graphs as adjacency lists

adj :: Strands -> [Pair] -> Node -> [Node]
adj strands precedes (s, p) =
    [ strand s h | (s, h) <- zip [0..] strands ] !! s !! p
    where
      strand :: Int -> Int -> [[Node]]
      strand s h = [ entry (s, p) | p <- nats h ]
      entry :: Node -> [Node]
      entry n = enrich n [ n0 | (n0, n1) <- precedes, n1 == n ]
      -- add strand succession edges
      enrich (s, p) ns
          | p > 0 = (s, p - 1) : ns
          | otherwise = ns

-- Transitive closure

close :: Strands -> [Pair] -> [Pair]
close strands precedes =
    loop prec False prec
    where
      prec = successors strands ++ precedes
      loop prec False [] = prec
      loop prec True [] =
          loop prec False prec -- restart loop
      loop prec repeat ((n0, n1) : pairs) =
          inner prec repeat pairs [(n, n1) | n <- adj strands precedes n0]
      inner prec repeat pairs [] =
          loop prec repeat pairs
      inner prec repeat pairs (p : rest)
          | elem p prec = inner prec repeat pairs rest
          | otherwise = inner (p : prec) True pairs rest

-- Filter pairs that don't effect annotations.

orient :: [Trace] -> [Pair] -> [Pair]
orient traces precedes =
    L.nub $ L.filter pred precedes
    where
      pred (n0, n1) = outb traces n0 && inb traces n1

outb :: [Trace] -> Node -> Bool
outb traces (s, p) = OutDir == traces !! s !! p

inb :: [Trace] -> Node -> Bool
inb traces (s, p) = InDir == traces !! s !! p

successors :: Strands -> [Pair]
successors strands =
    [((s, p), (s, p + 1)) | (s, n) <- zip [0..] strands, p <- nats (n - 1)]

before :: MonadFail m => Pos -> Preskel t g e -> m [(Node, [Node])]
before pos k =
    case isAcyclic graph nodes of
      False -> fail (shows pos "Cycle found")
      True -> return $ alist (traces k) $ orient (traces k) closure
    where
      nodes = [(s, p) | s <- nats $ length strands, p <- nats $ strands !! s]
      closure = close strands (orderings k)
      strands = map height (insts k)
      graph = adj strands (orderings k)

-- Computes all transmission nodes before a reception node

alist :: [Trace] -> [Pair] -> [(Node, [Node])]
alist traces precedes =
    [ node (s, p) | (s, t) <- zip [0..] traces, (p, d) <- zip [0..] t,
                    InDir == d ]
    where
      node n = (n, L.sort (depends n))
      depends n = [ n0 | (n0, n1) <- precedes, n == n1 ]

type Obligation t = (Node, t, Formula t)

obligations :: (Algebra t p g s e c, MonadFail m) => Pos ->
               Preskel t g e -> [Annotations t] -> m [Obligation t]
obligations pos k annos =
    do
      depends <- before pos k
      let obls = map (obligation annos) depends
      -- filter out trivial obligations
      return [ obl | Just obl@(_, _, f) <- obls, not (truth f) ]

obligation :: Algebra t p g s e c => [Annotations t] ->
              (Node, [Node]) -> Maybe (Obligation t)
obligation annos (n@(s, p), ns) =
    do
      (t, fs) <- annos !! s                      -- t relies on (fs !! p)
      let guar = [ if t == t' then f else says t' f | -- Believe t
                   (s, p) <- ns,                 -- For each predecessor
                   (t', fs) <- M.maybeToList (annos !! s), -- t' is the speaker
                   let f = fs !! p ]               -- f is the guarantee
      return (n, t, implies guar (fs !! p))

displayObls :: Algebra t p g s e c => [t] -> [Obligation t] -> SExpr ()
displayObls vars obls =
    L () (S () obligationsKey : map (displayObl vars) obls)

displayObl :: Algebra t p g s e c => [t] -> Obligation t -> SExpr ()
displayObl vars (n, t, f) =
    L () [displayNode n, displayTerm ctx t, displayFormula vars f]
    where
      ctx = varsContext vars

displayNode :: Node -> SExpr ()
displayNode (s, p) = L () [N () s, N () p]

addPos :: Pos -> SExpr a -> SExpr Pos
addPos pos (S _ s) = S pos s
addPos pos (Q _ s) = Q pos s
addPos pos (N _ n) = N pos n
addPos pos (L _ l) = L pos (map (addPos pos) l)

-- Association lists

-- Lookup value in alist, appending values with the same key
assoc :: String -> [SExpr a] -> [SExpr a]
assoc key alist =
    concat [ rest | L _ (S _ head : rest) <- alist, key == head ]

keyPred :: String -> SExpr a -> Bool
keyPred key (L _ (S _ head : _)) = key == head
keyPred _ _ = False

hasKey :: String -> [SExpr a] -> Bool
hasKey key alist = any (keyPred key) alist

strip :: String -> [SExpr a] -> [SExpr a]
strip key alist = filter (not . keyPred key) alist

-- the key used in protocols and preskeletons for annotations
annotationsKey :: String
annotationsKey = "annotations"

-- The key used in preskeletons for obligations
obligationsKey :: String
obligationsKey = "obligations"

-- The key used in preskeletons for communication orderings
precedesKey :: String
precedesKey = "precedes"

-- The key used in preskeletons for traces
tracesKey :: String
tracesKey = "traces"
