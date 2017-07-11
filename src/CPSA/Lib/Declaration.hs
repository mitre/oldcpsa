-- Declaration for contraints

-- Copyright (c) 2012 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module CPSA.Lib.Declaration
       (Declarations,
        dknon, dkpnon, dkunique, dkuniqFull, mkDecls, declsUnion, dkuniqgen,
        tagDeclsTermsOnly, tagDeclsLocsOnly, declFormats, declInputFormats,
        dterms, dlocs, daux, declInst, declInstAux, DeclInst, dkugenFull,
        DeclInstList, Declaration, DeclInFormat(..), DeclOutFormat(..),
        DeclList, declarationRoleTags,
--        BasicOutFmt, MultiTermOutFmt, NullOutFmt, BasicRoleOutFmt, GeneralOutFmt,
--        BasicInFmt, MultiTermInFmt, NullInFmt, TwoTermInFmt,
        declCheck, Loc, avoidTerms,
        forgetSomeDecls, declsNub, declarationTags, tagDecls, declCheckOrigs,
        declsMapTerms, declsMapLocations, declsFilterValid, validateDeclMap)
       where

import Data.List(delete, nub, sortBy)
import qualified Data.Set as S
import Data.Set (Set)
import CPSA.Lib.Utilities
import CPSA.Lib.Algebra
import CPSA.Lib.AlgebraLibrary

{-- Debugging support
import CPSA.Lib.Debug
-- Also see showst
--}

class (Eq l, Show l) => Loc l

instance Loc Int
instance (Loc a, Loc b) => Loc (a, b)

-- exported

-- List of formats for input of declarations
-- Fields: (fmt, areq, aux)
--   fmt specifies a descriptive choice for the format, describing
--   contraints on numbers of terms and nodes in each declared item.
--   aux is a boolean, specifying whether a sub-name is used.
--   areq = True means that all the terms must be atoms; this is in
--   input requirements only.
data DeclInFormat =
   BasicInFmt | MultiTermInFmt | NullInFmt | TwoTermInFmt | TermLocInFmt
   deriving Eq

declInputFormats :: [(DeclInFormat, Bool, Bool)]
declInputFormats =
   [(BasicInFmt, True, False),    -- non, pen-non, uniq
    (TwoTermInFmt, False, False),   -- neq, lt
    (TwoTermInFmt, False, True),    -- fn-of
    (NullInFmt, False, True),     -- inst-limit
    (MultiTermInFmt, False, False),  -- neq-list
    (BasicInFmt, True, True),      -- subsort
    (TermLocInFmt, False, False)   -- uniq-gen
   ]

-- List of formats for output of declarations
-- Fields: (fmt, aux)
--   fmt specifies a descriptive choice for the format, describing
--   contraints on numbers of terms and nodes in each declared item.
--   aux is a boolean, specifying whether a sub-name is used.
data DeclOutFormat =
   BasicOutFmt | MultiTermOutFmt | NullOutFmt | BasicRoleOutFmt | GeneralOutFmt | TermLocOutFmt
   deriving Eq

declOutputFormats :: [(DeclOutFormat, Bool)]
declOutputFormats =
  [(BasicOutFmt, False),
   (MultiTermOutFmt, False),
   (MultiTermOutFmt, True),
   (NullOutFmt, True),
   (BasicOutFmt, True),
   (BasicRoleOutFmt, False),
   (TermLocOutFmt, False),
   (GeneralOutFmt, False)]

defaultOutFormat :: Int
defaultOutFormat = ((length declOutputFormats) - 1)

-- Entries are of the form (tag, (infmt, outfmt))
-- tag is associated with indices into declInputFormats
-- and declOutputFormats respectively.
declFormats :: [(String, (Int, Int))]
declFormats =
   [("non-orig", (0, 0)),
    ("pen-non-orig", (0, 0)),
    ("uniq-orig", (0, 0)),
    ("uniq-gen", (0, 0)),
    ("fn-of", (2, 2)),
--  ("inst-limit", (3, 3)),
    ("lt", (1, 1)),
    ("neq", (1, 1)),
    ("neqlist", (4, defaultOutFormat)),
    ("subsort", (5, 4)),
    ("ind-zero", (0, 0)),
--    ("ind-zero-at", (0, 0)),
    ("ind-zero-in", (1, 1))
   ]

-- Formats for use in roles.
declRFormats :: [(String, (Int, Int))]
declRFormats =
   [("non-orig", (0, 5)),
    ("pen-non-orig", (0, 5))] ++ (drop 2 declFormats)

data DeclInst t l = DeclInst
     {
        dterms :: [t],
        dlocs :: [l],
        daux :: String }
     deriving (Show, Eq)

-- Exported
declInstAux :: [t] -> [l] -> String ->
               DeclInst t l
declInstAux ts ls aux =
   DeclInst { dterms = ts, dlocs = ls, daux = aux }

declInst :: [t] -> [l] -> DeclInst t l
declInst ts ls = declInstAux ts ls ""

type DeclInstList t l = [DeclInst t l]
type Declaration t l = (String, DeclInstList t l)
type DeclList t l = [Declaration t l]
data Declarations t l = Declarations
     {
       dlist :: DeclList t l }
     deriving Show

mkDecls :: (Algebra t p g s e c, Loc l) => DeclList t l ->
           Declarations t l
mkDecls dl = declsNub Declarations { dlist = dl }

dnon :: Declarations t l -> DeclInstList t l
dnon decls = tagDecls "non-orig" decls

dpnon :: Declarations t l -> DeclInstList t l
dpnon decls = tagDecls "pen-non-orig" decls

dunique :: Declarations t l -> DeclInstList t l
dunique decls = tagDecls "uniq-orig" decls

duniqgen :: Declarations t l -> DeclInstList t l
duniqgen decls = tagDecls "uniq-gen" decls

-- Exported
dknon :: Declarations t l -> [t]
dknon d = map head $ map dterms (dnon d)

-- Exported
dkpnon :: Declarations t l -> [t]
dkpnon d = map head $ map dterms (dpnon d)

-- Exported
dkunique :: Declarations t l -> [t]
dkunique d = map head $ map dterms (dunique d)

dkuniqgen :: Declarations t l -> [t]
dkuniqgen d = map head $ map dterms (duniqgen d)

-- Exported
dkuniqFull :: Declarations t l -> [(t,l)]
dkuniqFull d = map (\x -> (head $ dterms x, head $ dlocs x)) (dunique d)

-- Exported
dkugenFull :: Declarations t l -> [(t, Maybe l)]
dkugenFull d = map (\x -> foo x) (duniqgen d)
               where
                 foo x
                   | (length (dlocs x) == 0) = assertError
                       ("Declaration:dkugenFull something odd happened") -- (head $ dterms x, Nothing)
                   | otherwise = (head $ dterms x, Just (head $ dlocs x))

modname :: (Algebra t p g s e c, Loc l) => String -> DeclInstList t l ->
           Declarations t l -> Declarations t l
modname name ds decls =
  case lookup name (dlist decls) of
    Nothing -> mkDecls ((dlist decls) ++ [(name, ds)])
    Just _ -> mkDecls (map replaceWithds (dlist decls))
  where
    replaceWithds (n, ds') = if (n == name) then (n, ds)
                                          else (n, ds')

-- Exported
-- Unions together a list of Declarations
declsUnion :: (Algebra t p g s e c, Loc l) => [Declarations t l] ->
              Declarations t l
declsUnion [] = mkDecls []
declsUnion [d] = d
declsUnion (d:ds) = mkDecls $ declsMerge (dlistSort $ dlist (declsUnion ds))
                                         (dlistSort $ dlist d)

-- Unions together a pair of Declarations
declsMerge :: (Algebra t p g s e c, Loc l) => DeclList t l ->
              DeclList t l -> DeclList t l
declsMerge dl dl'
  | length dl < length dl' = declsMerge dl' dl  -- Can now assume |dl| >= |dl'|
  | null dl = dl'
  | null dl' = dl
  | firstName dl == firstName dl' =
       [(firstName dl, firstDs dl' ++ firstDs dl)] ++ declsMerge (tail dl) (tail dl')
  | declCompare (head dl) (head dl') == LT =
       head dl : declsMerge (tail dl) dl'
  | otherwise = head dl' : declsMerge dl (tail dl')
  where
    firstName dlst = fst (head dlst)
    firstDs dlst = snd (head dlst)

dlistSort :: DeclList t l -> DeclList t l
dlistSort dl = sortBy declCompare dl

declCompare :: Declaration t l -> Declaration t l -> Ordering
declCompare dec1 dec2 = compare (fst dec1) (fst dec2)

-- Exported
-- MDL: Note "Forgot t" limitation here (use of head)
forgetSomeDecls :: (Algebra t p g s e c, Loc l) => Declarations t l ->
                   [(t, Declarations t l)]
forgetSomeDecls decls =
  concatMap delNamedDecl (map fst (dlist decls))
  where
    delNamedDecl name =
      [ (head $ dterms d, modname name (delete d (tagDecls name decls)) decls)
      | d <- tagDecls name decls ]

declsNub :: (Algebra t p g s e c, Loc l) => Declarations t l ->
            Declarations t l
declsNub d =
  -- Do not use the wrap constructor here, since it calls declsNub!
  Declarations {dlist = (filter declNonEmpty (map nubDecl (dlist d)))}
  where
    nubDecl (name, ds) = (name, nub ds)
    declNonEmpty (_, ds) = not (null ds)

-- Exported
-- Classifies the declaration tags used in the given Declarations according to format.
declarationTags :: Declarations t l -> [(String, (DeclOutFormat, Bool))]
declarationTags d = nub $ declarationTagsCore d declFormats

-- Exported
declarationRoleTags :: Declarations t l -> [(String, (DeclOutFormat, Bool))]
declarationRoleTags d = nub $ declarationTagsCore d declRFormats

-- Common code for declarationRoleTags and declarationTags.
-- Looks up the format associated each declaration and pairs the tag with its format.
-- Since subtags are subsequent words in the tag string, we look up by the first word.
declarationTagsCore :: Declarations t l ->
                   [(String, (Int, Int))] -> [(String, (DeclOutFormat, Bool))]
declarationTagsCore d fmts =
   map (\ tag -> (tag, declOutputFormats !! (getOutFormat tag))) dtags
   where
     dtags = map fst (dlist d)
     getOutFormat tag = case lookup (tag) fmts of
                          Nothing -> defaultOutFormat
                          Just (_,out) -> out

-- Exported
tagDeclsTermsOnly :: String -> Declarations t l -> [t]
tagDeclsTermsOnly tag decls = map head $ map dterms $ tagDecls tag decls

-- Exported
tagDeclsLocsOnly :: String -> Declarations t l -> [l]
tagDeclsLocsOnly tag decls = map head $ map dlocs $ tagDecls tag decls

-- Exported
tagDecls :: String -> Declarations t l -> DeclInstList t l
tagDecls tag decls =
    case lookup tag (dlist decls) of
      Nothing -> []
      Just x -> x

-- Exported
declCheckOrigs :: (Algebra t p g s e c, Loc l) => Declarations t l ->
                  Declarations t l -> (g, e) -> Bool
declCheckOrigs d d' env =
  (S.fromList (map fst $ dlist d) == S.fromList (map fst $ dlist d')) &&
  (any matchRenaming (checkAllDecls [env] d d' (map fst $ dlist d)))

checkAllDecls :: (Algebra t p g s e c, Loc l) => [(g,e)] -> Declarations t l ->
                  Declarations t l -> [String] -> [(g,e)]
checkAllDecls envs _ _ [] = envs
checkAllDecls envs d d' (tag:tags) = checkAllDecls envs' d d' tags
  where
    envs' =
      [env' | env <- envs,
       env' <- checkDecl env (tagDecls tag d) (tagDecls tag d')]

checkDecl :: (Algebra t p g s e c, Loc l) => (g,e) -> [DeclInst t l] ->
             [DeclInst t l] -> [(g,e)]
checkDecl env [] [] = [env]
checkDecl env (i:is) is' =
  do
    i' <- filter (\inst -> (length (dterms i) == length (dterms inst)) &&
                           (dlocs i == dlocs inst) &&
                           (daux i == daux inst)) is'
    env' <- matchMany (dterms i) (dterms i') env
    checkDecl env' is (delete i' is')
checkDecl _ _ _ = []

-- Exported
declsMapTerms :: (Algebra t p g s e c, Loc l) => (t -> t) ->
                 Declarations t l -> Declarations t l
declsMapTerms f decls =
  mkDecls (map fdecls (dlist decls))
  where
    fdecls (name, ds) = (name, map g ds)
    g dinst = declInstAux (map f $ dterms dinst) (dlocs dinst) (daux dinst)

-- Exported
declsMapLocations :: (Algebra t p g s e c, Loc l') => (l -> l') ->
                     Declarations t l -> Declarations t l'
declsMapLocations f decls =
  mkDecls (map fdecls (dlist decls))
  where
    fdecls (name, ds) = (name, map g ds)
    g dinst = declInstAux (dterms dinst) (map f $ dlocs dinst) (daux dinst)

-- Exported
declsFilterValid :: (Algebra t p g s e c, Loc l) => [t] -> (l -> Bool) ->
                    Declarations t l -> Declarations t l
declsFilterValid terms locValid d =
  mkDecls (map filterAllDecls (dlist d))
  where
    filterAllDecls (n, ds) = (n, filter mentionedIn ds)
    mentionedIn dinst = varSubset (dterms dinst) terms && all locValid (dlocs dinst)

-- Exported
validateDeclMap :: (Algebra t p g s e c, Loc l, Loc l') =>
                   Declarations t l -> Declarations t l' ->
                   (l -> l') -> e -> Bool
validateDeclMap d d' locmap env =
    all okTag (map fst $ dlist d)
  where
    okTag tag = all (flip elem (tagDecls tag d')) (map f (tagDecls tag d))
    f dinst = declInstAux (map (instantiate env) $ dterms dinst)
                       (map locmap $ dlocs dinst) (daux dinst)

avoidTerms :: Algebra t p g s e c => Declarations t l -> Set t
avoidTerms decls =
  S.unions [ns, as, uos, ugs]
    where
      ns = S.fromList (dknon decls)
      as = S.fromList (dkpnon decls)
      uos = S.fromList (dkunique decls)
      ugs = S.fromList (dkuniqgen decls)

-------------------------- Constraint checking, where possible --------------
declCheck :: Algebra t p g s e c => Declarations t l -> (Bool, String)
declCheck d =
   case doDeclCheck d of
     Return _ -> (True, "")
     Fail str -> (False, str)
   where
     doDeclCheck d =
       do
         failwith "inequality condition violated" $ neqCheck d
         failwith "lt declarations form a cycle" $ ltCheck d
         failwith "subsort requirements violated" $ subsortCheck d
         failwith "[ASSERT FAILED] inst limit requirements violated" $ instlimitCheck d

instlimitCheck :: Declarations t l -> Bool
instlimitCheck _ = True

neqCheck :: Algebra t p g s e c => Declarations t l -> Bool
neqCheck decls =
   null failures
   where
      failures = [s | s <- dts,
                  length s >= 2,
                  i <- nats (length s),
                  j <- nats (length s),
                  i /= j,
                  s !! i == s !! j]
      dts = map dterms ((tagDecls "neq" decls) ++
                        (tagDecls "neqlist" decls))

ltCheck :: Algebra t p g s e c => Declarations t l -> Bool
ltCheck decls =
   checkAll pairs pairs ((length pairs)-1)
   where
     checkAll ltpairs allpairs n
        | any (\ ts -> length ts < 2) (ltpairs ++ allpairs) = assertError ("Bug in ltcheck!")
        | any (\ ts -> ts !! 0 == ts !! 1) allpairs = False
        | n > 0 = checkAll ltpairs (evlv ltpairs allpairs) (n-1)
        | otherwise = True
     evlv x y = [[(ts1 !! 0),(ts2 !! 1)]| ts1 <- y, ts2 <- x, ts1 !! 1 == ts2 !! 0]
     pairs = map dterms (tagDecls "lt" decls)

subsortCheck :: Algebra t p g s e c => Declarations t l -> Bool
subsortCheck decls =
   null failures
   where
     failures = [t1 | (tag1,t1) <- pairs, (tag2,t2) <- pairs, t1 == t2, tag1 /= tag2]
     pairs = map (\ di -> (daux di, head $ dterms di)) $ tagDecls "subsort" decls
