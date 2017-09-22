-- Displays protocols and preskeletons as S-expressions.

-- Copyright (c) 2009 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module CPSA.Lib.Displayer (displayProt, displayPreskel, displayNode) where

import qualified Data.List as L
import qualified Data.Set as S
import CPSA.Lib.SExpr
import CPSA.Lib.Algebra
import CPSA.Lib.Declaration
import CPSA.Lib.State
import CPSA.Lib.Protocol
import CPSA.Lib.Strand
import CPSA.Lib.Utilities

{-- Debugging support
import CPSA.Lib.Debug
--}

-- Display of protocols

displayProt :: Algebra t p g s e c => Prot t g -> SExpr ()
displayProt p =
    L () (S () "defprotocol" : S () (pname p) : S () (alg p) : rs)
    where
      rs = foldl f (pcomment p) (reverse (roles p))
      f rs r = displayRole r : rs

displayRole :: Algebra t p g s e c => Role t -> SExpr ()
displayRole r =
    L () (S () "defrole" :
          S () (rname r) :
          L () (S () "vars" : displayVars ctx vars) :
          L () (S () "trace" : displayTrace ctx (rtrace r)) :
          displayRoleDeclarations ctx (rdecls r)
           (rcomment r))
    where
      ctx = varsContext vars
      vars = rvars r

varsContext :: Algebra t p g s e c => [t] -> c
varsContext vars =
    addToContext emptyContext vars

displayTerms :: Algebra t p g s e c => c -> [t] -> [SExpr ()]
displayTerms ctx ts = map (displayTerm ctx) (L.sort ts)

displayTermsUnsorted ::Algebra t p g s e c => c -> [t] -> [SExpr ()]
displayTermsUnsorted ctx ts = map (displayTerm ctx) ts

displayTermPlusMaybeOneInts :: Algebra t p g s e c => c ->
                               RoleDeclInstList t -> [SExpr ()]
displayTermPlusMaybeOneInts ctx ts = map (displayTermPlusMaybeOneInt ctx) ts

displayTermPlusMaybeOneInt :: Algebra t p g s e c => c ->
                              RoleDeclInst t -> SExpr ()
displayTermPlusMaybeOneInt ctx dinst
    | (length (dlocs dinst) > 1) || (length (dterms dinst) /= 1) =
      assertError("Displayer.displayTermPlusMaybeOneInt: patterns unexpectedly exhausted in displayer")
    | (length (dlocs dinst) == 1) = L () [displayTerm ctx
                                          (head (dterms dinst)),
                                          N () ((dlocs dinst)!!0)]
    | otherwise = displayTerm ctx (head (dterms dinst))

displayTermPlusMaybeOneNodes :: Algebra t p g s e c => c ->
                               SkelDeclInstList t -> [SExpr ()]
displayTermPlusMaybeOneNodes ctx ts = map (displayTermPlusMaybeOneNode ctx) ts

displayTermPlusMaybeOneNode :: Algebra t p g s e c => c ->
                              SkelDeclInst t -> SExpr ()
displayTermPlusMaybeOneNode ctx dinst
    | (length (dlocs dinst) > 1) || (length (dterms dinst) /= 1) =
      assertError("Displayer.displayTermPlusMaybeOneNode patterns unexpectedly exhausted in displayer")
    | (length (dlocs dinst) == 1) = L () [displayTerm ctx
                                          (head (dterms dinst)),
                                          L () [N () $ fst $ (dlocs dinst)!!0,
                                                N () $ snd $ (dlocs dinst)!!0]]
    | otherwise = displayTerm ctx (head (dterms dinst))

displayOptional :: String -> [SExpr ()] -> [SExpr ()] -> [SExpr ()]
displayOptional _ [] rest = rest
displayOptional key value rest =
    L () (S () key : value) : rest

displayTrace :: Algebra t p g s e c => c ->
                Trace t -> [SExpr ()]
displayTrace ctx trace =
    map displayDt trace
    where
      displayDt (In t) = L () [S () "recv", displayTerm ctx t]
      displayDt (Out t) = L () [S () "send", displayTerm ctx t]
      displayDt (Sync t) = L () (displayTran ctx t)

displayTran :: Algebra t p g s e c => c -> Tran t -> [SExpr ()]
displayTran ctx (Tran(Just now, Just next, Nothing)) =
  [S () "tran", displayTerm ctx now,  displayTerm ctx next]
displayTran ctx (Tran(Just now, Just next, Just label)) =
  [S () "tran", displayTerm ctx now,  displayTerm ctx next,
                displayTerm ctx label]
displayTran ctx (Tran(Just now, Nothing, Nothing)) =
  [S () "obsv", displayTerm ctx now]
displayTran ctx (Tran(Just now, Nothing, Just label)) =
  [S () "obsv", displayTerm ctx now, displayTerm ctx label]
displayTran ctx (Tran(Nothing, Just next, Nothing)) =
  [S () "init", displayTerm ctx next]
displayTran ctx (Tran(Nothing, Just next, Just label)) =
  [S () "init", displayTerm ctx next, displayTerm ctx label]
displayTran _ (Tran(Nothing, Nothing, _)) =
  assertError("Displayer.displayTran: encountered sync node with current or next state")

-- Display of preskeletons

displayPreskel :: Algebra t p g s e c => Preskel t g s e ->
                  [SExpr ()] -> SExpr ()
displayPreskel k rest =
    L () (S () "defskeleton" :
          S () (pname (protocol k)) :
          L () (S () "vars" : displayVars ctx vars) :
          foldr f (displayRest k ctx rest) (insts k))
    where
      ctx = varsContext vars
      vars = kvars k
      f i rest = displayInst ctx i : rest

-- Display the remainder of a preskeleton
displayRest :: Algebra t p g s e c => Preskel t g s e ->
               c -> [SExpr ()] -> [SExpr ()]
displayRest k ctx rest =
    displayOptional "precedes" (displayOrdering (orderings k))
     (displayOptional "leadsto" (displayOrdering (leadsto k))
      (displayOptional "priority" (displayPriorities (kpriorities k))
       (displaySkelDeclarations ctx (decls k)
        (kcomment k ++
         (displayOperation k ctx
          (displayOptional "traces" traces rest))))))
  where
    traces = map (L () . displayTrace ctx . trace) (insts k)

-- Display declarations
displaySkelDeclarations :: Algebra t p g s e c =>
                       c -> SkelDeclarations t ->
                       [SExpr ()] -> [SExpr ()]
displaySkelDeclarations ctx decls rest =
    displaySkelDeclarationList ctx (declarationTags decls) decls rest

displayRoleDeclarations :: Algebra t p g s e c =>
                       c -> RoleDeclarations t ->
                       [SExpr ()] -> [SExpr ()]
displayRoleDeclarations ctx decls rest =
    displayRoleDeclarationList ctx (declarationRoleTags decls) decls rest

-- Display an individually tagged declaration
displaySkelDeclarationList :: Algebra t p g s e c => c ->
                              [(String, (DeclOutFormat, Bool))] ->
                              SkelDeclarations t -> [SExpr ()] -> [SExpr ()]
displaySkelDeclarationList _ [] _ rest = rest
displaySkelDeclarationList ctx (a:b) decls rest =
    displaySkelDeclaration ctx a decls
      (displaySkelDeclarationList ctx b decls rest)

displayRoleDeclarationList :: Algebra t p g s e c => c ->
                              [(String, (DeclOutFormat, Bool))] ->
                              RoleDeclarations t -> [SExpr ()] -> [SExpr ()]
displayRoleDeclarationList _ [] _ rest = rest
displayRoleDeclarationList ctx (a:b) decls rest =
    displayRoleDeclaration ctx a decls
      (displayRoleDeclarationList ctx b decls rest)

displaySkelDeclaration :: Algebra t p g s e c => c ->
                          (String, (DeclOutFormat, Bool)) ->
                          SkelDeclarations t -> [SExpr ()] -> [SExpr ()]
displaySkelDeclaration ctx (tag, (BasicOutFmt, True)) decls rest =
    displaySkelAuxDeclaration ctx tag (tagDecls tag decls) rest
displaySkelDeclaration ctx (tag, (MultiTermOutFmt, True)) decls rest =
    displaySkelAuxDeclaration2 ctx tag (tagDecls tag decls) rest
displaySkelDeclaration ctx (tag, (BasicOutFmt, False)) decls rest =
    displayOptional tag (displayTerms ctx (tagDeclsTermsOnly tag decls)) rest
displaySkelDeclaration ctx (tag, (GeneralOutFmt, False)) decls rest =
    displayOptional tag (displaySkelDeclGen ctx (tagDecls tag decls)) rest
displaySkelDeclaration _ (tag, (LocOutFmt, False)) decls rest =
    displayOptional tag (displaySkelDeclLoc (tagDecls tag decls)) rest
displaySkelDeclaration ctx (tag, (BasicRoleOutFmt, False)) decls rest =
    displayOptional tag (displayTermPlusMaybeOneNodes ctx (tagDecls tag decls)) rest
displaySkelDeclaration ctx (tag, (MultiTermOutFmt, False)) decls rest =
    displayOptional tag (map (\ item -> (L () (displayTermsUnsorted ctx (dterms item))))
                         (tagDecls tag decls)) rest
displaySkelDeclaration _ (tag,_) _ _ =
    assertError("Displayer.displaySkelDeclaration: skel output format not supported yet: " ++ tag)

displaySkelAuxDeclaration :: Algebra t p g s e c => c -> String ->
        SkelDeclInstList t -> [SExpr ()] -> [SExpr ()]
displaySkelAuxDeclaration ctx tag dis rest =
    (L () (S () tag:(dAuxDecl (L.sort (L.nub $ map daux dis)) )):rest)
    where
      dAuxDecl [] = []
      dAuxDecl (stag:r) = ((L () (S () stag:
                                  (displayTerms ctx
                                   (concatMap dterms
                                    (filter (\di -> daux di == stag) dis)))))
                           :dAuxDecl r)

displaySkelAuxDeclaration2 :: Algebra t p g s e c => c -> String ->
        SkelDeclInstList t -> [SExpr ()] -> [SExpr ()]
displaySkelAuxDeclaration2 ctx tag dis rest =
    (L () (S () tag:(dAuxDecl (L.sort (L.nub $ map daux dis)))):rest)
    where
      dAuxDecl [] = []
      dAuxDecl (stag:r) = ((L () (S () stag:
                                  (dAuxDecl2 (filter (\di -> daux di == stag) dis)))):
                           (dAuxDecl r))
      dAuxDecl2 [] = []
      dAuxDecl2 (di:r) = ((L () (displayTermsUnsorted ctx (dterms di))):
                          (dAuxDecl2 r))

displayRoleAuxDeclaration :: Algebra t p g s e c => c -> String ->
        RoleDeclInstList t -> [SExpr ()] -> [SExpr ()]
displayRoleAuxDeclaration ctx tag dis rest =
    (L () (S () tag:(dAuxDecl (L.sort (L.nub $ map daux dis)) )):rest)
    where
      dAuxDecl [] = []
      dAuxDecl (stag:r) = ((L () (S () stag:
                                  (displayTerms ctx
                                   (concatMap dterms
                                    (filter (\di -> daux di == stag) dis)))))
                           :dAuxDecl r)

displayRoleAuxDeclaration2 :: Algebra t p g s e c => c -> String ->
        RoleDeclInstList t -> [SExpr ()] -> [SExpr ()]
displayRoleAuxDeclaration2 ctx tag dis rest =
    (L () (S () tag:(dAuxDecl (L.sort (L.nub $ map daux dis)))):rest)
    where
      dAuxDecl [] = []
      dAuxDecl (stag:r) = ((L () (S () stag:
                                  (dAuxDecl2 (filter (\di -> daux di == stag) dis)))):
                           (dAuxDecl r))
      dAuxDecl2 [] = []
      dAuxDecl2 (di:r) = ((L () (displayTermsUnsorted ctx (dterms di))):
                          (dAuxDecl2 r))

displayRoleDeclaration :: Algebra t p g s e c => c ->
                          (String, (DeclOutFormat, Bool)) ->
                          RoleDeclarations t -> [SExpr ()] -> [SExpr ()]
displayRoleDeclaration ctx (tag, (BasicOutFmt, True)) decls rest =
    displayRoleAuxDeclaration ctx tag (tagDecls tag decls) rest
displayRoleDeclaration ctx (tag, (BasicOutFmt, False)) decls rest =
    displayOptional tag (displayTerms ctx (tagDeclsTermsOnly tag decls)) rest
displayRoleDeclaration ctx (tag, (MultiTermOutFmt, True)) decls rest =
    displayRoleAuxDeclaration2 ctx tag (tagDecls tag decls) rest
displayRoleDeclaration ctx (tag, (MultiTermOutFmt, False)) decls rest =
    displayOptional tag (map (\ item -> (L () (displayTermsUnsorted ctx (dterms item))))
                         (tagDecls tag decls)) rest
displayRoleDeclaration ctx (tag, (BasicRoleOutFmt, False)) decls rest =
    displayOptional tag (displayTermPlusMaybeOneInts ctx (tagDecls tag decls)) rest
displayRoleDeclaration ctx (tag, (GeneralOutFmt, False)) decls rest =
    displayOptional tag (displayRoleDeclGen ctx (tagDecls tag decls)) rest
displayRoleDeclaration _ (tag,_) _ _ =
    assertError("Displayer.displayRoleDeclaration: role output format not supported yet: " ++ tag)

displaySkelDeclGen :: (Algebra t p g s e c) => c -> SkelDeclInstList t ->
                      [SExpr ()]
displaySkelDeclGen _ [] = []
displaySkelDeclGen ctx (d:ds) =
    displaySkelDeclGenItem ctx d (displaySkelDeclGen ctx ds)

displayRoleDeclGen :: (Algebra t p g s e c) => c -> RoleDeclInstList t ->
                      [SExpr ()]
displayRoleDeclGen _ [] = []
displayRoleDeclGen ctx (d:ds) =
    displayRoleDeclGenItem ctx d (displayRoleDeclGen ctx ds)

displaySkelDeclGenItem :: (Algebra t p g s e c) => c -> SkelDeclInst t ->
                          [SExpr ()] -> [SExpr ()]
displaySkelDeclGenItem ctx dinst rest =
    (L () ((L () (displayTermsUnsorted ctx ts)):(displayNodes ns)): rest)
    where
        ts = dterms dinst
        ns = dlocs dinst

displayRoleDeclGenItem :: (Algebra t p g s e c) => c -> RoleDeclInst t ->
                          [SExpr ()] -> [SExpr ()]
displayRoleDeclGenItem ctx dinst rest =
    (L () ((L () (displayTermsUnsorted ctx ts)):(displayInts ns)): rest)
    where
        ts = dterms dinst
        ns = dlocs dinst

displaySkelDeclLoc :: (Algebra t p g s e c) => SkelDeclInstList t ->
                      [SExpr ()]
displaySkelDeclLoc [] = []
displaySkelDeclLoc (d:ds) =
    displaySkelDeclLocItem d (displaySkelDeclLoc ds)

displaySkelDeclLocItem :: (Algebra t p g s e c) => SkelDeclInst t ->
                          [SExpr ()] -> [SExpr ()]
displaySkelDeclLocItem dinst rest =
    displayNodes ns ++ rest
    where
      ns = dlocs dinst

displayInst :: Algebra t p g s e c => c ->
               Instance t e -> SExpr ()
displayInst ctx s =
    case listenerTerm s of
      Just t -> L () [S () "deflistener", displayTerm ctx t]
      Nothing ->
          L () (S () "defstrand" :
                S () (rname r) :
                N () (height s) :
                map (displayMaplet rctx ctx) maplets)
          where
            r = role s
            domain = rvars r
            maplets = L.sort (reify domain (env s))
            rctx = varsContext domain

displayMaplet :: Algebra t p g s e c => c -> c -> (t, t) -> SExpr ()
displayMaplet domain range (x, t)=
    L () [displayTerm domain x, displayTerm range t]

displayPriorities :: [(Node,Int)] -> [SExpr ()]
displayPriorities priorities =
    map displayPriority priorities

displayPriority :: (Node,Int) -> SExpr ()
displayPriority ((s,i),p) =
    L () [L () [N () s, N () i], N () p]

displayOrdering :: [Pair] -> [SExpr ()]
displayOrdering orderings =
    map displayPair (L.sort orderings)

displayPair :: Pair -> SExpr ()
displayPair (n0, n1) =
    L () [displayNode n0, displayNode n1]

displayInts :: [Int] -> [SExpr ()]
displayInts [] = []
displayInts (i:is) = (N () i):(displayInts is)

displayNodes :: [Node] -> [SExpr ()]
displayNodes [] = []
displayNodes (n:ns) = [displayNode n] ++ displayNodes ns

displayNode :: Node -> SExpr ()
displayNode (s, p) = L () [N () s, N () p]

-- Display the reason the preskeleton was created
displayOperation :: Algebra t p g s e c => Preskel t g s e ->
                    c -> [SExpr ()] -> [SExpr ()]
displayOperation k ctx rest =
    case operation k of
      New -> rest
      Contracted subst cause ->
          let substitution = displaySubst ctx subst in
          displayCause (L () (S () "contracted" : substitution)) cause
      Displaced s s' role height cause ->
          displayCause
          (L () [S () "displaced", N () s, N () s', S () role, N () height])
          cause
      AddedStrand role height cause ->
          displayCause
          (L () [S () "added-strand", S () role, N () height]) cause
      AddedListener t cause ->
          displayCause (L () [S () "added-listener", displayOpTerm ctx t]) cause
      AddedAbsence t1 t2 cause ->
          displayCause (L () [S () "added-absence", displayOpTerm ctx t1,
                                displayOpTerm ctx t2]) cause
      AlgebraSolved subst cause ->
          let substitution = displaySubst ctx subst in
          displayCause (L () (S () "algebra-contracted" : substitution)) cause
      Generalized method ->
          let desc = displayMethod ctx method in
          L () (S () "operation" : S () "generalization" : desc) : rest
      Collapsed s s' ->
          let desc = [N () s, N () s'] in
          L () (S () "operation" : S () "collapsed" : desc) : rest
    where
      displayCause op (Cause dir node critical escape) =
          L () (S () "operation" :
                displayDirection dir :
                op :
                displayOpTerm ctx critical :
                displayNode node :
                displayOpTerms ctx (S.toList escape)) : rest
      displayDirection Encryption = S () "encryption-test"
      displayDirection Nonce = S () "nonce-test"
      displayDirection StatePassing = S () "state-passing-test"
      displayMethod _ (Deleted node) =
          [S () "deleted", displayNode node]
      displayMethod _ (Weakened (n0, n1)) =
          [S () "weakened", L () [displayNode n0, displayNode n1] ]
      displayMethod ctx (Separated t) =
          [S () "separated", displayOpTerm ctx t]
      displayMethod ctx (Forgot t) =
          [S () "forgot", displayOpTerm ctx t]

-- Terms in the operation field may contain variables not in the skeleton
displayOpTerm :: Algebra t p g s e c => c -> t -> SExpr ()
displayOpTerm ctx t = displayTerm (addToContext ctx [t]) t

displayOpTerms :: Algebra t p g s e c => c -> [t] -> [SExpr ()]
displayOpTerms ctx ts = map (displayTerm (addToContext ctx ts)) (L.sort ts)
