-- Security Goals

-- Copyright (c) 2015 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module CPSA.Lib.Goal where

import CPSA.Lib.Protocol

-- Syntax for the atomic formulas
data AForm t
  = RolePred (Role t) Int t
  | ParamPred (Role t) t t t
  | StrPrec t t
  | Prec t t
  | Non t
  | Pnon t
  | Uniq t
  | UniqAt t t
  | UgenAt t t
  | Ugen t
  | Equals t t
  deriving Show

data Goal t
  = Goal { uvars :: [t],          -- Universally quantified variables
           antec :: [AForm t],    -- Antecedent
           concl :: [[AForm t]] } -- Conclusion

-- Ordering used to sort by constructor order.
aFormOrder :: AForm t -> AForm t -> Ordering
aFormOrder (RolePred _ _ _) (RolePred _ _ _) = EQ
aFormOrder (RolePred _ _ _) (ParamPred _ _ _ _) = LT
aFormOrder (RolePred _ _ _) (StrPrec _ _) = LT
aFormOrder (RolePred _ _ _) (Prec _ _) = LT
aFormOrder (RolePred _ _ _) (Non _) = LT
aFormOrder (RolePred _ _ _) (Pnon _) = LT
aFormOrder (RolePred _ _ _) (Uniq _) = LT
aFormOrder (RolePred _ _ _) (UniqAt _ _) = LT
aFormOrder (RolePred _ _ _) (UgenAt _ _) = LT
aFormOrder (RolePred _ _ _) (Ugen _) = LT
aFormOrder (RolePred _ _ _) (Equals _ _) = LT
aFormOrder (ParamPred _ _ _ _) (RolePred _ _ _) = GT
aFormOrder (ParamPred _ _ _ _) (ParamPred _ _ _ _) = EQ
aFormOrder (ParamPred _ _ _ _) (StrPrec _ _) = LT
aFormOrder (ParamPred _ _ _ _) (Prec _ _) = LT
aFormOrder (ParamPred _ _ _ _) (Non _) = LT
aFormOrder (ParamPred _ _ _ _) (Pnon _) = LT
aFormOrder (ParamPred _ _ _ _) (Uniq _) = LT
aFormOrder (ParamPred _ _ _ _) (UniqAt _ _) = LT
aFormOrder (ParamPred _ _ _ _) (UgenAt _ _) = LT
aFormOrder (ParamPred _ _ _ _) (Ugen _) = LT
aFormOrder (ParamPred _ _ _ _) (Equals _ _) = LT
aFormOrder (StrPrec _ _) (RolePred _ _ _) = GT
aFormOrder (StrPrec _ _) (ParamPred _ _ _ _) = GT
aFormOrder (StrPrec _ _) (StrPrec _ _) = EQ
aFormOrder (StrPrec _ _) (Prec _ _) = LT
aFormOrder (StrPrec _ _) (Non _) = LT
aFormOrder (StrPrec _ _) (Pnon _) = LT
aFormOrder (StrPrec _ _) (Uniq _) = LT
aFormOrder (StrPrec _ _) (UniqAt _ _) = LT
aFormOrder (StrPrec _ _) (UgenAt _ _) = LT
aFormOrder (StrPrec _ _) (Ugen _) = LT
aFormOrder (StrPrec _ _) (Equals _ _) = LT
aFormOrder (Prec _ _) (RolePred _ _ _) = GT
aFormOrder (Prec _ _) (ParamPred _ _ _ _) = GT
aFormOrder (Prec _ _) (StrPrec _ _) = GT
aFormOrder (Prec _ _) (Prec _ _) = EQ
aFormOrder (Prec _ _) (Non _) = LT
aFormOrder (Prec _ _) (Pnon _) = LT
aFormOrder (Prec _ _) (Uniq _) = LT
aFormOrder (Prec _ _) (UniqAt _ _) = LT
aFormOrder (Prec _ _) (UgenAt _ _) = LT
aFormOrder (Prec _ _) (Ugen _) = LT
aFormOrder (Prec _ _) (Equals _ _) = LT
aFormOrder (Non _) (RolePred _ _ _) = GT
aFormOrder (Non _) (ParamPred _ _ _ _) = GT
aFormOrder (Non _) (StrPrec _ _) = GT
aFormOrder (Non _) (Prec _ _) = GT
aFormOrder (Non _) (Non _) = EQ
aFormOrder (Non _) (Pnon _) = LT
aFormOrder (Non _) (Uniq _) = LT
aFormOrder (Non _) (UniqAt _ _) = LT
aFormOrder (Non _) (UgenAt _ _) = LT
aFormOrder (Non _) (Ugen _) = LT
aFormOrder (Non _) (Equals _ _) = LT
aFormOrder (Pnon _) (RolePred _ _ _) = GT
aFormOrder (Pnon _) (ParamPred _ _ _ _) = GT
aFormOrder (Pnon _) (StrPrec _ _) = GT
aFormOrder (Pnon _) (Prec _ _) = GT
aFormOrder (Pnon _) (Non _) = GT
aFormOrder (Pnon _) (Pnon _) = EQ
aFormOrder (Pnon _) (Uniq _) = LT
aFormOrder (Pnon _) (UniqAt _ _) = LT
aFormOrder (Pnon _) (UgenAt _ _) = LT
aFormOrder (Pnon _) (Ugen _) = LT
aFormOrder (Pnon _) (Equals _ _) = LT
aFormOrder (Uniq _) (RolePred _ _ _) = GT
aFormOrder (Uniq _) (ParamPred _ _ _ _) = GT
aFormOrder (Uniq _) (StrPrec _ _) = GT
aFormOrder (Uniq _) (Prec _ _) = GT
aFormOrder (Uniq _) (Non _) = GT
aFormOrder (Uniq _) (Pnon _) = GT
aFormOrder (Uniq _) (Uniq _) = EQ
aFormOrder (Uniq _) (UniqAt _ _) = LT
aFormOrder (Uniq _) (UgenAt _ _) = LT
aFormOrder (Uniq _) (Ugen _) = LT
aFormOrder (Uniq _) (Equals _ _) = LT
aFormOrder (UniqAt _ _) (RolePred _ _ _) = GT
aFormOrder (UniqAt _ _) (ParamPred _ _ _ _) = GT
aFormOrder (UniqAt _ _) (StrPrec _ _) = GT
aFormOrder (UniqAt _ _) (Prec _ _) = GT
aFormOrder (UniqAt _ _) (Non _) = GT
aFormOrder (UniqAt _ _) (Pnon _) = GT
aFormOrder (UniqAt _ _) (Uniq _) = GT
aFormOrder (UniqAt _ _) (UniqAt _ _) = EQ
aFormOrder (UniqAt _ _) (UgenAt _ _) = LT
aFormOrder (UniqAt _ _) (Ugen _) = LT
aFormOrder (UniqAt _ _) (Equals _ _) = LT
aFormOrder (UgenAt _ _) (RolePred _ _ _) = GT
aFormOrder (UgenAt _ _) (ParamPred _ _ _ _) = GT
aFormOrder (UgenAt _ _) (StrPrec _ _) = GT
aFormOrder (UgenAt _ _) (Prec _ _) = GT
aFormOrder (UgenAt _ _) (Non _) = GT
aFormOrder (UgenAt _ _) (Pnon _) = GT
aFormOrder (UgenAt _ _) (Uniq _) = GT
aFormOrder (UgenAt _ _) (UniqAt _ _) = GT
aFormOrder (UgenAt _ _) (UgenAt _ _) = EQ
aFormOrder (UgenAt _ _) (Ugen _) = LT
aFormOrder (UgenAt _ _) (Equals _ _) = LT
aFormOrder (Ugen _) (RolePred _ _ _) = GT
aFormOrder (Ugen _) (ParamPred _ _ _ _) = GT
aFormOrder (Ugen _) (StrPrec _ _) = GT
aFormOrder (Ugen _) (Prec _ _) = GT
aFormOrder (Ugen _) (Non _) = GT
aFormOrder (Ugen _) (Pnon _) = GT
aFormOrder (Ugen _) (Uniq _) = GT
aFormOrder (Ugen _) (UniqAt _ _) = GT
aFormOrder (Ugen _) (UgenAt _ _) = GT
aFormOrder (Ugen _) (Ugen _) = EQ
aFormOrder (Ugen _) (Equals _ _) = LT
aFormOrder (Equals _ _) (RolePred _ _ _) = GT
aFormOrder (Equals _ _) (ParamPred _ _ _ _) = GT
aFormOrder (Equals _ _) (StrPrec _ _) = GT
aFormOrder (Equals _ _) (Prec _ _) = GT
aFormOrder (Equals _ _) (Non _) = GT
aFormOrder (Equals _ _) (Pnon _) = GT
aFormOrder (Equals _ _) (Uniq _) = GT
aFormOrder (Equals _ _) (UniqAt _ _) = GT
aFormOrder (Equals _ _) (UgenAt _ _) = GT
aFormOrder (Equals _ _) (Ugen _) = GT
aFormOrder (Equals _ _) (Equals _ _) = EQ
