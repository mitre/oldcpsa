-- Expands macros using definitions in the input.

-- Copyright (c) 2009 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

{-# LANGUAGE CPP #-}

#if !(MIN_VERSION_base(4,13,0))
#define MonadFail Monad
#endif

module CPSA.Lib.Expand (expand, readSExprs, Macro,
                        expandSExpr, bound, getMacroName,
                        getMacroArgs, getMacroBody) where

import Control.Monad
import System.IO (openFile, IOMode (ReadMode))
import CPSA.Lib.Entry (readSExpr, tryIO)
import CPSA.Lib.SExpr

-- The macroexpand loop limit
limit :: Int
limit = 1000

-- Include bound
bound :: Int
bound = 16

expand :: [SExpr Pos] -> IO [SExpr Pos]
expand sexprs =
    do
      (_, sexprs) <- foldM (expandSExpr bound) ([], []) sexprs
      return (reverse sexprs)

expandSExpr :: Int -> ([Macro], [SExpr Pos]) -> SExpr Pos ->
               IO ([Macro], [SExpr Pos])
expandSExpr _ (macs, sexprs) (L pos (S _ "defmacro" : xs)) =
    do                          -- Process a macro definition
      mac <- defmacro pos xs
      return (mac : macs, sexprs)
expandSExpr bound (macs, sexprs) (L pos [S _ "include", Q _ file]) =
    if bound <= 0 then
      fail (shows pos ("Include depth exceeded with file " ++ file))
    else
      include (bound - 1) (macs, sexprs) pos file
expandSExpr _ (macs, sexprs) sexpr =
    do                          -- Process all other S-expressions
      sexpr <- expandAll macs sexpr
      return (macs, sexpr : sexprs)

-- The include form is processed here.

include :: Int -> ([Macro], [SExpr Pos]) -> Pos -> String ->
           IO ([Macro], [SExpr Pos])
include bound (macs, sexprs) pos file =
    do
      input <- tryIO (openFile file ReadMode)
      case input of
        Left err -> fail (shows pos ("File " ++ file ++ ": " ++ err))
        Right input ->
          do
            p <- posHandle file input
            incsexprs <- readSExprs p
            foldM (expandSExpr bound) (macs, sexprs) incsexprs

readSExprs :: PosHandle -> IO [SExpr Pos]
readSExprs p =
    loop []
    where
      loop xs =
          do
            x <- readSExpr p
            case x of
              Nothing ->
                  return $ reverse xs
              Just x ->
                  loop (x:xs)

-- A macro definition is of the form:
--
-- (defmacro (NAME ARG*) BODY)
--
-- where NAME and each ARG is a symbol

data Macro = Macro
    { name :: String,
      args :: [String],
      body :: SExpr Pos }

getMacroName :: Macro -> String
getMacroName (Macro {name=n, args=_, body=_}) = n

getMacroArgs :: Macro -> [String]
getMacroArgs (Macro {name=_, args=a, body=_}) = a

getMacroBody :: Macro -> SExpr Pos
getMacroBody (Macro {name=_, args=_, body=b}) = b

defmacro :: MonadFail m => Pos -> [SExpr Pos] -> m Macro
defmacro _ [L _ (name : args), body] =
    do
      name <- symbol name
      args <- mapM symbol args
      return $ Macro { name = name,
                       args = args,
                       body = body}
defmacro pos _ = fail (shows pos "Malformed macro")

symbol :: MonadFail m => SExpr Pos -> m String
symbol (S _ string) = return string
symbol x = fail (shows (annotation x) "Malformed macro: Expecting a symbol")

-- Expand an S-expression using a given set of macros

expandAll :: MonadFail m => [Macro] -> SExpr Pos -> m (SExpr Pos)
expandAll macs sexpr =
    do
      sexpr <- macroExpand macs (annotation sexpr) limit sexpr
      case sexpr of
        L pos xs ->             -- Expand elements of list
            do
              xs <- mapM (expandAll macs) xs
              return (L pos (splice xs))
        _ -> return sexpr

-- Expand one S-expression limiting the number of expansions.

macroExpand :: MonadFail m => [Macro] -> Pos ->  Int ->
               SExpr Pos -> m (SExpr Pos)
macroExpand _ pos limit _
    | limit <= 0 = fail (shows pos "Expansion limit exceeded")
macroExpand macs pos limit sexpr@(L _ (S _ sym : xs)) =
    case macroExpand1 macs sym xs of
      Nothing -> return sexpr   -- Nothing to do
      Just sexpr -> macroExpand macs pos (limit - 1) sexpr
macroExpand _ _ _ sexpr = return sexpr

-- Expand one macro call or return Nothing

macroExpand1 :: [Macro] -> String -> [SExpr Pos] -> Maybe (SExpr Pos)
macroExpand1 [] _ _ = Nothing
macroExpand1 (mac : macs) sym xs
    | name mac == sym && length (args mac) == length xs =
        Just (apply mac xs)
    | otherwise =
        macroExpand1 macs sym xs

-- Apply a macro to some parameters

apply :: Macro -> [SExpr Pos] -> SExpr Pos
apply mac xs =
    subst (zip (args mac) xs) (body mac)

-- Substitute parameters into the macro body

subst :: [(String, SExpr Pos)] -> SExpr Pos -> SExpr Pos
subst env sexpr@(S _ sym) =
    maybe sexpr id (lookup sym env)
subst env (L pos sexprs) =
    L pos (map (subst env) sexprs)
subst _ sexpr = sexpr

-- Splice sexprs that start with splice symbol.  The splice symbol is ^.

splice :: [SExpr Pos] -> [SExpr Pos]
splice [] = []
splice ((L _ (S _ "^":xs)):ys) =
   loop xs ys
   where
     loop [] ys = splice ys
     loop (x:xs) ys = x:(loop xs ys)
splice (x:xs) = x:splice xs
