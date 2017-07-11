-- A CPSA specific pretty printer using S-expressions.

-- Copyright (c) 2009 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module CPSA.Lib.Printer (pp) where

import CPSA.Lib.Pretty
import CPSA.Lib.SExpr

-- Pretty printer within specified margin.
pp :: Int -> Int -> SExpr a -> String
pp margin indent sexpr =
    pr margin (pretty indent sexpr) ""

type Printer a = Int -> SExpr a -> Pretty

-- A pretty printer that indents a constant amount for each list.  The
-- top-level lists are laid out specially.  Whenever some breaks
-- occur, all breaks are forced.  Also, breaks are only placed before
-- strings and lists.  CPSA protocols are handled specially.  Each
-- defrole is handled as are top-level lists.
pretty :: Printer a
pretty indent (L _ (x@(S _ "defprotocol") : xs)) =
    loop [block indent x, str "("] xs
    where
      loop es [] = grp indent (reverse (str ")" : es))
      loop es (x@(S _ _):xs) = loop (block indent x : str " " : es) xs
      loop es (x@(Q _ _):xs) = loop (block indent x : brk 1 : es) xs
      loop es (x@(N _ _):xs) = loop (block indent x : str " " : es) xs
      loop es (x@(L _ (S _ "defrole": _)):xs) =
          loop (group indent x : brk 1 : es) xs
      loop es (x@(L _ _):xs) = loop (block indent x : brk 1 : es) xs
pretty indent x@(L _ (S _ "defmacro" : _)) = block indent x
pretty indent x@(L _ (S _ "herald" : _)) = block indent x
pretty indent x = group indent x

group :: Printer a
group indent (L _ (x:xs)) =
    loop [block indent x, str "("] xs
    where
      loop es [] = grp indent (reverse (str ")" : es))
      loop es (x@(S _ _):xs) = loop (block indent x : str " " : es) xs
      loop es (x@(Q _ _):xs) = loop (block indent x : brk 1 : es) xs
      loop es (x@(N _ _):xs) = loop (block indent x : str " " : es) xs
      loop es (x@(L _ _):xs) = loop (block indent x : brk 1 : es) xs
group indent x = block indent x

-- A pretty printer for interior lists using block style breaking.
block :: Printer a
block _ (S _ s) = str s
block _ (Q _ s) = str (showQuoted s "")
block _ (N _ n) = str (show n)
block _ (L _ []) = str "()"
block indent (L _ (x:xs)) =
    loop [block indent x, str "("] xs
    where
      loop es [] = blo indent (reverse (str ")" : es))
      loop es (x:xs) = loop (block indent x : brk 1 : es) xs
