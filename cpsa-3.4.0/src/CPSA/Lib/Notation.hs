-- A CPSA specific pretty printer using infix notation.

-- Copyright (c) 2009 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module CPSA.Lib.Notation (printItem, printMsg, printEnv) where

import CPSA.Lib.Pretty
import CPSA.Lib.SExpr

-- Top-level pretty printer within specified margin.
printItem :: Int -> Int -> SExpr a -> String
printItem margin indent sexpr =
    pr margin (item indent sexpr) ""

-- Message pretty printer within specified margin.
printMsg :: Int -> Int -> SExpr a -> String
printMsg margin indent sexpr =
    pr margin (mesg indent sexpr) ""

-- Environment pretty printer within specified margin.
printEnv :: Int -> Int -> SExpr a -> String
printEnv margin indent sexpr =
    pr margin (env indent sexpr) ""
    where
      env indent (L _ xs) = blo indent $ str "(" : aft "," ")"
                           ( map (maplet indent) xs)
      env indent x = generic indent x

type Printer a = Int -> SExpr a -> Pretty

-- Print a top-level form
item :: Printer a
item indent (L _ (S _ "comment" : xs)) =
    blo indent $ str "comment " : aft "" ";" (map (generic indent) xs)
item indent (L _ (S _ "defprotocol" : S _ name : S _ alg : body)) =
    protocol indent name alg body
item indent (L _ (S _ "defskeleton" : S _ name :
                  L _ (S _ "vars" : decls) : body)) =
    skeleton indent name decls body
item indent x = generic indent x

-- Print a generic list.  Used when there is a syntax error.
generic :: Printer a
generic _ (S _ s) = str s
generic _ (Q _ s) = str (showQuoted s "")
generic _ (N _ n) = str (show n)
generic indent (L _ xs) =
    blo indent $ str "[" : aft "," "]" (map (generic indent) xs)

-- Add a separator and a final symbol to a list of Pretty's.
-- A break point is inserted after each separator.
aft :: String -> String -> [Pretty] -> [Pretty]
aft _ end [] = [str end]
aft _ end [p] = [p, str end]
aft sep end (p:ps) =
    p : str sep : brk 1 : aft sep end ps

-- Print a protocol
protocol :: Int -> String -> String -> [SExpr a] -> Pretty
protocol indent name alg body =
    grp indent $ header : brk 1 : aft "" "}" (map (alist indent) body)
    where
      header = str ("prot " ++ name ++ " " ++ alg ++ " {")

-- Print a skeleton
skeleton :: Int -> String -> [SExpr a] -> [SExpr a] -> Pretty
skeleton indent name decls body =
    grp indent $ header : brk 1 : aft "" "}" (map (alist indent) body)
    where
      start = str ("skel " ++ name ++ "(")
      header = blo (2 * indent) $ start : aft ";" ") {"
               (map (decl indent) decls)

-- A vars list is written with the sort first, followed by the variables.
-- Thus ((a b text) (k skey)) => (text a, b; skey k)
decl :: Int -> SExpr a -> Pretty
decl indent (L _ xs@(_:_:_)) =
    blo indent (aft "," ":" vars ++ [brk 1, sort])
    where
      (sort, vars) = split [] xs
      split vars [S _ sort] = (str sort, reverse vars)
      split vars (S _ var: xs) = split (str var : vars) xs
      split _ _ = error "[ASSERT FAILED] split: bad arg"
decl indent x = generic indent x

-- Association lists

-- The body of a protocol, role, and skeleton is an association list.
-- Each element of the list is a symbol followed by a list.

-- One function is used to process association lists even though a
-- role declaration may not occur in a skeleton or a role, and a
-- precedes declaration may not appear in a role or a protocol.

alist :: Printer a
alist _ (L _ [S _ key]) =
    str (key ++ ";")
alist indent (L _ (S _ "defrole" : S _ name :
                   L _ (S _ "vars" : decls) :
                   L a (S _ "trace" : trace) : body)) =
    role indent name decls (L a trace) body
alist indent (L _ (S _ "defstrand" : S _ name : N _ height : env)) =
    strand indent name height (map (maplet indent) env)
alist indent x@(L _ (S _ "operation" : _)) =
    operation indent x
alist indent x@(L _ (S _ key : _))
      | key == "annotations" || key == "obligations" =
          annotations indent x
alist indent (L _ (S _ key : xs))
      | key == "non-orig" || key == "uniq-orig" ||
        key == "deflistener" || key == "parameters" =
          values indent mesg key xs
      | key == "precedes" =
          values indent order key xs
      | key == "unrealized" =
          values indent node key xs
      | key == "traces" =
          grp indent $ str key : brk 1 : aft "," ";" (map (trace indent) xs)
      | otherwise =
          values indent generic key xs
alist indent x = generic indent x

-- Show the values associated with a key using the given printer.
values :: Int -> Printer a -> String -> [SExpr a] -> Pretty
values indent printer key values =
    blo indent $ str key : str " " : aft "," ";" (map (printer indent) values)

-- Print a role
role :: Int -> String -> [SExpr a] -> SExpr a -> [SExpr a] -> Pretty
role indent name decls tr body =
    grp indent $ header : brk 1 : aft "" "}" rest
    where
      start = str ("role " ++ name ++ "(")
      header = blo (2 * indent) $ start : aft ";" ") {"
               (map (decl indent) decls)
      rest = trace indent tr : map (alist indent) body

-- Print a trace
trace :: Printer a
trace indent (L _ xs) =
    blo 1 $ str "[" : aft "," "]" (map (event indent) xs)
trace indent x = generic indent x

-- Print an event
event :: Printer a
event indent (L _ [S _ "send", x]) = blo 0 [str "+", mesg indent x]
event indent (L _ [S _ "recv", x]) = blo 0 [str "-", mesg indent x]
event indent (L _ [S _ "sync", x]) = blo 0 [str "!", mesg indent x]
event indent x = generic indent x

-- Print a term in the Basic Crypto Algebra in compact form.

-- (enc a b ... z) => [a, b, ...]z
-- (cat a b ... z) => (a, b, ..., z)
-- (hash a b .. z) => #(a, b, ..., z)
-- (pubk a) => K(a)
-- (invk a) => I(a)
-- (privk a) => I(K(a))
-- (ltk a b) => S(a, b)
mesg :: Printer a
mesg indent (L _ (S _ "enc" : xs@(_:_:_))) =
    blo 0 [body, mesg indent key]
    where
      (key, plaintext) = rotate xs
      body = blo indent $ str "[" : aft "," "]" (map (mesg indent) plaintext)
mesg indent (L a (S b "privk" : xs)) =
    mesg indent (L a [S b "invk", L a (S b "pubk" : xs)])
mesg indent (L _ (S _ name : xs))
     | name == "cat" =
         fun indent "" args
     | name == "hash" =
         fun indent "#" args
     | name == "pubk" =
         fun indent "K" args
     | name == "invk" =
         fun indent "I" args
     | name == "ltk" =
         fun indent "S" args
     | otherwise =
         fun indent name args
     where
       args = map (mesg indent) xs
mesg indent (L _ xs@[N _ _, _]) =
    fun indent "" $ map (mesg indent) xs
mesg indent x = generic indent x

-- Print generic function application
fun :: Int -> String -> [Pretty] -> Pretty
fun indent name args =
    blo indent $ str name : str "(" : aft "," ")" args

-- Get last element of list and list without last element
rotate :: [a] -> (a, [a])
rotate (x:xs) =
    loop [] x xs
    where
      loop xs x [] = (x, reverse xs)
      loop ys y (x:xs) = loop (y:ys) x xs
rotate [] = error "[ASSERT FAILED] rotate: bad arg"

-- Print a node ordering
order :: Printer a
order indent (L _ [x, y]) =
    blo indent [node indent x, str " < ", node indent y]
order indent x = generic indent x

-- Print a node
node :: Printer a
node _ (L _ [N _ s, N _ i]) =
    str ("(" ++ show s ++ ", " ++ show i ++ ")")
node indent x = generic indent x

-- Print a maplet
maplet :: Printer a
maplet indent (L _ [x, y]) =
    blo indent [mesg indent x, str " -> ", mesg indent y]
maplet indent x = generic indent x

-- Print a strand
strand :: Int -> String -> Int -> [Pretty] -> Pretty
strand indent role height env =
    blo indent $ aft "," ");" (header:env)
    where
      header = str ("strand " ++ role ++ "(" ++ show height)

-- Print an operation
operation :: Printer a
operation indent (L _ (S _ "operation" : S _ direction : op :
                       critical : test : escape))
    | direction == "encryption-test" || direction == "nonce-test" =
      blo indent $ aft "" ";" body
      where
        body =  [str "operation", str direction, solve indent op,
                 mesg indent critical, node indent test, set]
        set = blo 1 $ str "{" : aft "," "}" (map (mesg indent) escape)
operation indent (L _ (S _ "operation" : S _ "generalization" : desc)) =
    blo indent $ aft "" ";" [str "operation", str "generalization",
                             generalization indent desc]
operation indent (L _ (S _ "operation" : xs)) =
    blo indent $ aft "" ";" $ str "operation" : (map (generic indent) xs)
operation indent x = generic indent x

-- Print authentication test solving operation
solve :: Printer a
solve indent (L _ (S _ "contracted" : xs)) =
    blo indent $ str "contracted(" : aft "," ")" (map (maplet indent) xs)
solve indent (L _ (S _ "displaced" : xs)) =
    blo indent $ str "displaced(" : aft "," ")" (map (generic indent) xs)
solve indent (L _ (S _ "added-strand" : xs)) =
    blo indent $ str "added-strand(" : aft "," ")" (map (generic indent) xs)
solve indent (L _ [S _ "added-listener", t]) =
    blo indent $ [str "added-listener(", mesg indent t, str ")"]
solve indent x = generic indent x

-- Print generalization method
generalization :: Int -> [SExpr a] -> Pretty
generalization indent [S _ "deleted", n] =
    blo indent [str "deleted ", node indent n]
generalization indent [S _ "weakened", o] =
    blo indent [str "weakened ", order indent o]
generalization indent [S _ "separated", t] =
    blo indent [str "separated ", mesg indent t]
generalization indent [S _ "forgot", t] =
    blo indent [str "forgot ", mesg indent t]
generalization indent xs =
    blo indent $ aft "" "" (map (generic indent) xs)

-- Annotations

-- Print an annotation
annotations :: Printer a
annotations indent (L _ (S _ key :
                         (xs@(L _ (L _ [N _ _, N _ _] : _) : _)))) =
    grp indent $ str key : brk 1 : aft "," ";" (map (obligation indent) xs)
annotations indent (L _ (S _ key : term : formulas)) =
    blo indent $ str key : brk 1 : mesg indent term :
        str "," : brk 1 : aft "," ";" (map (numform indent) formulas)
annotations indent x = generic indent x

numform :: Printer a
numform indent (L _ [n@(N _ _), f]) =
    blo indent [str "(", mesg indent n, str ",", brk 1,
                form indent f, str ")"]
numform indent x = generic indent x

obligation :: Printer a
obligation indent (L _ [n, term, formula]) =
    blo indent [str "[", node indent n, str ",", brk 1, mesg indent term,
                str ",", brk 1, form indent formula, str "]"]
obligation indent x = generic indent x

-- Print a formula
form :: Printer a
form indent (L _ [S _ "implies", h, c]) =
    blo indent [mesg indent h, brk 1, str "implies", brk 1, mesg indent c]
form indent (L _ (S _ "implies" : xs@(_:_:_:_))) =
    grp 0 [lhs, brk 1, str "implies", brk 1, mesg indent concl]
    where
      lhs = grp indent $ str "and(" : brk 0 :
            aft "," ")" (map (mesg indent) hypoth)
      (concl, hypoth) = rotate xs
form indent x =
    mesg indent x
