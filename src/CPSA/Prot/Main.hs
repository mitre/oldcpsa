-- Translate protocols in Alice and Bob notation into defprotocol syntax

-- Copyright (c) 2020 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

{-
This program translates a protocol expressed in Alice and Bob notation
into defprotocol syntax.  It translates defprot forms, and passes all
else through unchanged.  The syntax of a defprot form is:

PROT       ::= (defprot NAME ALGEBRA (vars DECLS) NOTATION* RULE* PROT-ALIST)
NOTATION   ::= (msg ROLE ROLE CHMSG)
            |  (msg ROLE ROLE CHMSG CHMSG)
            |  (from ROLE CHMSG)
            |  (to ROLE CHMSG)
            |  (clone ROLE ROLE)
            |  (assume ROLE ASSUMPTION*)
CHMSG      ::= (chmsg CHAN TERM) | (chmsg TERM) | TERM
ASSUMPTION ::= (uniq-orig ...) | (non-orig ...) | ...
RULE       ::= (defrule ...)

The form (msg A B M) says role A sends M, and role B receives M.
The form (msg A B M N) says role A sends M, and role B receives N.
The form (from A M) says role A sends M.
The form (to A M) says role A receives M.
The form (clone A B) says make role B have the same messages as A up
to this point in the specification.
-}

module Main (main) where

import Numeric
import System.IO
import System.Console.GetOpt
import Control.Monad (foldM)
import CPSA.Lib.SExpr
import CPSA.Lib.Printer (pp)
import CPSA.Lib.Entry
import CPSA.Lib.Expand (readSExprs, expand)

-- Runtime parameters

data Params = Params
    { file :: Maybe FilePath,   -- Nothing specifies standard output
      margin :: Int,            -- Output line length
      doExpand :: Bool}         -- Expand macros first?
    deriving Show

defaultIndent :: Int
defaultIndent = optIndent defaultOptions

defaultMargin :: Int
defaultMargin = optMargin defaultOptions

main :: IO ()
main =
    do
      (p, params) <- start options interp
      sexprs <- readSExprs p
      -- Could expand macros here
      sexprs <- tryAbort (maybeExpand (doExpand params) sexprs)
      sexprs <- tryAbort (mapM translate sexprs)
      h <- outputHandle $ file params
      mapM_ (display (margin params) h) sexprs
      hClose h

maybeExpand :: Bool -> [SExpr Pos] -> IO [SExpr Pos]
maybeExpand False sexprs = return sexprs
maybeExpand True sexprs = expand sexprs

tryAbort :: IO a -> IO a
tryAbort x =
  do
    a <- tryIO x
    case a of
      Left err -> abort err
      Right a -> return a

display :: Int -> Handle -> SExpr a -> IO ()
display margin h x =
  do
    hPutStrLn h $ pp margin defaultIndent x
    hPutStrLn h ""

-- Command line option flags
data Flag
    = Help                      -- Help
    | Info                      -- Version information
    | Margin String             -- Output line length
    | Output String             -- Output file name
    | Expand                    -- Expand macros
      deriving Show

options :: [OptDescr Flag]
options =
    [ Option ['o'] ["output"]   (ReqArg Output "FILE") "output FILE",
      Option ['m'] ["margin"]   (ReqArg Margin "INT")
      ("set output margin (default " ++ show defaultMargin ++ ")"),
      Option ['e'] ["expand"]   (NoArg Expand)         "expand macros first",
      Option ['h'] ["help"]     (NoArg Help)           "show help message",
      Option ['v'] ["version"]  (NoArg Info)           "show version number"]

-- Interpret option flags
interp :: [Flag] -> IO Params
interp flags =
    loop flags (Params { file = Nothing, -- By default, no output file
                         margin = defaultMargin, doExpand = False})
    where
      loop [] params = return params
      loop (Expand : flags) params =
              loop flags $ params { doExpand = True }
      loop (Output name : flags) params
          | file params == Nothing =
              loop flags $ params { file = Just name }
      loop (Margin value : flags) params =
          case readDec value of
            [(margin, "")] ->
                loop flags $ params { margin = margin }
            _ ->
                do
                  msg <- usage options ["Bad value for margin\n"]
                  abort msg
      loop (Info : _) _ =
          success cpsaVersion
      loop (Help : _) _ =
          do                    -- Show help then exit with success
            msg <- usage options []
            success msg
      loop _ _ =
           do                   -- Show help then exit with failure
             msg <- usage options ["Bad option combination\n"]
             abort msg

-- Translate one S-expression
translate :: MonadFail m => SExpr Pos -> m (SExpr ())
translate (L pos (S _ "defprot" : xs)) =
  prot pos xs
translate x = return $ strip x

strip :: SExpr a -> SExpr ()
strip (S _ s) = S () s
strip (Q _ s) = Q () s
strip (N _ n) = N () n
strip (L _ l) = L () (map strip l)

prot :: MonadFail m => Pos -> [SExpr Pos] -> m (SExpr ())
prot _ (S _ name : S _ alg : L _ (S _ "vars" : vs) : body) =
  do
    let vars = L () (S () "vars" : map strip vs)
    rs <- roles vars body
    return
      (L () (S () "defprotocol" : S () name :
             S () alg : rs ++ filter scrub (map strip body)))
prot p _ = fail (shows p "Malformed defprot")

roles :: MonadFail m => SExpr () -> [SExpr Pos] -> m [SExpr ()]
roles vars body =
  do
    env <- foldM msg [] body
    assumes <- foldM assume [] body
    return $ map (makeRole vars assumes) env

makeRole :: SExpr () -> Env -> (String, [SExpr ()]) -> SExpr ()
makeRole vars assumes (name, trace) =
  L () (S () "defrole" : S () name : vars :
        L () (S () "trace" : reverse trace) :
        case lookup name assumes of
          Nothing -> []
          Just xs -> reverse xs)

type Env = [(String, [SExpr ()])]

msg :: MonadFail m => Env -> SExpr Pos -> m Env
msg env (L _ [S _ "msg", S _ from, S _ to, term]) =
  return (update to (L () (S () "recv" : chmsg term))
          (update from (L () (S () "send" : chmsg term)) env))
msg env (L _ [S _ "msg", S _ from, S _ to, fromTerm, toTerm]) =
  return (update to (L () (S () "recv" : chmsg toTerm))
          (update from (L () (S () "send" : chmsg fromTerm)) env))
msg _ (L p (S _ "msg" : _)) =
  fail (shows p "Malformed msg")
msg env (L _ [S _ "from", S _ from, term]) =
  return (update from (L () (S () "send" : chmsg term)) env)
msg _ (L p (S _ "from" : _)) =
  fail (shows p "Malformed from")
msg env (L _ [S _ "init", S _ from, term]) =
  return (update from (L () (S () "init" : chmsg term)) env)
msg _ (L p (S _ "init" : _)) =
  fail (shows p "Malformed init")
msg env (L _ [S _ "obsv", S _ from, term]) =
  return (update from (L () (S () "obsv" : chmsg term)) env)
msg _ (L p (S _ "obsv" : _)) =
  fail (shows p "Malformed obsv")
msg env (L _ [S _ "tran", S _ from, term1, term2]) =
  return (update from (L () (S () "tran" : [strip term1, strip term2])) env)
msg _ (L p (S _ "tran" : _)) =
  fail (shows p "Malformed tran")
msg env (L _ [S _ "to", S _ to, term]) =
  return (update to (L () (S () "recv" : chmsg term)) env)
msg _ (L p (S _ "to" : _)) =
  fail (shows p "Malformed to")
msg env (L _ [S _ "clone", S p1 from, S p2 to]) =
  case lookup to env of
    Just _ -> fail (shows p2 ("Role " ++ to ++ " already defined"))
    Nothing ->
      case lookup from env of
        Nothing -> fail (shows p1 ("Role " ++ from ++ " not defined yet"))
        Just trace -> return $ (to, trace) : env
msg _ (L p (S _ "clone" : _)) =
  fail (shows p "Malformed clone")
msg env _ = return env

chmsg :: SExpr a -> [SExpr ()]
chmsg (L _ [S _ "chmsg", S _ chan, term]) =
  [S () chan, strip term]
chmsg (L _ [S _ "chmsg", term]) = [strip term]
chmsg term = [strip term]

update :: String -> SExpr () -> Env -> Env
update role msg [] = [(role, [msg])]
update role msg ((name, trace) : rest)
  | role == name = (name, msg : trace) : rest
  | otherwise = (name, trace) : update role msg rest

assume :: MonadFail m => Env -> SExpr Pos -> m Env
assume env (L _ (S _ "assume" : S _ role : xs)) =
  return $ augment role (map strip xs) env
assume _ (L p (S _ "assume" : _)) =
  fail (shows p "Malformed assume")
assume env _ = return env

augment :: String -> [SExpr ()] -> Env -> Env
augment role assumes [] = [(role, reverse assumes)]
augment role assumes ((name, a) : rest)
  | role == name = (name, reverse assumes ++ a) : rest
  | otherwise = (name, a) : augment role assumes rest

scrub :: SExpr () -> Bool
scrub (L () (S () "msg" : _)) = False
scrub (L () (S () "from" : _)) = False
scrub (L () (S () "to" : _)) = False
scrub (L () (S () "clone" : _)) = False
scrub (L () (S () "assume" : _)) = False
scrub (L () (S () "init" : _)) = False
scrub (L () (S () "obsv" : _)) = False
scrub (L () (S () "tran" : _)) = False
scrub _ = True
