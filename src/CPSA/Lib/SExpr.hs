{-|
Module:      CPSA.Lib.SExpr
Description: S-expressions and a reader
Copyright:   (c) 2009 The MITRE Corporation
License:     BSD

This module provides a data structure for S-expressions, and a reader.
The reader records the position in the file at which items that make
up the list are located.

The S-expressions used are restricted so that most dialects of Lisp
can read them, and characters within symbols and strings never need
quoting. Every list is proper. An atom is either a symbol, an integer,
or a string. The characters that make up a symbol are the letters, the
digits, and these special characters.

@
    +-*/<=>!?:$%_&~^
@

A symbol may not begin with a digit or a sign followed by a digit. The
characters that make up a string are the printing characters omitting
double quote and backslash, except when double quote and backslash are
escaped using the backslash character. Double quotes delimit a
string. A comment begins with a semicolon and continues to the end of
the current line.

-}

-- Copyright (c) 2009 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module CPSA.Lib.SExpr (SExpr(..), showQuoted, stringSExpr, annotation,
                       -- * S-expression Reader
                       Pos, PosHandle, posHandle, load) where

import Data.Char (isSpace, isDigit, isAlphaNum, isPrint)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO (Handle, hIsEOF, hGetChar, hLookAhead, hClose)

-- | An S-expression--all of its constructors are strict.
data SExpr a
    = S !a !String                 -- ^ A symbol
    | Q !a !String                 -- ^ A quoted string
    | N !a !Int                    -- ^ An integer
    | L !a ![SExpr a]              -- ^ A proper list

-- | Equality ignores position annotations.
instance Eq (SExpr a) where
    S _ s == S _ s' = s == s'
    Q _ s == Q _ s' = s == s'
    N _ n == N _ n' = n == n'
    L _ xs == L _ xs' = xs == xs'
    _ == _ = False

-- | Ordering ignores position annotations.
instance Ord (SExpr a) where
    compare (S _ s) (S _ s') = compare s s'
    compare (S _ _) (Q _ _) = LT
    compare (S _ _) (N _ _) = LT
    compare (S _ _) (L _ _) = LT
    compare (Q _ _) (S _ _) = GT
    compare (Q _ s) (Q _ s') = compare s s'
    compare (Q _ _) (N _ _) = LT
    compare (Q _ _) (L _ _) = LT
    compare (N _ _) (S _ _) = GT
    compare (N _ _) (Q _ _) = GT
    compare (N _ n) (N _ n') = compare n n'
    compare (N _ _) (L _ _) = LT
    compare (L _ _) (S _ _) = GT
    compare (L _ _) (Q _ _) = GT
    compare (L _ _) (N _ _) = GT
    compare (L _ xs) (L _ xs') = compare xs xs'

-- | This printer produces no line breaks.
instance Show (SExpr a) where
    showsPrec _ (S _ s) = showString s
    showsPrec _ (Q _ s) = showQuoted s
    showsPrec _ (N _ n) = shows n
    showsPrec _ (L _ []) = showString "()"
    showsPrec _ (L _ (x:xs)) =
        showChar '(' . shows x . showl xs . showChar ')'
        where
          showl [] = id
          showl (x:xs) = showChar ' ' . shows x . showl xs

-- | Add quotes to a string so it reads as an S-expression string.
showQuoted :: String -> ShowS
showQuoted s = showChar '"' . showEscaped s . showChar '"'

showEscaped :: String -> ShowS
showEscaped ('\\' : s) = showString "\\\\" . showEscaped s
showEscaped ('"' : s) = showString "\\\"" . showEscaped s
showEscaped (ch : s) = showChar ch . showEscaped s
showEscaped [] = id

-- | Convert a raw string into a quoted S-expression.
stringSExpr :: String -> SExpr ()
stringSExpr s =
  if all isPrint s then
    Q () s
  else
    error "SExpr.stringSExpr: Bad string"

-- | Extract an S-expression's annotation.
annotation :: SExpr a -> a
annotation (S a _) = a
annotation (Q a _) = a
annotation (N a _) = a
annotation (L a _) = a

-- S-expression Reader

-- | The reader returns objects of type 'SExpr' 'Pos' so that error
-- messages can include a location.
data Pos = Pos { file :: !String, line :: !Int, column :: !Int }

-- | Show a position in a form Emacs can read.
instance Show Pos where
    showsPrec _ pos = showString (file pos) .
                      showString ":" .
                      shows (line pos) .
                      showString ":" .
                      shows (column pos) .
                      showString ": "

-- | Keep track of position information associated with a given handle.
data PosHandle = PosHandle { pHandle :: Handle, pFile :: String,
                             pPosition :: IORef (Int, Int) }
-- | Create a 'PosHandle'.
posHandle :: FilePath -> Handle -> IO PosHandle
posHandle file handle =
    do
      pos <- newIORef (1, 1)    -- Set initial position in a file
      return $ PosHandle { pHandle = handle, pFile = file, pPosition = pos }

setPosHandle :: PosHandle -> Int -> Int -> IO ()
setPosHandle ph line column =
    writeIORef (pPosition ph) (line, column)

-- The result of scanning is a token.
data Token
    = Atom !(SExpr Pos)
    | Lparen !Pos
    | Rparen !Pos
    | Eof

-- | Read one S-expression or return 'Nothing' on EOF
load :: PosHandle -> IO (Maybe (SExpr Pos))
load p =
    do
      (l, c) <- readIORef $ pPosition p
      (l, c, t) <- scan p l c
      case t of
        Atom x ->
            do
              setPosHandle p l c
              return $ Just x
        Lparen pos ->
            do
              (l, c, x) <- list p pos l c []
              setPosHandle p l c
              return $ Just x
        Rparen pos ->
            abort p (shows pos "Close of unopened list")
        Eof ->
            do
              hClose $ pHandle p
              return Nothing

-- A recursive decent parser
list :: PosHandle -> Pos -> Int -> Int -> [SExpr Pos] ->
        IO (Int, Int, SExpr Pos)
list p pos l c xs =
    do
      (l, c, t) <- scan p l c
      case t of
        Rparen _ ->
            return (l, c, L pos (seqrev xs))
        Atom x ->
            list p pos l c (x : xs)
        Lparen pos' ->
            do
              (l, c, x) <- list p pos' l c []
              list p pos l c (x : xs)
        Eof ->
            abort p (shows pos "Unexpected end of input in list")

-- Read the next character returning Nothing on EOF
get :: PosHandle -> IO (Maybe Char)
get p =
    do
      let h = pHandle p
      eof <- hIsEOF h
      case eof of
        True ->
            return Nothing
        False ->
            do
              ch <- hGetChar h
              return $ Just ch

-- Peek at the next character returning Nothing on EOF
peek :: PosHandle -> IO (Maybe Char)
peek p =
    do
      let h = pHandle p
      eof <- hIsEOF h
      case eof of
        True ->
            return Nothing
        False ->
            do
              ch <- hLookAhead h
              return $ Just ch

-- Return the next token and update line and column information
scan :: PosHandle -> Int -> Int -> IO (Int, Int, Token)
scan p l c =
    do
      ch <- get p
      case ch of
        Nothing ->
            return (l, c, Eof)
        Just ch ->
            skip p l c ch

-- Skip whitespace and then handle first character of a token
skip :: PosHandle -> Int -> Int -> Char -> IO (Int, Int, Token)
skip p l _ '\n' = scan p (l + 1) 1
skip p l c ch | isSpace ch  = scan p l (c + 1)
skip p l c ';' = comment p l (c + 1)
skip p l c '(' =
    return (l, c + 1, Lparen $ pos p l c)
skip p l c ')' =
    return (l, c + 1, Rparen $ pos p l c)
skip p l c ch =
    atom p l (c + 1) (pos p l c) ch

pos :: PosHandle -> Int -> Int -> Pos
pos p l c =
    Pos { file = pFile p, line = l, column = c }

-- Consume a comment
comment :: PosHandle -> Int -> Int -> IO (Int, Int, Token)
comment p l c =
    do
      ch <- get p
      case ch of
        Nothing ->
            return (l, c, Eof)
        Just '\n' ->
            scan p (l + 1) 1
        Just _ ->
            comment p l (c + 1)

-- Scan a string, number, or a symbol
atom :: PosHandle -> Int -> Int -> Pos -> Char -> IO (Int, Int, Token)
atom p l c pos '"' = string p l c pos []
atom p l c pos ch | isDigit ch = number p l c pos [ch]
atom p l c pos ch | ch == '+' || ch == '-' = numOrSym p l c pos [ch]
atom p l c pos ch | isSym ch = symbol p l c pos [ch]
atom p _ _ pos _ = abort p (shows pos "Bad char in atom")

-- Scan a quoted string of characters
string :: PosHandle -> Int -> Int -> Pos -> String -> IO (Int, Int, Token)
string p l c pos s =
    do
      ch <- get p
      case ch of
        Nothing ->
            abort p (shows pos "End of input in string")
        Just '"' ->
            return (l, c + 1, Atom (Q pos (seqrev s)))
        Just '\\' ->
            escaped p l (c + 1) pos s
        Just ch | isPrint ch ->
            string p l (c + 1) pos (ch : s)
        Just _ ->
            abort p (shows pos "Bad char in string")

-- Scan an escaped character in a quoted string of characters
escaped :: PosHandle -> Int -> Int -> Pos -> String -> IO (Int, Int, Token)
escaped p l c pos s =
    do
      ch <- get p
      case ch of
        Nothing ->
            abort p (shows pos "End of input in escaped char in string")
        Just '"' ->
            string p l (c + 1) pos ('"' : s)
        Just '\\' ->
            string p l (c + 1) pos ('\\' : s)
        Just _ ->
            abort p (shows pos "Bad escaped char in string")

-- Scan a sequence of digits
number :: PosHandle -> Int -> Int -> Pos -> String -> IO (Int, Int, Token)
number p l c pos s =
    do
      ch <- peek p
      case ch of
        Nothing ->
            return (l, c, Atom (N pos (read (seqrev s))))
        Just ch | isDigit ch ->
            do
              _ <- hGetChar $ pHandle p
              number p l (c + 1) pos (ch : s)
        Just ch | isSym ch ->
            abort p (shows pos "Bad char after number")
        Just _ ->
            return (l, c, Atom (N pos (read (seqrev s))))

-- Scan a number that starts with a sign or a symbol
numOrSym :: PosHandle -> Int -> Int -> Pos -> String -> IO (Int, Int, Token)
numOrSym p l c pos s =
    do
      ch <- peek p
      case ch of
        Nothing ->
            symbol p l c pos s
        Just ch | isDigit ch ->
            if s == "+" then
              number p l c pos []
            else
              number p l c pos s
        Just _ ->
            symbol p l c pos s

-- Scan a symbol
symbol :: PosHandle -> Int -> Int -> Pos -> String -> IO (Int, Int, Token)
symbol p l c pos s =
    do
      ch <- peek p
      case ch of
        Nothing ->
            return (l, c, Atom (S pos (seqrev s)))
        Just ch | isSym ch ->
            do
              _ <- hGetChar $ pHandle p
              symbol p l (c + 1) pos (ch : s)
        Just _ ->
            return (l, c, Atom (S pos (seqrev s)))

-- A reverse that evaluates the list elements.
seqrev :: [a] -> [a]
seqrev l =
    foldl (\xs x -> x `seq` xs `seq` (x:xs)) [] l

-- A symbol is made from alphanumeric characters or special
-- characters.  A symbol may not start with a digit, or with a plus or
-- minus sign followed by a digit.  The special characters are
-- "+-*/<=>!?:$%_&~^".

isSym :: Char -> Bool
isSym '+' = True
isSym '-' = True
isSym '*' = True
isSym '/' = True
isSym '<' = True
isSym '=' = True
isSym '>' = True
isSym '!' = True
isSym '?' = True
isSym ':' = True
isSym '$' = True
isSym '%' = True
isSym '_' = True
isSym '&' = True
isSym '~' = True
isSym '^' = True
isSym c = isAlphaNum c

-- Close input handle and then report failure
abort :: PosHandle -> String -> IO a
abort p msg =
    do
      hClose $ pHandle p
      fail msg
