-- Translate JSON encoded CPSA into CPSA S-Expressions

-- Copyright (c) 2014 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module Main (main) where

import Numeric
import Data.Char (isSpace, isDigit, isPrint)
import System.IO
import System.Environment
import System.Console.GetOpt
import CPSA.Lib.CPSA
import CPSA.Lib.Entry

-- Runtime parameters

data Params = Params
    { file :: Maybe FilePath,   -- Nothing specifies standard output
      prefix :: Bool,           -- Use prefix notation?
      margin :: Int }           -- Output line length
    deriving Show

indent :: Int
indent = optIndent defaultOptions

main :: IO ()
main =
    do
      (p, params) <- jStart options interp
      h <- outputHandle $ file params
      let printer = if prefix params then pp else printItem
      go (writeCpsaLn (printer (margin params) indent) h) p
      hClose h

writeCpsaLn :: (SExpr a -> String) -> Handle -> SExpr a -> IO ()
writeCpsaLn printer h sexpr =
    do
      hPutStrLn h $ printer sexpr
      hPutStrLn h ""

go :: (SExpr () -> IO ()) -> Handle -> IO ()
go f p =
    loop
    where
      loop =
          do
            x <- jRead p
            case x of
              Nothing ->
                  return ()
              Just sexpr ->
                  do
                    f sexpr
                    loop

-- Command line option flags
data Flag
    = Help                      -- Help
    | Info                      -- Version information
    | Margin String             -- Output line length
    | Infix                     -- Select output notation
    | Output String             -- Output file name
      deriving Show

defaultMargin :: Int
defaultMargin = optMargin defaultOptions

options :: [OptDescr Flag]
options =
    [ Option ['o'] ["output"]   (ReqArg Output "FILE") "output FILE",
      Option ['m'] ["margin"]   (ReqArg Margin "INT")
      ("set output margin (default " ++ show defaultMargin ++ ")"),
      Option ['i'] ["infix"]    (NoArg Infix)  "output uses infix notation",
      Option ['h'] ["help"]     (NoArg Help)           "show help message",
      Option ['v'] ["version"]  (NoArg Info)           "show version number" ]

-- Interpret option flags
interp :: [Flag] -> IO Params
interp flags =
    loop flags (Params { file = Nothing, -- By default, no output file
                         prefix = True,
                         margin = defaultMargin })
    where
      loop [] params = return params
      loop (Output name : flags) params
          | file params == Nothing =
              loop flags $ params { file = Just name }
      loop (Infix : flags) params =
          loop flags $ params { prefix = False }
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

-- Returns the input S-expression and an interpretation of the command
-- line options.
jStart :: [OptDescr a] -> ([a] -> IO b) -> IO (Handle, b)
jStart options interp =
    do
      argv <- getArgs
      (flags, files) <- opts options argv
      opts <- interp flags
      p <- openInput options files
      return (p, opts)

opts :: [OptDescr a] -> [String] -> IO ([a], [String])
opts options argv =
    case getOpt RequireOrder options argv of
      (o, n, []) -> return (o, n)
      (_, _, errs) ->
          do
            msg <- usage options errs
            abort msg

openInput ::  [OptDescr a] -> [String] -> IO Handle
openInput _ [file] =
    openFile file ReadMode      -- Input from named file
openInput _ [] =
    return stdin                -- Input from the standard input
openInput options _ =
    do
      msg <- usage options ["too many input files\n"]
      abort msg

-- Read a JSON expression, and fail on error
jRead :: Handle -> IO (Maybe (SExpr ()))
jRead p =
    do
      x <- tryIO (jLoad p)
      case x of
        Right x ->
            return x
        Left err ->
            abort err

-- The result of scanning is a token.
data Token
    = Atom (SExpr ())
    | Lparen
    | Rparen
    | Eof

-- Top level parser
jLoad :: Handle -> IO (Maybe (SExpr ()))
jLoad h =
    do
      t <- scan h
      case t of
        Atom x ->
            return $ Just x
        Lparen ->
            do
              x <- list h []
              return $ Just x
        Rparen ->
            abort "Close of unopened list"
        Eof ->
            do
              hClose h
              return Nothing

-- A recursive decent parser
list :: Handle -> [SExpr ()] -> IO (SExpr ())
list h xs =
    do
      t <- scan h
      case t of
        Rparen ->
            return (L () (seqrev xs))
        Atom x ->
            list h (x : xs)
        Lparen ->
            do
              x <- list h []
              list h (x : xs)
        Eof ->
            abort "Unexpected end of input in list"

-- Read the next character returning Nothing on EOF
get :: Handle -> IO (Maybe Char)
get h =
    do
      eof <- hIsEOF h
      case eof of
        True ->
            return Nothing
        False ->
            do
              ch <- hGetChar h
              return $ Just ch

-- Peek at the next character returning Nothing on EOF
peek :: Handle -> IO (Maybe Char)
peek h =
    do
      eof <- hIsEOF h
      case eof of
        True ->
            return Nothing
        False ->
            do
              ch <- hLookAhead h
              return $ Just ch

-- Return the next token
scan :: Handle -> IO (Token)
scan h =
    do
      ch <- get h
      case ch of
        Nothing ->
            return Eof
        Just ch ->
            skip h ch

-- Skip whitespace and then handle first character of a token
skip :: Handle -> Char -> IO (Token)
skip h ',' = scan h             -- Treat comma as white space
skip h ch | isSpace ch  = scan h
skip _ '[' =
    return Lparen
skip _ ']' =
    return Rparen
skip h ch =
    atom h ch

-- Scan a string, number, or a symbol
atom :: Handle -> Char -> IO (Token)
atom h '"' = string h []
atom h '+' = sign h True
atom h '-' = sign h False
atom h ch | isDigit ch = number h [ch]
atom _ _ = abort "Bad char in atom"

-- Scan a quoted string of characters
string :: Handle -> String -> IO (Token)
string h s =
    do
      ch <- get h
      case ch of
        Nothing ->
            abort "End of input in string"
        Just '"' ->
            symOrStr s
        Just '\\' ->
            quote h s
        Just ch | isPrint ch ->
            string h (ch : s)
        Just _ ->
            abort "Bad char for string"

-- Handle backslash in string
quote :: Handle -> String -> IO (Token)
quote h s =
    do
      ch <- get h
      case ch of
        Nothing ->
            abort "End of input in string"
        Just ch | isPrint ch ->
            string h (ch : s)
        Just _ ->
            abort "Bad char for string"

-- Is string a symbol or a quoted string?
symOrStr :: String -> IO (Token)
symOrStr ('"' : s) =
  case seqrev s of
    '"' : str ->
      return $ Atom (Q () str)
    _ ->
      abort "Quote in symbol"
symOrStr s =
  return $ Atom (S () $ seqrev s)

-- A reverse that evaluates the list elements.
seqrev :: [a] -> [a]
seqrev l =
    foldl (\xs x -> x `seq` xs `seq` (x:xs)) [] l

-- Scan a sequence of digits
number :: Handle -> String -> IO (Token)
number h s =
    do
      ch <- peek h
      case ch of
        Nothing ->
            return (Atom (N () (read (seqrev s))))
        Just ch | isDigit ch ->
            do
              _ <- hGetChar h
              number h (ch : s)
        Just _ ->
            return (Atom (N () (read (seqrev s))))

-- Scan a number that starts with a sign
sign :: Handle -> Bool -> IO (Token)
sign h plus =
    do
      ch <- get h
      case ch of
        Nothing ->
            abort "Sign followed by EOF"
        Just ch | isDigit ch ->
            let s = if plus then [ch] else [ch, '-'] in
            number h s
        Just _ ->
            abort "Sign not followed by a digit"
