-- Generates a text view of CPSA output as a LaTeX document.

-- Copyright (c) 2009 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module CPSA.Graph.LaTeXView (latexView) where

import System.IO
import CPSA.Lib.CPSA (nats, SExpr, Pos)
import CPSA.Graph.Loader
import CPSA.Graph.Layout

type Printer a = Int -> Int -> SExpr a -> String

-- Main entry point
latexView :: Handle -> Int -> Printer Pos -> [SExpr Pos] ->
             Preskel -> State -> IO ()
latexView h margin pp cmts k s =
    do
      hPutStrLn h "\\usepackage[matrix,arrow,curve]{xy}"
      hPutStrLn h "\\begin{document}"
      hPutStrLn h "\\begin{verbatim}"
      mapM_ (writeItem h margin pp) cmts
      hPutStrLn h "\\end{verbatim}"
      writeLnPreskel h margin pp k s

-- Write each preskeleton
writeLnPreskel :: Handle -> Int -> Printer Pos -> Preskel -> State -> IO ()
writeLnPreskel h m pp k s =
    do
      case parent k of          -- Write protocol with first preskeleton
        Nothing ->
            do
              hPutStrLn h (showString "\n\\paragraph{Tree " $
                                      shows (label k) "}\n")
              hPutStrLn h "\n\\begin{verbatim}"
              writeItem h m pp (protSrc k)
              hPutStrLn h "\\end{verbatim}"
        Just _ -> return ()
      hPutStrLn h (showString "\n\\paragraph{Label " $ shows (label k) "}\n")
      hPutStrLn h (show (matrix k))
      hPutStrLn h "\n\\begin{verbatim}"
      writeItem h m pp (preskelSrc k)
      hPutStrLn h "\\end{verbatim}"
      n <- loadNext s
      case n of
        Nothing ->              -- EOF
            do
              hPutStrLn h "\n\\end{document}"
              hClose h
        Just (k, s) ->
            writeLnPreskel h m pp k s

-- XY-pic output

newtype Matrix = Matrix [Row]   -- Generates \xymatrix when shown

newtype Row = Row [Entry]       -- A row in the matrix

data Entry                      -- An entry in a row
    = Empty                     -- Leave entry blank.
    | Item { tag :: Maybe String, -- Maybe place text, else use bullet
             len :: Int,          -- Length of the strand succession arrow
                                  -- Communication arrows, bend arrows
             adj :: [(Int, Int, Bool, Bool)] } -- when second bool is True

-- Construct the matrix for a preskeleton
matrix :: Preskel -> Matrix
matrix k =
    Matrix (roles : [ row r | r <- nats (1 + maxRank k rank) ])
    where                       -- Column header is a role name
      roles = Row (map role (initial k))
      role v = Item { tag = Just ("\\txt{\\strut " ++ part (inst v) ++ "}"),
                      len = 0, adj = [] }
      row r = Row [ entry r c | c <- nats (strands k) ]
      entry r c =               -- Entry constructor
          case lookup (c, r) alist of
            Nothing -> Empty    -- Nothing at this grid point
            Just v -> item c r v -- Grid point has content
      item c r v =
          let len = maybe 0 (\v'->rank (vnode v') - r) (next v)
              adj = map (style c r v) (succs v) in
          Item { tag = Nothing, len = len, adj = map (curve c r) adj }
      style c r src dst =
          (msg src == msg dst, x, y)
          where
            (x, y) = shift (c, r) $ rankNode $ vnode dst
      curve c r (solid, x, y) = -- Decide if an arrows is curved
          (x, y, solid, abs x > 1 && bend)
          where
            bend = y /= 0 || any (obstruction c r) (between x)
      obstruction c r x =       -- Is there an obstruction here?
          maybe False (const True) (lookup (c + x, r) alist)
      between n =               -- List of ints toward zero
          case compare n 0 of
            LT -> [(n + 1)..(-1)]
            EQ -> []
            GT -> [1..(n - 1)]
      -- Map grid points with content to vertices
      alist = [ (rankNode (vnode v), v) | v <- vertices k ]
      rankNode (s, p) = (s, rank (s, p))
      rank = layout k     -- Get causally intuitive preskeleton layout

-- Shift origin of a pair
shift :: (Int, Int) -> (Int, Int) -> (Int, Int)
shift (x0, y0) (x1, y1) = (x1 - x0, y1 - y0)

-- A matrix is translated into LaTeX XY-pic macros using show methods

instance Show Matrix where
    showsPrec _ (Matrix []) =
        showString "\\relax"
    showsPrec _ (Matrix (r : rs)) =
        showString "$$\\xymatrix{\n" . shows r . rows rs . showString "}$$"
        where
          rows [] = id
          rows (r : rs) =
              showString "\\\\\n" . shows r . rows rs

instance Show Row where
    showsPrec _ (Row []) = id
    showsPrec _ (Row (e : es)) =
        shows e . entries es
        where
          entries [] = id
          entries (e : es) =
              showChar '&' . shows e . entries es

instance Show Entry where
    showsPrec _ Empty = id
    showsPrec _ (Item {tag = tag, len = len, adj = adj}) =
        element . succ . comm adj
        where
          element = showString (maybe "\\bullet" id tag)
          succ = if len > 0 then
                     showString "\\ar@{=>}[" . vert len . showChar ']'
                 else
                     id
          comm [] = id
          comm ((x, y, solid, bend) : cs) =
              showString ar . horiz x . vert y . showChar ']' . comm cs
              where
                curve = case () of
                        _ | not bend -> "["
                          | x > 0 -> "@/_/["
                          | otherwise -> "@/^/["
                style = if solid then "" else "@{-->}"
                ar = "\\ar" ++ style ++ curve

vert :: Int -> ShowS
vert n =
    case compare n 0 of
      LT -> showChar 'u' . vert (succ n)
      EQ -> id
      GT -> showChar 'd' . vert (pred n)

horiz :: Int -> ShowS
horiz n =
    case compare n 0 of
      LT -> showChar 'l' . horiz (succ n)
      EQ -> id
      GT -> showChar 'r' . horiz (pred n)

-- Writer
writeItem :: Handle -> Int -> Printer a -> SExpr a -> IO ()
writeItem h margin pp sexpr =
    hPutStrLn h (pp margin indent sexpr)

-- S-expression pretty print parameters
indent :: Int
indent = 2
