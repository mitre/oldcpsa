-- Arrays using zero-based indexing

-- Copyright (c) 2009 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module CPSA.Lib.Vector where

import Array

newtype Vector a = Vector (Array Int a)

-- Vector construction
vector :: [a] -> Vector a
vector vs = Vector (listArray (0, length vs - 1) vs)

-- The length of a vector
vlen :: Vector a -> Int
vlen (Vector a) = rangeSize (bounds a)

-- The value stored at a given index
vref :: Vector a -> Int -> a
vref (Vector a) i = a ! i

-- Update the vector with a new value at a given index
vmod :: Vector a -> Int -> a -> Vector a
vmod (Vector a) i v = Vector (a // [(i, v)])

-- The list of values in the vector.
vlist :: Vector a -> [a]
vlist (Vector a) = elems a

instance Eq a => Eq (Vector a) where
    Vector x == Vector y = x == y

instance Ord a => Ord (Vector a) where
    compare (Vector x) (Vector y) = compare x y

instance Show a => Show (Vector a) where
    showsPrec p (Vector a)
        = showParen (p > vecPrec)
          (showString "vector " .
           showsPrec (vecPrec + 1) (elems a))

instance Read a => Read (Vector a) where
    readsPrec p = readParen (p > vecPrec)
          (\r -> [ (vector vs, t) |
                   ("vector", s) <- lex r,
                   (vs, t)       <- readsPrec (vecPrec + 1) s])

-- Precedence of the 'vector' function is that of application itself
vecPrec :: Int
vecPrec = 10
