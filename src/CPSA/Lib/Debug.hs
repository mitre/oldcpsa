-- Debugging functions

-- Copyright (c) 2013 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module CPSA.Lib.Debug where

import System.IO.Unsafe

z :: Show a => a -> b -> b
z x y = unsafePerformIO (print x >> return y)

zln :: Show a => a -> b -> b
zln x y = unsafePerformIO (putStrLn (show x) >> return y)

zz :: Show a => a -> a
zz x = z x x

zzz :: (Show a, Show b) => a -> b -> b
zzz x y = zfun (\y -> (x,y)) y

zcond :: Show a => Bool -> a -> b -> b
zcond True a b = z a b
zcond _ _ b = b

zzcond :: Show a => (a -> Bool) -> a -> a
zzcond p x
  | p x = zz x
  | otherwise = x

zfcond :: Show c => (b -> Bool) -> (b -> c) -> b -> b
zfcond p f x = zcond (p x) (f x) x

zmaybe :: Show a => Maybe a -> b -> b
zmaybe (Just x) y = z x y
zmaybe _ y = y

zlist :: Show a => [a] -> b -> b
zlist [] b = b
zlist (x:xs) b = (zln x (zlist xs b))

zb :: Show a => a -> Bool -> Bool
zb a False = z a False
zb _ b = b

zn :: Show a => a -> Maybe b -> Maybe b
zn x Nothing = z x Nothing
zn _ y = y

zf :: Show a => a -> Bool -> Bool
zf x False = z x False
zf _ y = y

zt :: Show a => a -> Bool -> Bool
zt x True = z x True
zt _ y = y

zl :: Show a => [a] -> [a]
zl a = z (length a) a

zfun :: (Show b) => (a -> b) -> a -> a
zfun f x = z (f x) x

zlfun :: (Show b) => (a -> [b]) -> a -> a
zlfun f x = zlist (f x) x
