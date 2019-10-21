-- Filters the intermediate skeletons out of a CPSA run.

-- Copyright (c) 2009 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

{-# LANGUAGE CPP #-}

#if !(MIN_VERSION_base(4,13,0))
#define MonadFail Monad
#endif

module CPSA.Shapes.Shapes (Map, empty, shape) where

import qualified Data.IntMap as I
import Data.IntMap (IntMap)
import CPSA.Lib.SExpr

-- An int map is used to bind a skeleton to its parent.

type Map = IntMap Int

empty :: Map
empty = I.empty

-- Omit S-expressions for skeletons that are not the point-of-view
-- skeleton or a shape of a point-of-view skeleton.  The parent of a
-- skeleton is changed to reflect omitted skeletons, and its seen
-- children are deleted.

shape :: MonadFail m => Map -> SExpr Pos -> m (Map, Maybe (SExpr Pos))
shape map x@(L _ (S _ "defskeleton" : _ : _ : strands)) =
    do
      label <- nassoc "label" xs
      checkLabel label
    where
      xs = findAList strands   -- Association list part of preskeleton
      checkLabel Nothing =
          return (map, Just x) -- Not labeled, just add it to the answers
      checkLabel (Just label) =
          do                    -- Found a label
            parent <- nassoc "parent" xs
            checkParent label parent
      checkParent label Nothing = -- Point of view preskeleton
          return (I.singleton label label, Just $ reparent label x)
      checkParent label (Just parent) = -- Descendent of POV
          lookupParent label $ I.lookup parent map
      lookupParent _ Nothing =
          fail "parent unknown"
      lookupParent label (Just ancestor) =
          -- Ancestor is the label of POV preskeleton or of a shape
          let shape = maybe False (const True) (assoc "shape" xs) in
          if shape then
              return (I.insert label label map,
                      Just $ reparent ancestor x)
          else                  -- Drop S-expression here
              return (I.insert label ancestor map, Nothing)
shape map x = return (map, Just x)

findAList :: [SExpr Pos] -> [SExpr Pos]
findAList (L _ (S _ "defstrand" : _) : xs) = findAList xs
findAList (L _ (S _ "deflistener" : _) : xs) = findAList xs
findAList xs = xs

-- Lookup value in alist, appending values with the same key
assoc :: String -> [SExpr Pos] -> Maybe [SExpr Pos]
assoc key alist =
    loop alist Nothing
    where
      loop ((L _ (S _ head : tail)) : rest) vals
          | key == head = loop rest (extend tail vals)
          | otherwise = loop rest vals
      loop _ vals = vals
      extend x Nothing = Just x
      extend x (Just y) = Just (x ++ y)

-- Look up a value known to be an Int
nassoc :: MonadFail m => String -> [SExpr Pos] -> m (Maybe Int)
nassoc key xs =
    case assoc key xs of
      Nothing -> return Nothing
      Just [val] ->
          do
            ns <- num val
            return (Just ns)
      Just (x:_) -> fail (shows (annotation x) "Expecting one number")
      Just [] -> fail (shows (annotation (head xs)) "Expecting one number")

num :: MonadFail m => SExpr Pos -> m Int
num (N _ n) = return n
num x = fail (shows (annotation x) "Expecting a number")

reparent :: Int -> SExpr Pos -> SExpr Pos
reparent n (L p0 (S p1 "defskeleton" : protocol : vars : strands)) =
    L p0 (S p1 "defskeleton" : protocol : vars : updateStrands n strands)
reparent _ x = x

updateStrands :: Int -> [SExpr Pos] -> [SExpr Pos]
updateStrands n (L p0 (S p1 "defstrand" : x) : xs) =
    L p0 (S p1 "defstrand" : x) : updateStrands n xs
updateStrands n (L p0 (S p1 "deflistener" : x) : xs) =
    L p0 (S p1 "deflistener" : x) : updateStrands n xs
updateStrands n xs = updateParent n xs

updateParent :: Int -> [SExpr Pos] -> [SExpr Pos]
updateParent n (L p0 [tag@(S _ "parent"), N p1 _] : xs) =
    L p0 [tag, N p1 n] : updateParent n xs
-- Handle the case of seen in the original problem, and dump it.
updateParent n (L _ (S _ "seen" : _) : xs) = updateParent n xs
updateParent n (x : xs) = x : updateParent n xs
updateParent _ [] = []
