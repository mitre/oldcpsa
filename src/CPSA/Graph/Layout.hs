-- Produces node rankings for the Causally Intuitive Preskeleton Layout

-- Copyright (c) 2009 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module CPSA.Graph.Layout (Node, vnode, Rank, layout, maxRank) where

import qualified Data.List as L
import CPSA.Lib.CPSA (adjoin)
import CPSA.Graph.Loader

-- The node's rank determines its vertical position.
type Rank = Node -> Int

maxRank :: Preskel -> Rank -> Int
maxRank k rank =
    maximum (map (rank . vnode . lastVertex) (initial k))

-- Node rank computation

type Ranking = [(Node, Int)]

-- The default rank of a node is its position in the strand.
ranker :: Ranking -> Rank
ranker ranking n@(_, p) =
    case lookup n ranking of
      Nothing -> p
      Just rank -> rank

-- Layout algorithm described is described in the design document.
layout :: Preskel -> Rank
layout k =
    stretch [] (vertices k)
    where
      -- The node list is a todo list.
      stretch :: Ranking -> [Vertex] -> Rank
      stretch r [] = compress r (vertices k)
      stretch r (n1:ns) =
          if r1 < h then
              linearize ((vnode n1, h):r) ns h (next n1)
          else
              stretch r ns
          where
            r1 = ranker r (vnode n1)
            h = foldr (max . ranker r . vnode) r1 (preds n1)
      linearize :: Ranking -> [Vertex] -> Int -> Maybe Vertex -> Rank
      linearize r ns _ Nothing = stretch r ns
      linearize r ns r0 (Just n1) =
          if r1 <= r0 then
              linearize ((vnode n1, r0 + 1):r) ns' (r0 + 1) (next n1)
          else
              stretch r ns
          where
            r1 = ranker r (vnode n1)
            ns' = L.union (succs n1) ns
      compress :: Ranking -> [Vertex] -> Rank
      compress r [] = ranker r
      compress r (n1:ns) =
          case next n1 of
            Nothing ->  compress r ns
            Just n2 ->
                if r1 < h then
                    compress ((vnode n1, h):r) ns'
                else
                    compress r ns
                where
                  r1 = ranker r (vnode n1)
                  r2 = ranker r (vnode n2)
                  h = foldr (min . ranker r . vnode) (r2 - 1) (succs n1)
                  ns' = mbAdd (prev n1) (L.union (preds n1) ns)
                  mbAdd n ns = maybe ns (flip adjoin ns) n
