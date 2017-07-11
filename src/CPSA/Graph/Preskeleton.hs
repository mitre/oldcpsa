-- Draws preskeletons

-- Copyright (c) 2009 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module CPSA.Graph.Preskeleton (skel) where

import CPSA.Lib.CPSA (SExpr, printMsg, printEnv)
import CPSA.Graph.XMLOutput
import CPSA.Graph.Config
import CPSA.Graph.SVG
import CPSA.Graph.Loader
import CPSA.Graph.Layout
import qualified Data.List as L

showMsg :: Config -> SExpr a -> String
showMsg conf x =
    case notation conf of
      Prefix -> show x
      Infix -> printMsg margin indent x

showEnv :: Config -> SExpr a -> String
showEnv conf x =
    case notation conf of
      Prefix -> show x
      Infix -> printEnv margin indent x

margin :: Int
margin = 1024

indent :: Int
indent = 0

-- Compute a node's position.
npos :: Config -> Rank -> Node -> (Float, Float)
npos conf rank (s, p) =
    (xpos, ypos)
    where
      -- Node x coordinate is determined by its strand
      xpos = mx conf + dx conf * fromIntegral s
      -- Node y coordinate is determined by its rank
      ypos = my conf + dy conf * fromIntegral (rank (s, p) + 1)

-- Dimension of the diagram
dim :: Config -> Int -> Int -> (Float, Float)
dim conf strands maxRank =
    (w, h)
    where
      w = 2 * mx conf + dx conf * fromIntegral (strands - 1)
      h = 2 * my conf + dy conf * fromIntegral (maxRank + 1)

-- Add a cross strand arrow
addEdge :: Config -> Rank -> Vertex -> Float -> Float ->
           String -> [Element] -> Vertex -> [Element]
addEdge conf rank src x1 y1 color elements n =
    let (x2, y2) = npos conf rank (vnode n) in
    arrow conf (sameMsg src n) x1 y1 x2 y2 color : elements

sameMsg :: Vertex -> Vertex -> Bool
sameMsg src dest =
  case (dir src, dir dest) of
    (OutDir, InDir) -> msg src == msg dest
    (SyncDir, SyncDir) -> True
    _ -> False

-- Add a strand node
addNode :: Config -> Preskel -> Rank -> [Element] -> Vertex -> [Element]
addNode conf k rank elements node =
    let (x, y) = npos conf rank (vnode node)
        bullet = circ conf (nodeColor k node) x y
        es = tooltip (showMsg conf $ msg node) [bullet] : elements
        es' = foldl (addEdge conf rank node x y "black") es (succs node L.\\ leadstosuccs node)
        es'' = foldl (addEdge conf rank node x y "blue") es' (leadstosuccs node) in
    maybe es'' (addNode conf k rank es'') (next node)

nodeColor :: Preskel -> Vertex -> Maybe String
nodeColor k node =
  case dir node of
    OutDir -> Nothing           -- Transmission nodes are black
    SyncDir
      | elem node (maybe [] id (unrealized k)) -> Just "orange"
      | otherwise -> Just "gray" -- Realized nodes are gray
    InDir
      | elem node (maybe [] id (unrealized k)) -> Just "red"
      | otherwise -> Just "blue" -- Realized nodes are blue

-- Add role above a strand
addRole :: Config -> [Element] -> Vertex -> [Element]
addRole conf es node =
    if null r then
        es
    else
        tooltip (showEnv conf e) [text conf x y r] : es
    where
      r = role node
      e = env node
      (x, y0) = npos conf snd (strand node, 0)
      (_, y1) = npos conf snd (strand node, (-1))
      y = (y0 + y1) / 2

-- Add strand lines
addStrand :: Config -> Rank -> [Element] -> Vertex -> [Element]
addStrand conf rank es n =
    line conf x1 y1 x2 y2 : es
    where
      (x1, y1) = npos conf rank (vnode n)
      (x2, y2) = npos conf rank (vnode (lastVertex n))

-- Add title of diagram
title :: Config -> Preskel -> [Element]
title conf k =
    foldl (addRole conf) [header] (initial k)
    where
      (w, _) = dim conf (strands k) 0
      (_, y) = npos conf snd (0, (-1))
      content = protocol k ++ " " ++ show (label k)
      header = text conf (w / 2) y (content ++ annotation)
      annotation =
          case unrealized k of
            Nothing -> ""
            Just [] -> " (realized)"
            _ -> ""

-- Draw a preskeleton
skel :: Config -> Preskel -> (Float, Float, [Element])
skel conf k =
    (w, h, lines)
    where
      (w, h) = dim conf (strands k) (maxRank k rank)
      circs = foldl (addNode conf k rank) (title conf k) (initial k)
      lines = foldl (addStrand conf rank) circs (initial k)
      rank = layout k
