-- Generates a compact view of CPSA S-expressions as an SVG image

-- Copyright (c) 2009 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module CPSA.Graph.CompactView (compactView) where

import System.IO
import CPSA.Graph.XMLOutput
import CPSA.Graph.Config
import CPSA.Graph.SVG
import CPSA.Graph.Loader
import CPSA.Graph.Tree
import CPSA.Graph.Preskeleton

compactView :: Handle -> Config -> [Preskel] -> IO ()
compactView h config preskels =
    do
      let (width, height, es) = draw config preskels
      hPutStrLn h $ show $ docRoot config width height es
      hClose h
      return ()

-- Generates the SVG document root.
docRoot :: Config -> Float -> Float -> [Element] -> Element
docRoot conf w h es =
    ec "svg" attrs es
    where
      attrs = [("width", showL w ++ units conf),
               ("height", showL h ++ units conf),
               ("xmlns", "http://www.w3.org/2000/svg"),
               ("version", "1.1"),
               ("viewBox", viewbox),
               ("font-size", showL (font conf)),
               ("onload", "init(evt)")]
      viewbox = "0 0 " ++ showL w ++ " " ++ showL h

draw :: Config -> [Preskel] -> (Float, Float, [Element])
draw conf ks =
    case forest ks of
      [t] ->
          (w, h, [script, defs conf, e])
          where
            (w, h, e) = tdraw conf 0 0 t
      ts ->
          fdraw conf ts

-- Draw a forest
fdraw :: Config -> Forest -> (Float, Float, [Element])
fdraw conf ts =
    (w + fw, max h fh, script : defs conf : rect conf 0 0 w h : es)
    where
      w = 2 * mx conf
      h = 2 * my conf + fromIntegral (length ts - 1) * ty conf
      (fw, fh, es) = foldl f (0, 0, index conf ts) ts
      f (fw, fh, es) t =
          (max fw tw, max fh th, e : es)
          where
            (tw, th, e) = tdraw conf w 0 t

-- Draw an index that gives access to trees in the forest
index :: Config -> Forest -> [Element]
index conf (t:ts) =
    snd $ foldl loop (y, top) ts
    where
      x = mx conf
      y = my conf
      top = [tbutton conf x y (label (vertex t))]
      loop (y1, es) t =
          (y2, es')
          where
            es' = tbutton conf x y2 (label (vertex t)) : es
            y2 = y1 + ty conf
index _ _ = error "Tree: Empty forest found"

-- Draw one tree in the forest
tdraw :: Config -> Float -> Float -> Tree -> (Float, Float, Element)
tdraw conf x y t =
    (w, h, ec "svg" attrs (rect conf 0 0 tw th : contents))
    where
      attrs = [("id", tid (show (label (vertex t)))),
               ("x", showL x), ("y", showL y),
               ("width", showL w), ("height", showL h),
               ("visibility", "hidden")]
      w = tw + kw               -- Total width
      h = max th kh             -- Total height
      (tw, th, es) = tree conf t
      (kw, kh, contents) = body conf tw 0 (0, 0, es) t

-- Extract the preskeletons within one tree
body :: Config -> Float -> Float -> (Float, Float, [Element]) ->
        Tree -> (Float, Float, [Element])
body conf x y (w, h, es) t =
    foldl (body conf x y) (max w w', max h h', e:es) (children t)
    where
      (w', h', e) = kdraw conf x y (vertex t)

-- Draw a preskeleton
kdraw :: Config -> Float -> Float -> Preskel -> (Float, Float, Element)
kdraw conf x y k =
    (w, h, ec "svg" attrs (rect conf 0 0 w h : lines))
    where
      attrs = [("id", kid (show (label k))), ("x", showL x),
               ("y", showL y), ("width", showL w), ("height", showL h)] ++
              if compact conf then [("visibility", "hidden")] else []
      (w, h, lines) = skel conf k
