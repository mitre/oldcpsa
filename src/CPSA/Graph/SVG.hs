-- Exports SVG primitives.

-- Copyright (c) 2009 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module CPSA.Graph.SVG where

import qualified Numeric as N
import CPSA.Graph.XMLOutput
import CPSA.Graph.Config

-- Show a coordinate as a length

showsL :: Float -> ShowS
showsL len =
    N.showFFloat (Just 3) len

showL :: Float -> String
showL len = showsL len []

-- Shortens a line from (x1, y1) to (x2, y2) by d.
shorten :: Float -> Float -> Float -> Float -> Float -> (Float, Float)
shorten d x1 y1 x2 y2 =
    (x3 + x1, y3 + y1)
    where
      dx, dy, fd :: Float
      dx = x2 - x1
      dy = y2 - y1
      fd = 1.0 - d / sqrt (dx * dx + dy * dy)
      x3 = fd * dx
      y3 = fd * dy

-- Tooltip
tooltip :: String -> [Element] -> Element
tooltip tip content =
    ec "g" [] (mc "title" [] tip:content)

-- Centered text
text :: Config -> Float -> Float -> String -> Element
text _ x y content =
    mc "text" [("x", showL x), ("y", showL y), ("style", style)] content
    where
      style = props [("text-anchor", "middle")]

-- Circle with radius from configuration
circ :: Config -> Maybe String -> Float -> Float -> Element
circ conf color x y =
    ec "circle" (style ++ dims) []
    where
      dims = [("cx", showL x), ("cy", showL y), ("r", showL (br conf))]
      style =
          case color of
            Nothing -> []
            Just color -> [("style", props [("fill", color)])]

rect :: Config -> Float -> Float -> Float -> Float -> Element
rect conf x y w h =
    ec "rect" [("x", showL x), ("y", showL y),
               ("width", showL w), ("height", showL h),
               ("style", props style)] []
    where
      style = [("fill", "none"), ("stroke", "black"),
               ("stroke-width", showL (stroke conf))]

-- Gray line
line :: Config -> Float -> Float -> Float -> Float -> Element
line conf x1 y1 x2 y2 =
    ec "line" [("x1", showL x1), ("y1", showL y1),
               ("x2", showL x2), ("y2", showL y2),
               ("style", props style)] []
    where
      style = [("stroke-width", showL (stroke conf)), ("stroke", "gray")]

-- An arrow that is curved when its horizontal length exceeds a limit.
arrow :: Config -> Bool -> Float -> Float -> Float -> Float -> String -> Element
arrow conf solid x1 y1 x3 y3 color =
    ec "path" [("d", path), ("style", props style)] []
    where
      (x2, y2) = shorten (br conf) x1 y1 x3 y3
      path = showString "M " $
             showsL x1 $ showChar ' ' $ showsL y1 $ showString " Q " $
             showsL xq $ showChar ' ' $ showsL yq $ showChar ' ' $
             showsL x2 $ showChar ' ' $ showL y2
      xq = (x1 + x2) / 2
      yq = (y1 + y2 - delta) / 2
      delta = if abs (x1 - x2) > dx conf then dy conf else 0
      style = if solid then solidStyle else dashedStyle
      solidStyle =
          [("stroke-width", showL (stroke conf)),
           ("stroke", color),
           ("marker-end", "url(#arrow)"),
           ("fill", "none")]
      dashedStyle = ("stroke-dasharray",
                     showsL (dash conf) $ showString "," $ showL (gap conf))
                    : solidStyle

data ButtonKind
    = AliveTree                 -- tree node is alive
    | Shape                     -- tree node is alive and is a shape
    | AliveDup                  -- duplicate node is alive
    | DeadTree                  -- tree node is dead
    | DeadDup                   -- duplication node is dead
    | Realized                  -- realized but not a shape

-- A button that displays a preskeleton
kbutton :: Config -> Float -> Float -> ButtonKind -> Int -> Element
kbutton conf x y kind label =
    mc "text" attrs (show label)
    where
      attrs = [("x", showL x), ("y", showL y), ("style", props style),
               ("onclick", onclick conf label)]
      style = italic kind [("text-anchor", "middle"), ("fill", color kind)]
      color AliveTree = "black"
      color Shape = "blue"
      color AliveDup = "green"
      color DeadTree = "red"
      color DeadDup = "orange"
      color Realized = "blue"
      italic AliveTree props = props
      italic Shape props = ("font-weight", "bold"):props
      italic AliveDup props = ("font-style", "italic"):props
      italic DeadTree props = props
      italic DeadDup props = ("font-style", "italic"):props
      italic Realized props = props

onclick :: Config -> Int -> String
onclick conf label =
    if compact conf then
        showString "showk(evt, \"" $ kid $ shows label "\")"
    else
        showString "window.open(\"#" $ kid $ shows label "\", \"_self\")"

-- A button that displays a tree and its first preskeleton
tbutton :: Config -> Float -> Float -> Int -> Element
tbutton _ x y label =
    mc "text" attrs (content [])
    where
      attrs = [("x", showL x), ("y", showL y),
               ("style", style), ("onclick", script)]
      style = props [("text-anchor", "middle")]
      script = showString "showt(evt, \"" $ tid $ content $
               showString "\", \"" $ kid $ content "\")"
      content = shows label

-- Tree identifier
tid :: ShowS
tid = showChar 't'

-- Skeleton identifier
kid :: ShowS
kid = showChar 'k'

-- The script for the buttons
ecmascript :: String
ecmascript =
    "\nvar t;\n\
    \var k;\n\n\
    \function init(evt) {\n\
    \  t = evt.target.ownerDocument.getElementById(\"t0\");\n\
    \  t.setAttribute(\"visibility\", \"visible\");\n\
    \  k = evt.target.ownerDocument.getElementById(\"k0\");\n\
    \  k.setAttribute(\"visibility\", \"visible\");\n\
    \}\n\n\
    \function showk(evt, kid) {\n\
    \  k.setAttribute(\"visibility\", \"hidden\");\n\
    \  k = evt.target.ownerDocument.getElementById(kid);\n\
    \  k.setAttribute(\"visibility\", \"visible\");\n\
    \}\n\n\
    \function showt(evt, tid, kid) {\n\
    \  showk(evt, kid);\n\
    \  t.setAttribute(\"visibility\", \"hidden\");\n\
    \  t = evt.target.ownerDocument.getElementById(tid);\n\
    \  t.setAttribute(\"visibility\", \"visible\");\n\
    \}\n"

script :: Element
script = mc "script" [("type", "application/ecmascript")] ecmascript

-- The marker for arrow heads
defs :: Config -> Element
defs _ =
    ec "defs" [] [marker]
    where
      marker =
          ec "marker"
                 [("id", "arrow"), ("orient", "auto"),
                  ("markerWidth", "5"), ("markerHeight", "10"),
                  ("refX", "5"), ("refY", "5")] [path]
      path = ec "path" [("d", "M 0 0 5 5 0 10"), ("style", props style)] []
      style = [("stroke-width", "2"), ("fill", "none"), ("stroke", "black")]
