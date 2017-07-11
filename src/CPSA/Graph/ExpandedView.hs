-- Generates an expanded view of CPSA output as a compound document
-- that contains SVG within XHTML.

-- Copyright (c) 2009 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module CPSA.Graph.ExpandedView (expandedView, treelessView) where

import Data.List (intersperse)
import System.IO
import CPSA.Lib.CPSA
import CPSA.Graph.XMLOutput
import CPSA.Graph.Config
import CPSA.Graph.SVG
import CPSA.Graph.Loader
import CPSA.Graph.Preskeleton
import CPSA.Graph.Tree

type Printer a = Int -> Int -> SExpr a -> String

expandedView :: Handle -> Config -> Int -> [SExpr Pos] -> [Preskel] -> IO ()
expandedView h conf margin cmts ks =
    do
      hPutList h (header (scripts conf) cmts ks)
      let pp = printer conf
      comments h margin pp cmts
      case forest ks of
        [t] ->
            tdrawer h conf margin pp False t
        f ->
            do
              toc h f
              mapM_ (tdrawer h conf margin pp True) f
      hPutList h closer
      hClose h

header :: Bool -> [SExpr Pos] -> [Preskel] -> [String]
header addScript cmts ks =
    ["<html xmlns=\"http://www.w3.org/1999/xhtml\">",
     "<head>",
     " <title>" ++ title cmts ks ++ "</title>",
     " <meta http-equiv=\"content-type\"" ++
     " content=\"application/xhtml+xml; charset=UTF-8\" />",
     " <style>",
     "  svg.diagram { border-width: 1px; border-style: solid }",
     " </style>"]
    ++ maybeScript addScript ++
    ["</head>",
     "<body>"]

maybeScript :: Bool -> [String]
maybeScript False = []
maybeScript True =
    [ " <script>"]
    ++ javascript ++
    [" </script>"]

-- Find title in a herald form, but if absent use a protocol name.
title :: [SExpr Pos] -> [Preskel] -> String
title [] ks = protocolTitle ks
title ((L _ (S _ "herald" : S _ title : _)) : _) _ = title
title ((L _ (S _ "herald" : Q _ title : _)) : _) _ = title
title (_ : cmts) ks = title cmts ks

protocolTitle :: [Preskel] -> String
protocolTitle [] = "CPSA"
protocolTitle (k : _) = protocol k

comments :: Handle -> Int -> Printer Pos -> [SExpr Pos] -> IO ()
comments h margin pp cmts =
    do
      hPutStrLn h ""
      let xs = concat $ intersperse "\n" $ map (pp margin indent) cmts
      hPutStrLn h $ show $ mc "pre" [] xs

closer :: [String]
closer =
    ["", "</body>", "</html>"]

hPutList :: Handle -> [String] -> IO ()
hPutList h xs = mapM_ (hPutStrLn h) xs

-- Generates a list of trees within the document when there are more
-- than one.
toc :: Handle -> Forest -> IO ()
toc h f =
    do
      hPutStrLn h ""
      hPutStr h $ "<p id=\"" ++ topid ++ "\">Trees:"
      mapM_ (anchor h treeid . label . vertex) f
      hPutStrLn h ".</p>"

topid :: String
topid = "top"

anchor :: Handle -> (Int -> String) -> Int -> IO ()
anchor h id n =
    hPutStr h $ " <a href=\"#" ++ id n ++ "\">" ++ show n ++ "</a>"

-- Generates an SVG document root and puts it into a div element.
-- When scripting is enabled, it places all elements into a g element
-- that is used as the target of scaling actions.
docRoot :: Config -> Float -> Float -> [Element] -> Element
docRoot conf w h es =
    ec "div" [] div
    where
      attrs = [("class", "diagram"),
               ("width", showL w ++ units conf),
               ("height", showL h ++ units conf),
               ("xmlns", "http://www.w3.org/2000/svg"),
               ("version", "1.1"),
               ("viewBox", viewbox),
               ("font-size", showL (font conf))]
      viewbox = "0 0 " ++ showL w ++ " " ++ showL h
      svg = ec "svg" attrs (if scripts conf then [ec "g" [] es] else es)
      div = if scripts conf then [zoomControl, br, svg] else [svg]
      br = ec "br" [] []

zoomControl :: Element
zoomControl =
    ec "select" [("onchange", "zoom(event)")]
       [mc "option" [("value", "1.0")] "1.0",
        mc "option" [("value", "0.8")] "0.8",
        mc "option" [("value", "0.6")] "0.6",
        mc "option" [("value", "0.4")] "0.4",
        mc "option" [("value", "0.2")] "0.2"]

-- Draws one tree
tdrawer :: Handle -> Config -> Int -> Printer Pos -> Bool -> Tree -> IO ()
tdrawer h conf margin pp toc t =
    do
      hPutStrLn h ""
      let id = label (vertex t)
      hPutStr h $ "<p id=\"" ++ treeid id ++ "\">Tree"
      case toc of
        True -> anchor h (\_ -> topid) id
        False -> hPutStr h $ " " ++ show id
      hPutStrLn h ".</p>"
      hPutStrLn h ""
      let (width, height, es) = tree conf t
      hPutStrLn h $ show $ docRoot conf width height es
      hPutSExpr h margin pp (protSrc (vertex t))
      kdrawer h conf margin pp id [t]

treeid :: Int -> String
treeid label = tid (show label)

-- Draws the first item in the queue
kdrawer :: Handle -> Config -> Int -> Printer Pos -> Int -> [Tree] -> IO ()
kdrawer _ _ _ _ _ [] = return ()
kdrawer h conf margin pp tid (t:ts) =
    do
      hPutStrLn h ""
      let k = vertex t
      let id = label k
      hPutStr h $ "<p id=\"" ++ itemid id ++ "\">Item"
      anchor h (\_ -> treeid tid) id
      case parent k of
        Nothing -> return ()
        Just p ->
            do
              hPutStr h ", Parent:"
              anchor h itemid p
      titledList h "Child" "Children" $ map (label . vertex) (children t)
      titledList h "Seen Child" "Seen Children" $ seen k
      hPutStrLn h ".</p>"
      hPutStrLn h ""
      let (width, height, es) = skel conf k
      hPutStrLn h $ show $ docRoot conf width height (defs conf : es)
      hPutSExpr h margin pp (purgeTraces conf $ preskelSrc k)
      -- Use a breadth first ordering by appending children to the queue
      kdrawer h conf margin pp tid (ts ++ children t)

itemid :: Int -> String
itemid label = kid (show label)

-- Handle singular vs. plural.
titledList :: Handle -> String -> String -> [Int] -> IO ()
titledList _ _ _ [] =
    return ()
titledList h singular _ [id] =
    do
      hPutStr h $ ", " ++ singular ++ ":"
      anchor h itemid id
titledList h _ plural ls =
    do
      hPutStr h $ ", " ++ plural ++ ":"
      mapM_ (anchor h itemid) ls

hPutSExpr :: Handle -> Int -> Printer Pos -> SExpr Pos -> IO ()
hPutSExpr h margin pp sexpr =
    do
      hPutStrLn h ""
      hPutStrLn h $ show $ mc "pre" [] (pp margin indent sexpr)

-- S-expression pretty print parameters
indent :: Int
indent = 2

purgeTraces :: Config -> SExpr Pos -> SExpr Pos
purgeTraces conf x | purge conf =
  strip "traces" x
purgeTraces _ x = x

strip :: String -> SExpr Pos -> SExpr Pos
strip key (L p xs) =
  L p (filter f xs)
  where
    f (L _ (S _ s : _)) | s == key = False
    f _ = True
strip _ x = x

-- Encoded from src/zoom.js using src/js2hs
javascript :: [String]
javascript =
    ["<![CDATA[",
     "// Zoom support for use with cpsagraph and option --zoom",
     "",
     "// Find the g element within a near by svg element and apply a transform",
     "function zoom(evt) {",
     "    var g = evt.target.parentNode.lastElementChild.firstElementChild;",
     "    g.setAttribute(\"transform\", \"scale(\"+evt.target.value+\")\");",
     "}",
     "]]>"]

-- Treeless View -- fast because we don't generated derivation trees.

treelessView :: Handle -> Config -> Int -> [SExpr Pos] ->
                Preskel -> State -> IO ()
treelessView h conf margin cmts k s =
    do
      hPutList h (header (scripts conf) cmts [k])
      let pp = printer conf
      comments h margin pp cmts
      body h conf margin pp k s

body :: Handle -> Config -> Int -> Printer Pos -> Preskel -> State -> IO ()
body h conf margin pp k s =
    do
      case parent k of
        Nothing ->
            do
              hPutStrLn h ""
              hPutStrLn h $ show $ mc "p" [] ("Tree " ++ show (label k))
              hPutSExpr h margin pp (protSrc k)
        Just _ -> return ()
      hPutStrLn h ""
      hPutStrLn h $ show $ mc "p" [] ("Item " ++ show (label k))
      hPutStrLn h ""
      let (width, height, es) = skel conf k
      hPutStrLn h $ show $ docRoot conf width height (defs conf : es)
      hPutSExpr h margin pp (purgeTraces conf $ preskelSrc k)
      n <- loadNext s
      case n of
        Nothing ->              -- EOF
            do
              hPutList h closer
              hClose h
        Just (k, s) ->
            body h conf margin pp k s
