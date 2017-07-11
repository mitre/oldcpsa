-- Simple support for generating XML

-- This package assumes that the body of an element is either CDATA or
-- a sequence of elements.  An element is showable.

-- Copyright (c) 2009 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module CPSA.Graph.XMLOutput (Attribute, Element, mc, ec, props) where

import CPSA.Lib.CPSA

type Attribute = (String, String)

data Element
    = M String [Attribute] String    -- Mixed content (just CDATA)
    | E String [Attribute] [Element] -- Element content

-- Build an element that contains CDATA (mixed content)

mc :: String -> [Attribute] -> String -> Element
mc = M

-- Build an element that contains only elements (element content)

ec :: String -> [Attribute] -> [Element] -> Element
ec = E

instance Show Element where
    showsPrec _ element = pr 72 (pretty 1 element)

pretty :: Int -> Element -> Pretty
pretty indent (M name attrs "") = start indent name attrs [str "/>"]
pretty indent (E name attrs []) = start indent name attrs [str "/>"]
pretty indent (M name attrs body) =
    start indent name attrs (str ">" : str (quote body []) : close name)
pretty indent (E name attrs body) =
    start indent name attrs (str ">" : contents body)
    where
      contents [] = brk 0 : close name
      contents (e:es) = brk 0 : pretty indent e : contents es

start :: Int -> String -> [Attribute] -> [Pretty] -> Pretty
start indent name attrs rest =
    blo indent (str "<" : str name : attributes attrs rest)

attributes :: [Attribute] -> [Pretty] -> [Pretty]
attributes [] tail = tail
attributes (attr:attrs) tail =
    brk 1 : blo 0 (attribute attr (loop attrs)) : tail
    where                       -- Attributes can be
      loop [] = []              -- vertically aligned
      loop (attr:attrs) = brk 1 : attribute attr (loop attrs)

attribute :: Attribute -> [Pretty] -> [Pretty]
attribute (key, value) tail =
    str key : str "='" : str (quote value []) : str "'" : tail

close :: String -> [Pretty]
close name = [str "</", str name, str ">"]

-- Quote mixed content and attribute values
quote :: String -> ShowS
quote [] = id
quote ('<':cs) = showString "&lt;" . quote cs
quote ('>':cs) = showString "&gt;" . quote cs
quote ('&':cs) = showString "&amp;" . quote cs
quote ('"':cs) = showString "&quot;" . quote cs
quote ('\'':cs) = showString "&apos;" . quote cs
quote (c:cs) = showChar c . quote cs

-- Support for CSS properties

props :: [Attribute] -> String
props [] = ""
props (p:ps) =
    prop p (propl ps "")
    where
      propl [] = id
      propl (p:ps) = showChar ' ' . prop p . propl ps

prop :: Attribute -> ShowS
prop (key, value) =
    showString key . showString ": " .
    showString value . showChar ';'
